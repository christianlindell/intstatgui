# Tabellkatalog och ämnesträd
#
# Eurostats innehållsförteckning (TOC) är en lista där varje rad har en nivå
# (hierarchy) och en typ: folder, dataset eller table. En table ligger ofta under
# ett dataset; här hamnar båda direkt i den närmaste mappen. Samma tabell kan
# stå på flera ställen i trädet.

# En cachefil per datakälla och språk
es_cache_file <- function(lang = "en", source = "eurostat") {
  nyckel <- if (source == "eurostat") lang else paste0(source, "_", lang)
  custom <- getOption("intstatgui.cache_file")
  if (!is.null(custom)) return(sub("(\\.rds)?$", paste0("_", nyckel, ".rds"), custom))
  dir <- tools::R_user_dir("intstatgui", which = "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, paste0("katalog_", nyckel, ".rds"))
}

# Höjs när katalogens innehåll ändras, så att gamla cachefiler inte används
es_catalogue_version <- 2L

# Hämtar katalogen. Använder cachen om den är yngre än max_age_hours.
es_fetch_catalogue <- function(backend = es_backend(source), lang = "en", force = FALSE,
                               max_age_hours = 24, source = "eurostat",
                               cache_file = es_cache_file(lang, source)) {
  if (!force && file.exists(cache_file)) {
    age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
    if (age < max_age_hours) {
      cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      if (identical(cached$version, es_catalogue_version)) return(cached)
    }
  }
  catalogue <- (backend$parse %||% es_parse_toc)(backend$toc(lang))
  catalogue$version <- es_catalogue_version
  catalogue$fetched <- Sys.time()
  catalogue$lang <- lang
  tryCatch(saveRDS(catalogue, cache_file), error = function(e) NULL)
  catalogue
}

# Gör om TOC till tre tabeller:
#   tables:  en rad per tabell
#   folders: en rad per mapp, med föräldern
#   items:   en rad per plats där en tabell står (tabell-id och mapp)
es_parse_toc <- function(toc) {
  toc <- as.data.frame(toc, stringsAsFactors = FALSE)
  rensa <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | x == ""] <- NA_character_
    x
  }
  n <- nrow(toc)
  # Eurostat har mappar utan namn, med bara --- eller ... som titel. De är inga egna nivåer i
  # trädet: det de innehåller hamnar i mappen ovanför.
  ar_mapp <- toc$type == "folder" & grepl("[[:alnum:]]", toc$title)
  ar_tabell <- toc$type != "folder"
  niva <- as.integer(toc$hierarchy)

  # Mapparna på vägen från roten till raden. Nyckeln innehåller radnumret, eftersom
  # samma mappkod kan stå på flera ställen, även på samma nivå: båda rotmapparna heter
  # data i Eurostats lista.
  stack <- character()
  stack_niva <- integer()
  key <- character(n)
  parent <- character(n)
  for (i in seq_len(n)) {
    kvar <- stack_niva < niva[i]
    stack <- stack[kvar]
    stack_niva <- stack_niva[kvar]
    parent[i] <- if (length(stack) > 0) stack[length(stack)] else es_root_key
    if (ar_mapp[i]) {
      key[i] <- paste0(toc$code[i], "_", i)
      stack <- c(stack, key[i])
      stack_niva <- c(stack_niva, niva[i])
    }
  }

  tables <- data.frame(
    id = as.character(toc$code[ar_tabell]),
    label = as.character(toc$title[ar_tabell]),
    type = as.character(toc$type[ar_tabell]),
    first_period = rensa(toc$data.start[ar_tabell]),
    last_period = rensa(toc$data.end[ar_tabell]),
    updated = rensa(toc$last.update.of.data[ar_tabell]),
    n_obs = as.numeric(toc$values[ar_tabell]),
    stringsAsFactors = FALSE
  )
  tables <- tables[!duplicated(tables$id), , drop = FALSE]
  rownames(tables) <- NULL

  list(
    tables = tables,
    folders = data.frame(key = key[ar_mapp], label = as.character(toc$title[ar_mapp]),
                         parent = parent[ar_mapp], stringsAsFactors = FALSE),
    items = unique(data.frame(table_id = as.character(toc$code[ar_tabell]),
                              key = parent[ar_tabell], stringsAsFactors = FALSE))
  )
}

# Filtrerar tabellerna på fritext (alla ord måste finnas i id eller namn) och typ
es_filter_tables <- function(tables, text = "", only_datasets = FALSE) {
  keep <- rep(TRUE, nrow(tables))
  if (only_datasets) keep <- keep & tables$type == "dataset"
  words <- strsplit(tolower(trimws(text %||% "")), "\\s+")[[1]]
  words <- words[nzchar(words)]
  if (length(words) > 0) {
    hay <- tolower(paste(tables$id, tables$label))
    for (w in words) keep <- keep & grepl(w, hay, fixed = TRUE)
  }
  tables[keep, , drop = FALSE]
}

# Nyckel för trädets rot
es_root_key <- "<rot>"

# Bygger trädet som en nästlad lista. Varje nod har key, label, children, tables
# (tabell-id:n direkt i noden) och n (antal tabeller i noden och alla undernoder).
# Mappar utan tabeller tas bort. Ordningen är TOC:s.
es_build_tree <- function(catalogue, table_ids = NULL) {
  items <- catalogue$items
  if (!is.null(table_ids)) items <- items[items$table_id %in% table_ids, , drop = FALSE]
  tables_by_key <- split(items$table_id, items$key)
  children_by_parent <- split(catalogue$folders$key, catalogue$folders$parent)
  label_by_key <- stats::setNames(catalogue$folders$label, catalogue$folders$key)

  make_node <- function(key) {
    kids <- lapply(children_by_parent[[key]] %||% character(), make_node)
    kids <- Filter(function(k) k$n > 0, kids)
    tabs <- unique(tables_by_key[[key]] %||% character())
    all_ids <- unique(c(tabs, unlist(lapply(kids, `[[`, "all_ids"))))
    list(key = key, label = if (key == es_root_key) "" else label_by_key[[key]],
         children = kids, tables = tabs, all_ids = all_ids, n = length(all_ids))
  }
  make_node(es_root_key)
}

# Antal mappar och tabeller, för statusraden
es_tree_stats <- function(tree) {
  count_folders <- function(node) {
    length(node$children) + sum(vapply(node$children, count_folders, 0))
  }
  list(subjects = length(tree$children), folders = count_folders(tree), tables = tree$n)
}

# Ritar trädet som nästlade <details>-element. Hela katalogen har över 10 000 rader,
# så html:en skrivs som text, vilket går mycket fortare än med htmltools-taggar.
# Tabellerna är länkar som skickar tabell-id:t till Shiny via JavaScript i app.R.
es_tree_html <- function(tree, tables, open = FALSE, selected = NULL) {
  esc <- function(x) htmltools::htmlEscape(x)
  rad <- match(unique(unlist(tree$all_ids)), tables$id)
  t <- tables[rad, , drop = FALSE]
  period <- ifelse(!is.na(t$first_period) & !is.na(t$last_period),
                   paste0('<span class="es-period">', esc(t$first_period), "–",
                          esc(t$last_period), "</span>"), "")
  badge <- ifelse(t$type == "table", '<span class="es-badge">tabell</span>', "")
  items <- paste0(
    '<li><a href="#" class="es-table', ifelse(t$id %in% selected, " es-selected", ""),
    '" data-id="', htmltools::htmlEscape(t$id, attribute = TRUE), '">',
    '<span class="es-table-id">', esc(t$id), "</span>",
    '<span class="es-table-label">', esc(t$label), "</span></a>", period, badge, "</li>"
  )
  names(items) <- t$id

  node_html <- function(node) {
    body <- paste0(
      paste(vapply(node$children, node_html, ""), collapse = ""),
      if (length(node$tables) > 0) {
        paste0('<ul class="es-tables">', paste(items[node$tables], collapse = ""), "</ul>")
      }
    )
    if (node$key == es_root_key) return(body)
    paste0('<details class="es-node"', if (open) " open", "><summary>",
           '<span class="es-folder">', esc(node$label), "</span>",
           '<span class="es-count">', node$n, "</span></summary>",
           '<div class="es-children">', body, "</div></details>")
  }
  shiny::HTML(paste0('<div class="es-tree">', node_html(tree), "</div>"))
}

#' Sök bland tabellerna hos Eurostat eller OECD
#'
#' Söker i datakällans tabellista efter tabeller vars id eller namn innehåller
#' alla ord i `pattern`. Listan hämtas en gång och sparas sedan i 24 timmar, per
#' datakälla och språk.
#'
#' Eurostat har två sorters tabeller. Ett dataset (`typ = "dataset"`) är hela
#' datakuben och en tabell (`typ = "table"`) är ett färdigt, mindre urval, ofta av
#' ett dataset. Båda hämtas på samma sätt med [es_get_data()]. OECD har bara dataset,
#' och dess lista saknar perioder, uppdateringsdatum och antal observationer.
#'
#' @param pattern Sökord som text. `NULL` (standard) ger alla tabeller.
#' @param lang Språk på tabellnamnen: `"en"` (standard), `"de"` (bara Eurostat) eller
#'   `"fr"`.
#' @param only_datasets Om `TRUE` ges bara dataset. Standard är `FALSE`.
#' @param source Datakälla: `"eurostat"` (standard) eller `"oecd"`, som kräver paketet
#'   OECDdata.
#'
#' @return En tibble med en rad per tabell och kolumnerna `id`, `namn`, `typ`,
#'   `forsta_period`, `sista_period`, `uppdaterad` och `antal_obs`.
#' @export
#' @example inst/examples/es_search_tables.R
es_search_tables <- function(pattern = NULL, lang = "en", only_datasets = FALSE, source = "eurostat") {
  source <- es_check_source(source)
  lang <- es_check_lang(lang, source)
  k <- es_fetch_catalogue(lang = lang, source = source)
  t <- es_filter_tables(k$tables, pattern %||% "", only_datasets)
  tibble::tibble(id = t$id, namn = t$label, typ = t$type, forsta_period = t$first_period,
                 sista_period = t$last_period, uppdaterad = t$updated, antal_obs = t$n_obs)
}
