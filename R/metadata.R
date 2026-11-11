# Variabler och värden för en tabell. Eurostats SDMX-API ger två svar:
#   dataflow med referencepartial: variablerna samt namn och koder på alla språk
#   contentconstraint: exakt vilka värden tabellen har, även perioderna

es_api <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/"
es_ns <- c(s = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure",
           c = "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common")

# Hos OECD (oecd = TRUE) görs ett nytt försök bara om servern tillfälligt inte svarar, aldrig vid en
# spärr (429), där varje försök kan räknas mot gränsen.
es_read_xml <- function(url, oecd = FALSE) {
  resp <- httr2::request(url) |>
    httr2::req_user_agent("intstatgui (https://github.com/rOpenGov/eurostat)") |>
    httr2::req_retry(max_tries = if (oecd) 2 else 3, max_seconds = 60,
                     is_transient = if (oecd) function(r) httr2::resp_status(r) %in% es_oecd_tillfalliga) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    fel <- tryCatch(xml2::xml_text(xml2::xml_find_first(httr2::resp_body_xml(resp), "//faultstring")),
                    error = function(e) NA_character_)
    stop("Eurostat svarade med status ", httr2::resp_status(resp),
         if (!is.na(fel)) paste0(": ", fel), call. = FALSE)
  }
  httr2::resp_body_xml(resp)
}

# Namnet på det valda språket, annars engelska, annars reserv
es_xml_name <- function(node, lang, reserv = NA_character_) {
  namn <- xml2::xml_find_all(node, "./c:Name", es_ns)
  sprak <- xml2::xml_attr(namn, "lang")
  i <- match(lang, sprak)
  if (is.na(i)) i <- match("en", sprak)
  if (is.na(i)) reserv else xml2::xml_text(namn[[i]])
}

es_get_structure <- function(id, lang = "en", source = "eurostat") {
  if (source == "oecd") return(es_oecd_structure(id, lang))
  flow <- es_read_xml(paste0(es_api, "dataflow/ESTAT/", id,
                             "/latest?references=descendants&detail=referencepartial"))
  constraint <- tryCatch(es_read_xml(paste0(es_api, "contentconstraint/ESTAT/", id)),
                         error = function(e) NULL)
  es_parse_structure(flow, constraint, id, lang)
}

# variables: code, label, role ("geo", "time" eller NA), size, elimination
# values: variabelkod -> data.frame(code, label)
# Tidsvariabeln heter time, som i data och i frågorna till API:et. En variabel kan
# bara utelämnas om dess värden är okända: annars vore det samma sak som alla värden.
es_parse_structure <- function(flow, constraint, id, lang = "en") {
  flow_node <- xml2::xml_find_first(flow, "//s:Dataflow", es_ns)
  concepts <- xml2::xml_find_all(flow, "//s:Concept", es_ns)
  concept_ids <- xml2::xml_attr(concepts, "id")
  codelists <- xml2::xml_find_all(flow, "//s:Codelist", es_ns)
  codelist_ids <- xml2::xml_attr(codelists, "id")
  dims <- xml2::xml_find_all(flow, "//s:DimensionList/*", es_ns)
  dims <- dims[order(xml2::xml_name(dims) == "TimeDimension")]

  actual <- list()
  if (!is.null(constraint)) {
    for (k in xml2::xml_find_all(constraint, "//s:CubeRegion/c:KeyValue", es_ns)) {
      actual[[xml2::xml_attr(k, "id")]] <- xml2::xml_text(xml2::xml_children(k))
    }
  }

  values <- list()
  rows <- lapply(dims, function(d) {
    dim_id <- xml2::xml_attr(d, "id")
    if (xml2::xml_name(d) == "TimeDimension") {
      koder <- sort(actual[[dim_id]] %||% character())
      values[["time"]] <<- data.frame(code = koder, label = koder, stringsAsFactors = FALSE)
      return(data.frame(code = "time", label = "Time", role = "time", size = length(koder),
                        elimination = length(koder) == 0, stringsAsFactors = FALSE))
    }
    ref <- xml2::xml_find_first(d, ".//*[local-name()='Enumeration']/*[local-name()='Ref']")
    i <- match(xml2::xml_attr(ref, "id"), codelist_ids)
    koder <- character()
    namn <- character()
    if (!is.na(i)) {
      noder <- xml2::xml_find_all(codelists[[i]], "./s:Code", es_ns)
      koder <- xml2::xml_attr(noder, "id")
      namn <- vapply(seq_along(noder), function(j) es_xml_name(noder[[j]], lang, koder[j]), "")
    }
    # tabellens egna värden, i Eurostats ordning
    if (!is.null(actual[[dim_id]])) {
      ny <- actual[[dim_id]]
      namn <- namn[match(ny, koder)]
      namn[is.na(namn)] <- ny[is.na(namn)]
      koder <- ny
    }
    values[[dim_id]] <<- data.frame(code = koder, label = namn, stringsAsFactors = FALSE)
    j <- match(dim_id, concept_ids)
    data.frame(code = dim_id,
               label = if (is.na(j)) dim_id else es_xml_name(concepts[[j]], lang, dim_id),
               role = if (dim_id == "geo") "geo" else NA_character_,
               size = length(koder), elimination = length(koder) == 0,
               stringsAsFactors = FALSE)
  })

  list(id = id, title = es_xml_name(flow_node, lang, id), lang = lang,
       variables = do.call(rbind, rows), values = values,
       annotations = es_parse_annotations(flow_node))
}

es_parse_annotations <- function(flow_node) {
  a <- xml2::xml_find_all(flow_node, "./c:Annotations/c:Annotation", es_ns)
  falt <- function(namn) xml2::xml_text(xml2::xml_find_first(a, paste0("./c:", namn), es_ns))
  data.frame(type = falt("AnnotationType"), title = falt("AnnotationTitle"),
             text = falt("AnnotationText"), url = falt("AnnotationURL"),
             stringsAsFactors = FALSE)
}

# Länder (och kandidatländer) vars regionkoder följer NUTS. Koden visar nivån: SE1,
# SE11, SE110. Alla andra koder med två tecken är också länder, utom EU och EA. Annat,
# t.ex. EU27_2020, EA20, EEA30 och EFTA, är aggregat.
es_countries <- c("AL", "AT", "BA", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
                  "FI", "FR", "HR", "HU", "IE", "IS", "IT", "LI", "LT", "LU", "LV", "ME", "MK",
                  "MT", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SI", "SK", "TR", "UK", "XK")

es_geo_levels <- c("Länder" = "land", "NUTS 1" = "nuts1", "NUTS 2" = "nuts2",
                   "NUTS 3" = "nuts3", "Aggregat och övriga" = "ovriga")

es_geo_level <- function(codes) {
  n <- nchar(codes)
  aggregat <- grepl("^(EU|EA|EEA)", codes) | grepl("[^A-Z0-9]", codes) | n > 5
  i_nuts <- substr(codes, 1, 2) %in% es_countries
  ifelse(aggregat, "ovriga",
         ifelse(n == 2, "land",
                ifelse(!i_nuts, "ovriga",
                       ifelse(n == 3, "nuts1", ifelse(n == 4, "nuts2", "nuts3")))))
}

# Indelningarna som en tabell kan erbjuda för regioner: minst två olika nivåer. Samma
# form som kodlistor i pxweb2rgui, men här bara ett urval av tabellens egna värden.
es_geo_choices <- function(values) {
  finns <- unique(es_geo_level(values$code))
  val <- es_geo_levels[es_geo_levels %in% finns]
  if (length(val) < 2) return(NULL)
  data.frame(id = unname(val), label = names(val), type = "indelning", stringsAsFactors = FALSE)
}

#' Hämta en tabells variabler och värden
#'
#' Ger koder och namn för alla variabler och värden i en tabell hos Eurostat eller
#' OECD, till exempel för att skriva `filters` till [es_get_data()].
#'
#' OECD anger bara vilken tid en tabell täcker. Perioderna räknas därför fram från
#' tabellens frekvenser, och alla behöver inte ha data.
#'
#' @param id Tabellens id, t.ex. `"demo_gind"` eller, hos OECD,
#'   `"DSD_LFS@DF_IALFS_UNE_M"`. Obligatorisk.
#' @param variable Variabelkod, t.ex. `"geo"`, om bara en variabel ska ges. `NULL`
#'   (standard) ger alla.
#' @param lang Språk på namnen: `"en"` (standard), `"de"` (bara Eurostat) eller `"fr"`.
#' @param source Datakälla: `"eurostat"` (standard) eller `"oecd"`, som kräver paketet
#'   OECDdata.
#'
#' @return En tibble med kolumnerna `var_kod`, `var_namn`, `kod` och `namn`, med en
#'   rad per värde.
#' @export
#' @example inst/examples/es_get_codes.R
es_get_codes <- function(id, variable = NULL, lang = "en", source = "eurostat") {
  source <- es_check_source(source)
  lang <- es_check_lang(lang, source)
  st <- es_get_structure(id, lang, source)
  v <- st$variables
  if (!is.null(variable)) {
    saknas <- setdiff(variable, v$code)
    if (length(saknas) > 0) {
      stop("Tabellen ", id, " har inte variabeln ", paste(saknas, collapse = ", "),
           ". Variablerna är ", paste(v$code, collapse = ", "), ".", call. = FALSE)
    }
    v <- v[v$code %in% variable, , drop = FALSE]
  }
  tibble::as_tibble(do.call(rbind, lapply(seq_len(nrow(v)), function(i) {
    d <- st$values[[v$code[i]]]
    data.frame(var_kod = rep(v$code[i], nrow(d)), var_namn = rep(v$label[i], nrow(d)),
               kod = d$code, namn = d$label, stringsAsFactors = FALSE)
  })))
}

# Vad annotationerna ska visas som, i visningsordning
es_comment_types <- c(
  UPDATE_DATA = "Data uppdaterad",
  UPDATE_STRUCTURE = "Struktur ändrad",
  OBS_PERIOD_OVERALL_OLDEST = "Första period",
  OBS_PERIOD_OVERALL_LATEST = "Senaste period",
  OBS_COUNT = "Antal observationer",
  SOURCE_INSTITUTIONS = "Källa",
  ESMS_HTML = "Metadata (ESMS)",
  FOOTNOTE = "Fotnoter"
)

es_structure_comments <- function(st) {
  if (identical(st$source, "oecd")) return(es_oecd_comments(st))
  a <- st$annotations
  rows <- lapply(names(es_comment_types), function(typ) {
    r <- a[a$type == typ, , drop = FALSE]
    if (nrow(r) == 0) return(NULL)
    text <- ifelse(is.na(r$text), r$title, r$text)
    if (startsWith(typ, "UPDATE_")) text <- substr(text, 1, 10)
    data.frame(typ = es_comment_types[[typ]], text = text, url = r$url, stringsAsFactors = FALSE)
  })
  doi <- a$title[a$type == "DISSEMINATION_DOI_XML"]
  doi <- regmatches(doi, regexpr("https://doi\\.org/[^\"]+", doi))
  rows <- c(rows, list(
    if (length(doi) > 0) data.frame(typ = "DOI", text = doi[1], url = doi[1], stringsAsFactors = FALSE),
    data.frame(typ = "Eurostat Data Browser", text = st$id,
               url = paste0("https://ec.europa.eu/eurostat/databrowser/view/", st$id,
                            "/default/table?lang=", st$lang),
               stringsAsFactors = FALSE)
  ))
  do.call(rbind, rows)
}

#' Hämta uppgifter om en tabell
#'
#' Ger Eurostats uppgifter om en tabell: när data och struktur senast ändrades,
#' första och senaste period, antal observationer, källa och länkar till
#' metadata (ESMS), fotnoter, DOI och Eurostat Data Browser. Hos OECD är det tabellens
#' beskrivning, ansvarig enhet, version och en länk till OECD Data Explorer.
#'
#' @param id Tabellens id, t.ex. `"demo_gind"`. Flera id:n ger en rad per uppgift
#'   och tabell. Obligatorisk.
#' @param lang Språk på text och länkar: `"en"` (standard), `"de"` (bara Eurostat) eller
#'   `"fr"`.
#' @param source Datakälla: `"eurostat"` (standard) eller `"oecd"`, som kräver paketet
#'   OECDdata.
#'
#' @return En tibble med kolumnerna `tabell`, `typ`, `text` och `url`. `url` är
#'   `NA` för uppgifter som inte är länkar.
#' @export
#' @example inst/examples/es_get_comments.R
es_get_comments <- function(id, lang = "en", source = "eurostat") {
  source <- es_check_source(source)
  lang <- es_check_lang(lang, source)
  tibble::as_tibble(do.call(rbind, lapply(id, function(x) {
    k <- es_structure_comments(es_get_structure(x, lang, source))
    cbind(tabell = x, k, stringsAsFactors = FALSE)
  })))
}
