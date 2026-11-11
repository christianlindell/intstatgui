# Bygger frågan till es_get_data() och motsvarande R-kod
#
# Varje variabel har en lista med valda värden:
#   alla värden valda                 -> "*" (variabeln utelämnas ur filtren)
#   inga värden                       -> problem, frågan kan inte köras
#   "senaste perioden" ikryssad (tid) -> "latest" (lastTimePeriod = 1)
#   annars                            -> de valda koderna
# En utelämnad variabel betyder alla värden, inte en summa. Hos Eurostat krävs därför ett val i
# varje variabel, utom när värdena är okända. Hos OECD är en tom lista alla möjliga värden, eftersom
# svaret bara har de kombinationer som finns (variables$elimination är då TRUE).

# variables, values: som i es_get_structure();
# selections: variabelkod -> list(values =, latest =, indelad =)
es_build_query <- function(variables, values, selections) {
  problems <- character()
  query <- list()
  for (i in seq_len(nrow(variables))) {
    code <- variables$code[i]
    s <- selections[[code]] %||% list()
    chosen <- as.character(s$values %||% character())
    all_codes <- values[[code]]$code %||% character()
    query[[code]] <- if (isTRUE(s$latest)) {
      "latest"
    } else if (length(chosen) == 0) {
      if (!isTRUE(variables$elimination[i])) {
        problems <- c(problems, paste0("Välj minst ett värde för ", variables$label[i], "."))
      }
      "*"
    } else if (!isTRUE(s$indelad) && setequal(chosen, all_codes)) {
      "*"
    } else {
      all_codes[all_codes %in% chosen]   # listans ordning
    }
  }
  list(query = query, problems = problems)
}

# Frågan som filter till es_get_data(), i variablernas ordning. Ingen variabel med
# urval ger NULL, vilket är hela tabellen.
es_query_filters <- function(query) {
  ut <- list()
  for (n in names(query)) {
    v <- query[[n]]
    if (identical(v, "*")) next
    if (identical(v, "latest")) ut$lastTimePeriod <- 1 else ut[[n]] <- v
  }
  if (length(ut) == 0) NULL else ut
}

# Förvalen i listorna
es_defaults <- function(st) {
  codes <- st$variables$code
  stats::setNames(lapply(codes, function(k) {
    es_default_selection(st$variables$role[match(k, codes)], st$values[[k]]$code)
  }), codes)
}

# Varning för mycket stora urval. Hämtningen stoppas inte.
es_cell_warning <- function(cells, limit = 1e6) {
  if (cells > limit) {
    paste("Varning: urvalet är större än 1 miljon celler. Hämtningen kan ta lång tid",
          "och belasta Eurostats server.")
  }
}

es_r_value <- function(v) {
  if (is.numeric(v)) return(if (length(v) == 1) as.character(v) else paste0("c(", paste(v, collapse = ", "), ")"))
  v <- gsub("\\", "\\\\", v, fixed = TRUE)
  q <- paste0('"', gsub('"', '\\"', v, fixed = TRUE), '"')
  if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
}

# R-kod som återskapar hämtningen. Språk och tidsformat skrivs alltid ut. Med direct = TRUE
# anropas eurostat::get_eurostat(), så att koden inte kräver det här paketet. Det går bara
# utan kodkolumner.
es_query_code <- function(id, title, filters, code_variables = NULL, lang = "en",
                          time_format = "raw", clean_names = FALSE, direct = FALSE,
                          object_name = "dataset_df", source = "eurostat") {
  direct <- direct && source == "eurostat" && length(code_variables) == 0
  r_name <- function(n) if (make.names(n) == n) n else paste0("`", n, "`")
  rader <- if (length(filters) > 0) {
    paste0("    ", vapply(names(filters), r_name, ""), " = ", vapply(filters, es_r_value, ""),
           collapse = ",\n")
  }
  filter_kod <- if (is.null(rader)) "NULL" else paste0("list(\n", rader, "\n  )")
  paste0(
    if (direct) "library(eurostat)\n\n" else "library(intstatgui)\n\n",
    "# ", title, "\n",
    object_name, if (direct) " <- get_eurostat(\n" else " <- es_get_data(\n",
    "  id = ", es_r_value(id), ",\n",
    "  filters = ", filter_kod, ",\n",
    if (direct) '  type = "label",\n',
    if (length(code_variables) > 0) paste0("  code_variables = ", es_r_value(unname(code_variables)), ",\n"),
    "  lang = ", es_r_value(lang), ",\n",
    "  time_format = ", es_r_value(time_format),
    if (direct) ",\n  cache = FALSE",
    if (source != "eurostat") paste0(",\n  source = ", es_r_value(source)),
    "\n)\n",
    if (isTRUE(clean_names)) {
      paste0("\n# R-kompatibla kolumnnamn\n", object_name, " <- janitor::clean_names(", object_name, ")\n")
    }
  )
}

# Hämtat data har kodkolumner för alla variabler; de som inte är valda tas bort
es_keep_code_columns <- function(d, code_variables) {
  dolda <- setdiff(names(d)[endsWith(names(d), "_code")], paste0(code_variables, "_code"))
  d[, !names(d) %in% dolda, drop = FALSE]
}

es_clean_names <- function(d) {
  if (!requireNamespace("janitor", quietly = TRUE)) return(d)
  janitor::clean_names(d)
}
