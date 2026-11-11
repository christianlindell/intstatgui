# Hämtning av data med eurostat::get_eurostat()

# Parametrar som väljer perioder men inte är variabler
es_time_params <- c("lastTimePeriod", "sinceTimePeriod", "untilTimePeriod")

# Filtren för en viss tabell. Variabler som tabellen saknar hoppas över och värden som
# den saknar tas bort. `unknown` är filter som tabellen saknar och `missing` är
# variabler där inget av de valda värdena finns: då kan tabellen inte hämtas.
es_table_filters <- function(filters, variables, values) {
  ut <- list()
  unknown <- character()
  missing <- character()
  for (n in names(filters)) {
    if (n %in% es_time_params) {
      ut[[n]] <- filters[[n]]
      next
    }
    kod <- if (tolower(n) %in% c("time", "time_period")) "time" else n
    if (!kod %in% variables$code) {
      unknown <- c(unknown, n)
      next
    }
    vals <- as.character(filters[[n]])
    finns <- values[[kod]]$code
    if (length(finns) > 0) vals <- vals[vals %in% finns]
    if (length(vals) == 0) {
      missing <- c(missing, variables$label[match(kod, variables$code)])
    } else {
      ut[[kod]] <- vals
    }
  }
  list(filters = ut, unknown = unknown, missing = missing)
}

# Antal celler som filtren ger: rutnätet, där inte alla celler behöver ha data
es_cell_count <- function(variables, values, filters) {
  sizes <- vapply(seq_len(nrow(variables)), function(i) {
    kod <- variables$code[i]
    if (kod == "time" && is.null(filters$time)) {
      return(as.numeric(length(es_time_codes(values$time$code, filters))))
    }
    v <- filters[[kod]]
    if (is.null(v)) as.numeric(variables$size[i]) else as.numeric(length(v))
  }, 0)
  prod(sizes)
}

# Perioderna som tidsparametrarna väljer
es_time_codes <- function(codes, filters) {
  codes <- sort(codes)
  if (!is.null(filters$sinceTimePeriod)) codes <- codes[codes >= as.character(filters$sinceTimePeriod)]
  if (!is.null(filters$untilTimePeriod)) codes <- codes[codes <= as.character(filters$untilTimePeriod)]
  if (!is.null(filters$lastTimePeriod)) codes <- utils::tail(codes, as.numeric(filters$lastTimePeriod))
  codes
}

# Delar filtren så att adressen inte blir för lång och svaret inte för stort. Den
# variabel som har flest värden delas på mitten, tills varje del är tillräckligt liten.
# En variabel utan filter skrivs då ut med alla sina värden.
es_split_filters <- function(filters, variables, values,
                             max_chars = getOption("intstatgui.max_chars", 6000),
                             max_cells = getOption("intstatgui.max_cells", 2e6)) {
  tecken <- sum(nchar(names(filters)) * lengths(filters) + 2 * lengths(filters)) +
    sum(nchar(unlist(filters, use.names = FALSE)))
  if (length(filters) == 0) return(list(filters))
  if (tecken <= max_chars && es_cell_count(variables, values, filters) <= max_cells) {
    return(list(filters))
  }
  antal <- vapply(variables$code, function(k) {
    if (k == "time" && any(es_time_params %in% names(filters))) return(0)
    length(filters[[k]] %||% values[[k]]$code)
  }, 0L + 0)
  if (max(antal) < 2) return(list(filters))
  kod <- variables$code[which.max(antal)]
  vals <- filters[[kod]] %||% values[[kod]]$code
  halv <- seq_len(ceiling(length(vals) / 2))
  a <- filters
  b <- filters
  a[[kod]] <- vals[halv]
  b[[kod]] <- vals[-halv]
  c(es_split_filters(a, variables, values, max_chars, max_cells),
    es_split_filters(b, variables, values, max_chars, max_cells))
}

# En hämtning, alltid med koder och perioder som text. Utan filter hämtar get_eurostat()
# hela tabellen som fil, vilket inte har någon storleksgräns. Den egna cachen används
# inte: den skiljer inte på språk och kan ge gamla värden.
es_fetch_part <- function(id, filters, stor = TRUE) {
  hamta <- function(f) {
    d <- eurostat::get_eurostat(id, filters = f, type = "code", time_format = "raw",
                                cache = FALSE, verbose = FALSE)
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    names(d)[names(d) == "TIME_PERIOD"] <- "time"
    d$time <- as.character(d$time)
    d
  }
  if (length(filters) == 0) return(hamta(NULL))
  tryCatch(
    hamta(filters),
    error = function(e) {
      # Eurostat kan svara att en stor hämtning skickas senare (status 413,
      # ASYNCHRONOUS_RESPONSE). Hela tabellen som fil går alltid att hämta. Samma fel, inga
      # värden, får också ett urval där kombinationen saknar data: för ett litet urval ska
      # det inte leda till att hela tabellen laddas ner.
      if (!grepl("413|ASYNCHRONOUS|didn't have any values", conditionMessage(e))) stop(e)
      if (!grepl("413|ASYNCHRONOUS", conditionMessage(e)) && !stor) {
        return(data.frame(time = character(), values = numeric()))
      }
      message("Eurostat svarar inte direkt på frågan. Hämtar hela ", id, " och väljer ur den.")
      es_filter_rows(hamta(NULL), filters)
    }
  )
}

es_filter_rows <- function(d, filters) {
  for (n in setdiff(names(filters), es_time_params)) {
    d <- d[d[[n]] %in% filters[[n]], , drop = FALSE]
  }
  if (is.null(filters$time)) d <- d[d$time %in% es_time_codes(unique(d$time), filters), , drop = FALSE]
  d
}

# Koderna blir namn, och de variabler som ska ha kvar koden får kolumnen <variabel>_code
es_label_data <- function(d, st, type = "label", code_variables = NULL) {
  if (type == "code") return(d)
  kolumner <- list()
  for (n in names(d)) {
    vals <- st$values[[n]]
    if (n == "time" || is.null(vals)) {
      kolumner[[n]] <- d[[n]]
      next
    }
    if (n %in% code_variables) kolumner[[paste0(n, "_code")]] <- d[[n]]
    namn <- vals$label[match(d[[n]], vals$code)]
    namn[is.na(namn)] <- d[[n]][is.na(namn)]
    kolumner[[n]] <- namn
  }
  as.data.frame(kolumner, stringsAsFactors = FALSE, check.names = FALSE)
}

# eurotime2date() tolkar alla värden som den första, så olika sorters perioder
# (OECD kan blanda 2024 och 2024-Q1) görs om var för sig.
es_convert_time <- function(x, time_format) {
  if (time_format == "raw" || length(x) == 0) return(x)
  fun <- switch(time_format,
                date = eurostat::eurotime2date,
                date_last = function(v) eurostat::eurotime2date(v, last = TRUE),
                num = eurostat::eurotime2num,
                stop("time_format ska vara \"raw\", \"date\", \"date_last\" eller \"num\".", call. = FALSE))
  sorter <- split(seq_along(x), es_time_group(x))
  ut <- fun(x[sorter[[1]]])[rep(1L, length(x))]
  for (i in sorter) ut[i] <- fun(x[i])
  ut
}

#' Hämta data från Eurostat eller OECD
#'
#' Hämtar en tabell och ger värdenas namn på valt språk, så att
#' resultatet går att läsa direkt. Från Eurostat hämtas data med
#' [eurostat::get_eurostat()] och från OECD med `OECDdata::importData()`. Det är den
#' här funktionen som koden från [es_gui()] anropar.
#'
#' OECD tillåter 60 hämtningar av data per timme, och spärrar tillfälligt vid fler.
#' Tabellen är en hämtning, och en till för varje sorts period om du väljer till
#' exempel både år och månader. OECD kan bara välja tid som ett intervall, så funktionen
#' väljer de perioder du angett ur svaret. `lastTimePeriod` gäller hos OECD tabellens
#' senaste period och inte varje serie för sig, och kan därför ge ett tomt resultat.
#' En variabel som saknas i `filters` ger bara de kombinationer som finns, så olika mått får sina
#' egna enheter. Har en variabel flera valda värden och något av dem inte ger någon rad säger ett
#' meddelande det, och attributet `saknas` på resultatet innehåller samma text.
#' Med OECD finns kolumnen `flagga` med värdets status, och `UNIT_MULT`, som visar vilken
#' tiopotens värdet anges i.
#'
#' Till skillnad från `get_eurostat()` kan funktionen ge både namn och koder och
#' dela upp mycket stora frågor. Den skiljer på språk,
#' eftersom cachen i `get_eurostat()` inte används: den skulle ge namnen från en
#' tidigare hämtning på ett annat språk.
#'
#' En variabel som saknas i `filters` hämtas med alla sina värden. Utan några filter
#' alls hämtas hela tabellen som fil.
#'
#' Eurostats flaggor (t.ex. `p` för preliminära värden) följer inte med, eftersom
#' `get_eurostat()` bara kan ge dem för en hel tabell.
#'
#' @param id Tabellens id, t.ex. `"demo_gind"` eller, hos OECD,
#'   `"DSD_LFS@DF_IALFS_UNE_M"`. En enda tabell. Obligatorisk.
#' @param filters Namngiven lista med variabelkoder och valda koder, t.ex.
#'   `list(geo = c("SE", "NO"), time = 2020:2023)`. Även `lastTimePeriod`,
#'   `sinceTimePeriod` och `untilTimePeriod` går att ange. `NULL` (standard) ger
#'   hela tabellen. [es_get_codes()] visar vilka koder som finns.
#' @param type `"label"` (standard) ger namn på värdena och `"code"` ger koder.
#' @param code_variables Variabelkoder, t.ex. `"geo"`, som också ska ha en kolumn med
#'   koden, `geo_code`. Används bara med `type = "label"`. Standard är `NULL`, inga.
#' @param lang Språk på namnen: `"en"` (standard), `"de"` (bara Eurostat) eller `"fr"`.
#' @param time_format Hur tiden visas: `"raw"` (standard) är Eurostats text, t.ex.
#'   `"2023-Q1"`, `"date"` är periodens första dag, `"date_last"` dess sista dag och
#'   `"num"` ett tal, t.ex. `2023.25`.
#' @param na_rm Om `TRUE` (standard) tas rader utan värde bort, vilket ger samma
#'   resultat med och utan filter. Många tabeller saknar värden för en stor del av
#'   rutnätet.
#' @param quiet Om `TRUE` visas inga meddelanden om hämtningen. Standard är `FALSE`.
#' @param source Datakälla: `"eurostat"` (standard) eller `"oecd"`, som kräver paketet
#'   OECDdata.
#'
#' @return En tibble med en kolumn per variabel, `time` och `values`.
#' @export
#' @example inst/examples/es_get_data.R
es_get_data <- function(id, filters = NULL, type = c("label", "code"), code_variables = NULL,
                        lang = "en", time_format = "raw", na_rm = TRUE, quiet = FALSE,
                        source = "eurostat") {
  type <- match.arg(type)
  source <- es_check_source(source)
  lang <- es_check_lang(lang, source)
  say <- function(...) if (!quiet) message(...)
  if (length(id) != 1) stop("id ska vara en enda tabell.", call. = FALSE)
  st <- es_get_structure(id, lang, source)
  tf <- es_table_filters(filters, st$variables, st$values)
  if (length(tf$unknown) > 0) {
    stop("Tabellen ", id, " har inte ", paste(tf$unknown, collapse = ", "),
         ". Variablerna är ", paste(st$variables$code, collapse = ", "), ".", call. = FALSE)
  }
  if (length(tf$missing) > 0) {
    stop(id, " hämtas inte: inga giltiga värden valda för ", paste(tf$missing, collapse = ", "), ".",
         call. = FALSE)
  }
  d <- if (source == "oecd") {
    es_oecd_fetch(id, tf$filters, st, lang)
  } else {
    parts <- es_split_filters(tf$filters, st$variables, st$values)
    if (length(parts) > 1) say("Hämtar ", id, " i ", length(parts), " delar.")
    dplyr::bind_rows(lapply(parts, function(f) {
      es_fetch_part(id, f, stor = es_cell_count(st$variables, st$values, f) > 1e6)
    }))
  }
  if (na_rm) d <- d[!is.na(d$values), , drop = FALSE]
  saknas <- character()
  if (nrow(d) == 0) {
    say("Urvalet gav inga värden från ", id, ". Kombinationen av värden saknar data.")
  } else if (source == "oecd") {
    saknas <- es_missing_values(d, tf$filters, st)
    if (length(saknas) > 0) say("Inga värden med övriga val för ", paste(saknas, collapse = "; "), ".")
  }
  d <- es_label_data(d, st, type, code_variables)
  d$time <- es_convert_time(d$time, time_format)
  res <- tibble::as_tibble(d)
  if (length(saknas) > 0) attr(res, "saknas") <- saknas
  res
}
