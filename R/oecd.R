# OECD: tabellista, struktur och hämtning med paketet OECDdata. Resultaten har samma form
# som för Eurostat, så att gränssnittet kan använda dem likadant. OECD tillåter 60
# hämtningar av data per timme, så en fråga delas inte upp i onödan. Metadata räknas inte
# in i OECDdata:s egen spärr.

es_oecd_api <- "https://sdmx.oecd.org/public/rest/"

# Status när OECD:s server tillfälligt inte svarar
es_oecd_tillfalliga <- c(502, 503, 504)

es_is_server_error <- function(e) grepl("HTTP 50[234]", conditionMessage(e))

# Har OECD spärrat för många anrop?
es_is_rate_limit <- function(e) grepl("429|rate limit", conditionMessage(e), ignore.case = TRUE)

# Felet som text till användaren
es_error_text <- function(e) {
  if (es_is_server_error(e)) {
    return("OECD:s server svarade inte just nu. Det brukar vara tillfälligt, så försök igen om en stund.")
  }
  if (!es_is_rate_limit(e)) return(conditionMessage(e))
  paste("OECD har tillfälligt spärrat anropen från din dator, eftersom de har varit för många.",
        "Gränsen gäller per timme, så vänta en stund och försök igen.")
}

es_need_oecddata <- function() {
  if (!requireNamespace("OECDdata", quietly = TRUE)) {
    stop("OECD kräver paketet OECDdata. Installera med install.packages(\"OECDdata\").", call. = FALSE)
  }
}

# OECDdata gör inga nya försök: vid en spärr räknas varje försök mot gränsen. Ett tillfälligt
# serverfel provas i stället en gång till i es_oecd_import().
es_with_oecd <- function(lang, code, api_version = "v2") {
  es_need_oecddata()
  old <- OECDdata::oecdOptions(language = lang, quiet = TRUE, strings_as_factors = FALSE,
                               open_browser = FALSE, api_version = api_version, retries = 0)
  on.exit(options(old), add = TRUE)
  force(code)
}

# Svaren som tabellistan och ämnesträdet byggs av
es_oecd_toc <- function(lang = "en") {
  list(
    list = es_with_oecd(lang, as.data.frame(OECDdata::importDataList())),
    scheme = es_read_xml(paste0(es_oecd_api, "categoryscheme/OECD/all?references=none"), oecd = TRUE),
    categorisation = es_read_xml(paste0(es_oecd_api, "categorisation/all?references=none"), oecd = TRUE),
    lang = lang
  )
}

# Samma form som es_parse_toc(). Ämnesträdet finns i OECD:s kategorier, där nyckeln är
# hela vägen, t.ex. GEO.GEO_URB. Tabeller utan kategori samlas i en egen mapp.
es_parse_oecd_toc <- function(raw) {
  lang <- raw$lang %||% "en"
  l <- raw$list
  tables <- data.frame(id = as.character(l$Dataflow), label = as.character(l[["Dataset name"]]),
                       type = "dataset", first_period = NA_character_, last_period = NA_character_,
                       updated = NA_character_, n_obs = NA_real_, stringsAsFactors = FALSE)

  sc <- xml2::xml_find_first(raw$scheme, "//s:CategoryScheme[@id='OECDCS1']", es_ns)
  keys <- character()
  labels <- character()
  parents <- character()
  walk <- function(node, parent, prefix) {
    for (k in xml2::xml_find_all(node, "./s:Category", es_ns)) {
      key <- paste0(prefix, xml2::xml_attr(k, "id"))
      keys <<- c(keys, key)
      labels <<- c(labels, es_xml_name(k, lang, key))
      parents <<- c(parents, parent)
      walk(k, key, paste0(key, "."))
    }
  }
  walk(sc, es_root_key, "")

  cz <- xml2::xml_find_all(raw$categorisation, "//s:Categorisation", es_ns)
  ref <- function(del) xml2::xml_find_first(cz, paste0("./s:", del, "/*[local-name()='Ref']"), es_ns)
  items <- data.frame(table_id = xml2::xml_attr(ref("Source"), "id"),
                      key = xml2::xml_attr(ref("Target"), "id"), stringsAsFactors = FALSE)
  items <- unique(items[items$key %in% keys & items$table_id %in% tables$id, , drop = FALSE])

  utan <- setdiff(tables$id, items$table_id)
  if (length(utan) > 0) {
    keys <- c(keys, "_utan_amne")
    labels <- c(labels, "(Utan ämnesområde)")
    parents <- c(parents, es_root_key)
    items <- rbind(items, data.frame(table_id = utan, key = "_utan_amne", stringsAsFactors = FALSE))
  }
  list(tables = tables,
       folders = data.frame(key = keys, label = labels, parent = parents, stringsAsFactors = FALSE),
       items = items)
}

# Vilken tid tabellen täcker, som c(start, slut) med datum som text. OECD ger bara ett intervall.
es_parse_oecd_time <- function(xml) {
  del <- function(n) {
    xml2::xml_text(xml2::xml_find_first(xml, paste0("//c:KeyValue[@id='TIME_PERIOD']//c:", n), es_ns))
  }
  tid <- c(del("StartPeriod"), del("EndPeriod"))
  if (anyNA(tid)) NULL else substr(tid, 1, 10)
}

# Perioderna som intervallet och frekvenserna ger. De finaste kommer först, så att den
# senaste perioden för den grövsta frekvensen står sist och blir förvald.
es_oecd_periods <- function(range, freqs = "A") {
  if (is.null(range)) return(character())
  start <- as.Date(range[1])
  slut <- as.Date(range[2])
  ar <- as.numeric(format(start, "%Y")):as.numeric(format(slut, "%Y"))
  kvartal <- paste0(rep(ar, each = 4), "-Q", 1:4)
  manad <- paste0(rep(ar, each = 12), "-", sprintf("%02d", 1:12))
  i_tid <- function(x, forsta, sista) x >= forsta & x <= sista
  c(
    if ("M" %in% freqs) manad[i_tid(manad, format(start, "%Y-%m"), format(slut, "%Y-%m"))],
    if ("Q" %in% freqs) {
      q <- function(d) paste0(format(d, "%Y"), "-Q", (as.numeric(format(d, "%m")) - 1) %/% 3 + 1)
      kvartal[i_tid(kvartal, q(start), q(slut))]
    },
    if ("A" %in% freqs) as.character(ar)
  )
}

es_oecd_structure <- function(id, lang = "en") {
  s <- es_with_oecd(lang, OECDdata::oecdStructure(id))
  tid <- tryCatch(
    es_parse_oecd_time(es_read_xml(paste0(es_oecd_api, "availableconstraint/", s$flow$code, "/all"), oecd = TRUE)),
    error = function(e) NULL
  )
  es_parse_oecd_structure(s, tid, id, lang)
}

# s: från OECDdata::oecdStructure(); time: från es_parse_oecd_time()
es_parse_oecd_structure <- function(s, time, id, lang = "en") {
  dims <- as.data.frame(s$dimensions)
  dim_ids <- dims$id[dims$kind != "TimeDimension"]
  values <- list()
  for (k in dim_ids) {
    d <- as.data.frame(s$labels[[k]], stringsAsFactors = FALSE)
    if (is.null(s$labels[[k]])) d <- data.frame(code = character(), label = character())
    finns <- s$available[[k]]
    # tabellens egna värden, i kodlistans ordning
    if (!is.null(finns)) {
      saknas <- setdiff(finns, d$code)
      d <- rbind(d[d$code %in% finns, c("code", "label"), drop = FALSE],
                 data.frame(code = saknas, label = saknas, stringsAsFactors = FALSE))
    }
    rownames(d) <- NULL
    values[[k]] <- d[, c("code", "label"), drop = FALSE]
  }
  koder <- es_oecd_periods(time, s$available$FREQ %||% values$FREQ$code %||% "A")
  values$time <- data.frame(code = koder, label = koder, stringsAsFactors = FALSE)
  n <- lengths(lapply(values[dim_ids], `[[`, "code"))
  variables <- data.frame(code = c(dim_ids, "time"),
                          label = c(dims$label[match(dim_ids, dims$id)], "Time"),
                          role = c(ifelse(dim_ids == "REF_AREA", "geo", NA_character_), "time"),
                          size = c(unname(n), length(koder)), elimination = TRUE,
                          stringsAsFactors = FALSE)
  list(id = id, source = "oecd", title = s$flow$name %||% id, lang = lang,
       variables = variables, values = values, flow = s$flow)
}

# Vilka värden som går att kombinera med filtren. För varje variabel svarar OECD med de värden
# som finns givet valen i de andra variablerna, och med hur många observationer valet ger. Eurostat
# har ingen sådan fråga. Det här är metadata, inte en hämtning av data.
es_oecd_available <- function(st, filters = NULL) {
  es_parse_oecd_available(es_read_xml(es_oecd_available_url(st, filters), oecd = TRUE))
}

# Långa listor gör adressen till OECD för lång
es_oecd_too_long <- function(x) nchar(paste(x, collapse = "+")) > 1500

es_oecd_available_url <- function(st, filters = NULL) {
  dims <- st$variables$code[st$variables$code != "time"]
  delar <- vapply(dims, function(k) {
    # en för lång lista räknas som alla värden
    if (es_oecd_too_long(filters[[k]])) "" else paste(filters[[k]], collapse = "+")
  }, "")
  tid <- ""
  if (length(filters$time) > 0) {
    p <- as.character(filters$time)
    if (length(unique(es_time_group(p))) > 1) p <- substr(p, 1, 4)
    tid <- paste0("&startPeriod=", min(p), "&endPeriod=", max(p))
  }
  paste0(es_oecd_api, "availableconstraint/", st$flow$code, "/", paste(delar, collapse = "."),
         "/all?mode=available", tid)
}

# values: variabel -> koder, utan de variabler som saknas i svaret. Finns inga observationer
# saknas tiden, och då är OECD:s intervall omvänt.
es_parse_oecd_available <- function(xml) {
  values <- list()
  for (k in xml2::xml_find_all(xml, "//c:KeyValue", es_ns)) {
    id <- xml2::xml_attr(k, "id")
    if (id != "TIME_PERIOD") values[[id]] <- xml2::xml_text(xml2::xml_find_all(k, "./c:Value", es_ns))
  }
  tid <- es_parse_oecd_time(xml)
  if (!is.null(tid) && tid[1] > tid[2]) tid <- NULL
  obs <- xml2::xml_text(xml2::xml_find_first(
    xml, "//*[local-name()='Annotation'][@id='obs_count']/*[local-name()='AnnotationTitle']"))
  list(values = values, time = tid, obs = as.numeric(obs))
}

# Meddelandet till listorna i flik 2. id_of: variabelns plats -> listans id. latest är den senaste
# möjliga perioden, som ersätter ett förval som inte går att kombinera.
es_available_message <- function(t, a, id_of) {
  v <- t$variables
  ut <- lapply(seq_len(nrow(v)), function(i) {
    kod <- v$code[i]
    koder <- if (kod == "time") {
      if (is.null(a$time)) return(NULL)
      intersect(t$values$time$code, es_oecd_periods(a$time, a$values$FREQ %||% t$values$FREQ$code %||% "A"))
    } else {
      a$values[[kod]]
    }
    if (length(koder) == 0) return(NULL)
    list(id = id_of(i), koder = as.list(koder),
         latest = if (kod == "time") utils::tail(koder, 1))
  })
  Filter(Negate(is.null), ut)
}

es_oecd_comments <- function(st) {
  f <- st$flow
  text <- gsub("\\s+", " ", gsub("<[^>]+>", " ", f$description %||% ""))
  lank <- paste0("https://data-explorer.oecd.org/vis?lc=", st$lang,
                 "&df[ds]=dsDisseminateFinalDMZ&df[ag]=", f$agency,
                 "&df[id]=", utils::URLencode(f$id, reserved = TRUE), "&df[vs]=", f$version)
  data.frame(typ = c("Beskrivning", "Ansvarig", "Version", "OECD Data Explorer"),
             text = c(trimws(text), f$agency, f$version, f$id),
             url = c(NA, NA, NA, lank), stringsAsFactors = FALSE)
}

# Grupperar perioder efter hur de skrivs: 2024, 2024-Q1, 2024-01
es_time_group <- function(x) gsub("[0-9]", "0", x)

# Hos OECD betyder koden _Z att variabeln inte gäller för serien: Population har ingen prisbas och
# får Not applicable, medan GDP har Constant prices. Väljer man Constant prices tas _Z därför med, så
# att Population blir kvar. Det är synligt i koden.
es_add_not_applicable <- function(filters, values) {
  for (k in setdiff(names(filters), c("time", es_time_params))) {
    if ("_Z" %in% values[[k]]$code && !"_Z" %in% filters[[k]]) filters[[k]] <- c(filters[[k]], "_Z")
  }
  filters
}

# Valda värden som inte gav någon rad, som text, när en variabel har flera valda värden. Väljer man
# till exempel Population och GDP och bara enheten US dollar saknas Population i svaret.
es_missing_values <- function(d, filters, st) {
  ut <- character()
  for (k in setdiff(names(filters), c("time", es_time_params))) {
    vals <- setdiff(filters[[k]], "_Z")
    if (length(vals) < 2 || is.null(d[[k]])) next
    saknas <- setdiff(vals, d[[k]])
    if (length(saknas) == 0) next
    namn <- st$values[[k]]$label[match(saknas, st$values[[k]]$code)]
    namn[is.na(namn)] <- saknas[is.na(namn)]
    mer <- if (length(namn) > 5) paste0(" och ", length(namn) - 5, " till")
    ut <- c(ut, paste0(st$variables$label[match(k, st$variables$code)], ": ",
                       paste(utils::head(namn, 5), collapse = ", "), mer))
  }
  ut
}

# Filter till OECDdata. Tiden anges där som ett intervall och med en sorts period åt gången,
# så varje sort hämtas för sig och raderna väljs efteråt. Senaste perioden tas från tabellens
# perioder, en per sort.
es_oecd_fetch <- function(id, filters, st, lang = "en") {
  perioder <- filters$time
  n_senaste <- filters$lastTimePeriod
  if (!is.null(n_senaste)) {
    alla <- st$values$time$code
    perioder <- unlist(lapply(split(alla, es_time_group(alla)), utils::tail, n_senaste), use.names = FALSE)
    if ("FREQ" %in% names(filters)) {
      perioder <- perioder[es_time_group(perioder) %in% es_freq_groups(filters$FREQ)]
    }
  }
  f <- filters[setdiff(names(filters), c("time", es_time_params))]
  # långa listor hämtas som alla värden och väljs ur svaret, i en enda hämtning
  langa <- f[vapply(f, es_oecd_too_long, TRUE)]
  f <- f[setdiff(names(f), names(langa))]
  fran <- filters$sinceTimePeriod
  till <- filters$untilTimePeriod
  hamta <- function(tid) {
    g <- f
    if (length(tid) > 0) g$TIME_PERIOD <- tid
    if (length(g) == 0) g <- NULL
    # API v1: med v2, som är standard i OECDdata 0.3.4, gav intervallet 2024-01 till 2024-03
    # bara januari, utan något fel
    d <- tryCatch(
      es_oecd_import(id, g, lang),
      error = function(e) {
        # ett urval utan data ger status 404 med NoRecordsFound
        if (!grepl("NoRecordsFound", conditionMessage(e), fixed = TRUE)) stop(e)
        data.frame()
      }
    )
    es_oecd_clean(d)
  }
  d <- if (length(perioder) > 0) {
    dplyr::bind_rows(lapply(split(perioder, es_time_group(perioder)), function(p) hamta(range(p))))
  } else {
    hamta(c(fran, till))
  }
  if (length(perioder) > 0) d <- d[d$time %in% perioder, , drop = FALSE]
  for (n in names(langa)) d <- d[d[[n]] %in% langa[[n]], , drop = FALSE]
  # svaret kommer i slumpvis ordning
  d[do.call(order, unname(as.list(d[intersect(st$variables$code, names(d))]))), , drop = FALSE]
}

# En hämtning. Status 502, 503 och 504 är oftast tillfälliga och provas en gång till.
es_oecd_import <- function(id, filters, lang, paus = 2) {
  hamta <- function() {
    es_with_oecd(lang, as.data.frame(OECDdata::importData(id, filters = filters), stringsAsFactors = FALSE),
                 api_version = "v1")
  }
  tryCatch(hamta(), error = function(e) {
    if (!es_is_server_error(e)) stop(e)
    Sys.sleep(paus)
    hamta()
  })
}

es_freq_groups <- function(freq) {
  c(A = "0000", Q = "0000-Q0", M = "0000-00")[intersect(freq, c("A", "Q", "M"))]
}

# Kolumnerna som för Eurostat: time och values. Flaggan heter flagga. Övriga attribut,
# t.ex. UNIT_MULT, är kvar eftersom de behövs för att tolka värdet.
es_oecd_clean <- function(d) {
  if (nrow(d) == 0 && ncol(d) == 0) return(data.frame(time = character(), values = numeric()))
  for (n in names(d)) if (is.factor(d[[n]])) d[[n]] <- as.character(d[[n]])
  names(d)[names(d) == "TIME_PERIOD"] <- "time"
  names(d)[names(d) == "value_"] <- "values"
  names(d)[names(d) == "flags_"] <- "flagga"
  d$time <- as.character(d$time)
  d[, !names(d) %in% c("OBS_STATUS", "DECIMALS"), drop = FALSE]
}
