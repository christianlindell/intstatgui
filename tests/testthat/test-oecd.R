# OECD utan nätverk: svaren har samma form som från OECDdata och OECD:s API
mock_oecd_flow <- function() {
  list(agency = "OECD.SDD.TPS", id = "DSD_LFS@DF_X", version = "1.0", name = "Unemployment",
       description = "<p>Rate of <b>unemployment</b>.</p>", code = "OECD.SDD.TPS,DSD_LFS@DF_X,1.0")
}

mock_oecd_struct <- function() {
  lab <- function(k, l) data.frame(code = k, label = l, stringsAsFactors = FALSE)
  list(
    flow = mock_oecd_flow(),
    dimensions = data.frame(id = c("REF_AREA", "SEX", "FREQ", "TIME_PERIOD"),
                            label = c("Reference area", "Sex", "Frequency", "Time period"),
                            kind = c("Dimension", "Dimension", "Dimension", "TimeDimension")),
    labels = list(REF_AREA = lab(c("AUS", "NOR", "SWE"), c("Australia", "Norway", "Sweden")),
                  SEX = lab(c("_T", "F", "M"), c("Total", "Female", "Male")),
                  FREQ = lab(c("A", "M"), c("Annual", "Monthly"))),
    available = list(REF_AREA = c("SWE", "NOR"), SEX = "_T", FREQ = c("M", "A"))
  )
}

mock_oecd_st <- function() {
  es_parse_oecd_structure(mock_oecd_struct(), c("2023-11-01", "2024-02-29"), "DSD_LFS@DF_X", "en")
}

test_that("strukturen får samma form som för Eurostat", {
  st <- mock_oecd_st()
  expect_equal(st$title, "Unemployment")
  expect_equal(st$variables$code, c("REF_AREA", "SEX", "FREQ", "time"))
  expect_equal(st$variables$role, c("geo", NA, NA, "time"))
  # bara tabellens egna värden, i kodlistans ordning
  expect_equal(st$values$REF_AREA$code, c("NOR", "SWE"))
  expect_equal(st$values$REF_AREA$label, c("Norway", "Sweden"))
  expect_equal(st$variables$size[1:2], c(2, 1))
  # en tom lista betyder alla möjliga värden
  expect_true(all(st$variables$elimination))
  # månader först, sedan år, så att senaste året blir förvalt
  expect_equal(st$values$time$code, c("2023-11", "2023-12", "2024-01", "2024-02", "2023", "2024"))
  expect_equal(es_default_selection("time", st$values$time$code), "2024")
})

test_that("okänd tid gör att tiden kan utelämnas", {
  st <- es_parse_oecd_structure(mock_oecd_struct(), NULL, "x")
  expect_length(st$values$time$code, 0)
  expect_true(st$variables$elimination[st$variables$code == "time"])
})

test_that("perioderna räknas fram per frekvens", {
  r <- c("2023-11-15", "2024-04-30")
  expect_equal(es_oecd_periods(r, "A"), c("2023", "2024"))
  expect_equal(es_oecd_periods(r, "Q"), c("2023-Q4", "2024-Q1", "2024-Q2"))
  expect_equal(es_oecd_periods(c("2024-01-01", "2024-02-29"), c("M", "Q", "A")),
               c("2024-01", "2024-02", "2024-Q1", "2024"))
  expect_length(es_oecd_periods(NULL), 0)
  expect_length(es_oecd_periods(r, "W"), 0)
})

test_that("tidsintervallet läses ur OECD:s svar", {
  x <- xml2::read_xml('<m xmlns:c="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common"><c:KeyValue id="TIME_PERIOD"><c:TimeRange><c:StartPeriod>1953-01-01T00:00:00</c:StartPeriod><c:EndPeriod>2026-09-30T23:59:59</c:EndPeriod></c:TimeRange></c:KeyValue></m>')
  expect_equal(es_parse_oecd_time(x), c("1953-01-01", "2026-09-30"))
  expect_null(es_parse_oecd_time(xml2::read_xml("<m/>")))
})

test_that("tabellistan och ämnesträdet byggs av OECD:s svar", {
  ns <- 'xmlns:s="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure" xmlns:c="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common"'
  raw <- list(
    list = data.frame(Dataflow = c("A@1", "B@2", "C@3"), `Dataset name` = c("Jobs", "Prices", "Other"),
                      check.names = FALSE),
    scheme = xml2::read_xml(paste0('<m ', ns, '><s:CategoryScheme id="OECDCS1"><s:Category id="ECO"><c:Name xml:lang="en">Economy</c:Name><c:Name xml:lang="fr">Economie</c:Name><s:Category id="ECO_P"><c:Name xml:lang="en">Prices</c:Name></s:Category></s:Category></s:CategoryScheme><s:CategoryScheme id="OECDCS1_CLOUD"><s:Category id="X"><c:Name xml:lang="en">Cloud</c:Name></s:Category></s:CategoryScheme></m>')),
    categorisation = xml2::read_xml(paste0('<m ', ns, '><s:Categorisation><s:Source><Ref id="A@1"/></s:Source><s:Target><Ref id="ECO"/></s:Target></s:Categorisation><s:Categorisation><s:Source><Ref id="B@2"/></s:Source><s:Target><Ref id="ECO.ECO_P"/></s:Target></s:Categorisation><s:Categorisation><s:Source><Ref id="ZZ"/></s:Source><s:Target><Ref id="ECO"/></s:Target></s:Categorisation></m>')),
    lang = "fr"
  )
  k <- es_parse_oecd_toc(raw)
  expect_setequal(k$tables$id, c("A@1", "B@2", "C@3"))
  t <- es_build_tree(k)
  expect_equal(t$n, 3)
  expect_equal(vapply(t$children, `[[`, "", "label"), c("Economie", "(Utan ämnesområde)"))
  expect_equal(t$children[[1]]$children[[1]]$tables, "B@2")
  expect_equal(t$children[[1]]$tables, "A@1")
  expect_equal(t$children[[2]]$tables, "C@3")
  # samma kod i html:en som för Eurostat, utan period
  expect_no_error(es_tree_html(t, k$tables))
})

test_that("uppgifterna tar bort html och länkar till Data Explorer", {
  k <- es_structure_comments(mock_oecd_st())
  expect_equal(k$text[k$typ == "Beskrivning"], "Rate of unemployment .")
  expect_match(k$url[k$typ == "OECD Data Explorer"], "df[id]=DSD_LFS%40DF_X&df[vs]=1.0", fixed = TRUE)
})

# OECDdata::importData() som svarar med tre rader och sparar frågorna
local_fake_oecd <- function(env = parent.frame()) {
  anrop <- new.env()
  anrop$filters <- list()
  fake <- function(id, filters = NULL) {
    anrop$filters <- c(anrop$filters, list(filters))
    anrop$api <- getOption("OECDdata.api_version")
    data.frame(REF_AREA = factor(c("SWE", "SWE", "NOR")), SEX = "_T", FREQ = "M",
               TIME_PERIOD = c("2023-12", "2024-01", "2024-01"), value_ = c(8, 9, NA),
               flags_ = "A", UNIT_MULT = 0, OBS_STATUS = "A", DECIMALS = 1)
  }
  testthat::skip_if_not_installed("OECDdata")
  local_mocked_bindings(importData = fake, .package = "OECDdata", .env = env)
  local_mocked_bindings(es_get_structure = function(id, lang = "en", source = "eurostat") mock_oecd_st(), .env = env)
  anrop
}

test_that("es_get_data hämtar från OECD med tiden som intervall och väljer perioderna", {
  anrop <- local_fake_oecd()
  d <- es_get_data("DSD_LFS@DF_X", filters = list(REF_AREA = "SWE", time = c("2023-11", "2024-01")),
                   code_variables = "REF_AREA", source = "oecd")
  expect_length(anrop$filters, 1)
  # API v2 ger för få rader med ett tidsintervall
  expect_equal(anrop$api, "v1")
  expect_equal(anrop$filters[[1]]$REF_AREA, "SWE")
  expect_equal(anrop$filters[[1]]$TIME_PERIOD, c("2023-11", "2024-01"))
  expect_named(d, c("REF_AREA_code", "REF_AREA", "SEX", "FREQ", "time", "values", "flagga", "UNIT_MULT"))
  expect_equal(d$REF_AREA[1], "Sweden")
  # 2023-12 finns i svaret men är inte vald
  expect_equal(d$time, "2024-01")
  expect_equal(d$values, 9)
})

test_that("olika sorters perioder blir en hämtning per sort", {
  anrop <- local_fake_oecd()
  es_get_data("DSD_LFS@DF_X", filters = list(time = c("2024", "2024-01")), source = "oecd")
  expect_length(anrop$filters, 2)
})

test_that("senaste perioden blir tabellens senaste, för vald frekvens", {
  anrop <- local_fake_oecd()
  es_get_data("DSD_LFS@DF_X", filters = list(FREQ = "M", lastTimePeriod = 1), source = "oecd")
  expect_length(anrop$filters, 1)
  expect_equal(anrop$filters[[1]]$TIME_PERIOD, c("2024-02", "2024-02"))
  expect_equal(anrop$filters[[1]]$FREQ, "M")
})

test_that("alla perioder och inga filter ger ett anrop utan filter", {
  anrop <- local_fake_oecd()
  d <- es_get_data("DSD_LFS@DF_X", source = "oecd")
  expect_null(anrop$filters[[1]])
  expect_equal(nrow(d), 2)
})

test_that("ett urval utan data ger en tom tabell och ett meddelande", {
  skip_if_not_installed("OECDdata")
  local_mocked_bindings(
    importData = function(id, filters = NULL) stop("OECD HTTP 404: no observations match.\nResponse: NoRecordsFound"),
    .package = "OECDdata")
  local_mocked_bindings(es_get_structure = function(id, lang = "en", source = "eurostat") mock_oecd_st())
  expect_message(d <- es_get_data("x", filters = list(REF_AREA = "SWE", time = "2024-01"), source = "oecd"),
                 "Urvalet gav inga värden")
  expect_equal(nrow(d), 0)

  # andra fel går vidare
  local_mocked_bindings(importData = function(id, filters = NULL) stop("OECD HTTP 404: Dataset not found"),
                        .package = "OECDdata")
  expect_error(es_get_data("x", filters = list(REF_AREA = "SWE"), source = "oecd"), "Dataset not found")
})

test_that("ett tillfälligt serverfel provas en gång till, men inte en spärr", {
  skip_if_not_installed("OECDdata")
  n <- 0
  svar <- "OECD HTTP 502: 502 Bad Gateway"
  local_mocked_bindings(importData = function(id, filters = NULL) {
    n <<- n + 1
    if (n == 1 || is.null(svar)) stop(if (is.null(svar)) "OECD HTTP 503" else svar)
    data.frame(a = 1)
  }, .package = "OECDdata")
  expect_equal(es_oecd_import("x", NULL, "en", paus = 0)$a, 1)
  expect_equal(n, 2)

  # andra försöket misslyckas också
  n <- 0
  svar <- NULL
  expect_error(es_oecd_import("x", NULL, "en", paus = 0), "503")
  expect_equal(n, 2)

  # vid en spärr görs inget nytt försök
  n <- 0
  svar <- "OECD HTTP 429: OECD rate limit reached."
  expect_error(es_oecd_import("x", NULL, "en", paus = 0), "429")
  expect_equal(n, 1)

  # felet förklaras på svenska
  expect_match(es_error_text(simpleError("OECD HTTP 502: bad")), "svarade inte just nu")
})

test_that("tomt svar från OECD ger en tom tabell", {
  expect_equal(nrow(es_oecd_clean(data.frame())), 0)
  expect_named(es_oecd_clean(data.frame()), c("time", "values"))
})

test_that("språk och datakälla kontrolleras", {
  expect_error(es_get_data("x", source = "scb"), "source ska vara")
  expect_error(es_get_data("x", lang = "de", source = "oecd"), "lang ska vara")
  expect_equal(es_check_lang("DE", "eurostat"), "de")
})

test_that("koden anger OECD och kräver det här paketet", {
  kod <- es_query_code("DSD_LFS@DF_X", "T", list(REF_AREA = "SWE"), direct = TRUE, source = "oecd")
  expect_match(kod, "library(intstatgui)", fixed = TRUE)
  expect_match(kod, 'source = "oecd"', fixed = TRUE)
  expect_no_error(parse(text = kod))
  expect_false(grepl("source =", es_query_code("x", "T", NULL), fixed = TRUE))
})

test_that("cachefilen skiljer på datakälla, och Eurostat har kvar sitt namn", {
  old <- options(intstatgui.cache_file = "c:/x/y.rds")
  on.exit(options(old))
  expect_equal(es_cache_file("en", "oecd"), "c:/x/y_oecd_en.rds")
  expect_equal(es_cache_file("en"), "c:/x/y_en.rds")
})

test_that("tider med blandade sorter görs om var för sig", {
  expect_equal(es_convert_time(c("2024-Q2", "2024"), "date"), as.Date(c("2024-04-01", "2024-01-01")))
  expect_length(es_convert_time(character(), "date"), 0)
})

test_that("gränssnittet byter datakälla, språk och tabell", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(kalla = "eurostat", sprak = "de")
    session$flushReact()
    expect_equal(site()$source, "eurostat")
    session$setInputs(vald_tabell = "demo_gind")
    expect_false(is.null(tabell()))

    # tyska finns inte hos OECD: inget nytt laddas förrän språket är bytt
    session$setInputs(kalla = "oecd")
    session$flushReact()
    expect_equal(tabell()$source, "eurostat")
    session$setInputs(sprak = "en")
    session$flushReact()
    expect_equal(site(), list(source = "oecd", lang = "en"))
    expect_null(tabell())

    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = "SE")
    expect_match(fraga()$code, 'source = "oecd"', fixed = TRUE)
  })
})

mock_avail_xml <- function(tom = FALSE) {
  ns <- 'xmlns:c="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/common"'
  xml2::read_xml(paste0(
    "<m ", ns, '><c:Annotation id="obs_count"><c:AnnotationTitle>', if (tom) 0 else 30,
    '</c:AnnotationTitle></c:Annotation><r><c:KeyValue id="REF_AREA"><c:Value>SWE</c:Value><c:Value>NOR</c:Value></c:KeyValue>',
    if (!tom) '<c:KeyValue id="SEX"><c:Value>_T</c:Value></c:KeyValue><c:KeyValue id="FREQ"><c:Value>A</c:Value></c:KeyValue>',
    '<c:KeyValue id="TIME_PERIOD"><c:TimeRange><c:StartPeriod>',
    if (tom) "9999-01-01T00:00:00" else "2023-01-01T00:00:00", "</c:StartPeriod><c:EndPeriod>",
    if (tom) "0001-12-31T23:59:59" else "2023-12-31T23:59:59", "</c:EndPeriod></c:TimeRange></c:KeyValue></r></m>"))
}

test_that("möjliga värden läses ur OECD:s svar", {
  a <- es_parse_oecd_available(mock_avail_xml())
  expect_equal(a$values$REF_AREA, c("SWE", "NOR"))
  expect_equal(a$obs, 30)
  expect_equal(a$time, c("2023-01-01", "2023-12-31"))
  # utan observationer är intervallet omvänt, och variabler saknas
  b <- es_parse_oecd_available(mock_avail_xml(tom = TRUE))
  expect_equal(b$obs, 0)
  expect_null(b$time)
  expect_null(b$values$SEX)
})

test_that("frågan om möjliga värden har variablerna i rätt ordning", {
  st <- mock_oecd_st()
  u <- es_oecd_available_url(st, list(SEX = "_T", REF_AREA = c("SWE", "NOR")))
  expect_match(u, "availableconstraint/OECD.SDD.TPS,DSD_LFS@DF_X,1.0/SWE+NOR._T./all?mode=available", fixed = TRUE)
  expect_match(es_oecd_available_url(st, list(time = c("2023-11", "2024-01"))),
               "/../all?mode=available&startPeriod=2023-11&endPeriod=2024-01", fixed = TRUE)
  expect_match(es_oecd_available_url(st, list(time = c("2023", "2024-01"))),
               "startPeriod=2023&endPeriod=2024", fixed = TRUE)
  # för lång lista: variabeln räknas som alla värden
  lang <- list(REF_AREA = paste0("K", 1:500))
  expect_match(es_oecd_available_url(st, lang), "/../all", fixed = TRUE)
})

test_that("meddelandet till listorna grånar rätt och väljer ensamma värden", {
  st <- mock_oecd_st()
  t <- c(list(id = st$id), st)
  a <- es_parse_oecd_available(mock_avail_xml())
  m <- es_available_message(t, a, function(i) paste0("v", i))
  expect_equal(vapply(m, `[[`, "", "id"), c("v1", "v2", "v3", "v4"))
  expect_equal(unlist(m[[1]]$koder), c("SWE", "NOR"))
  # tiden från intervallet och frekvensen
  expect_equal(unlist(m[[4]]$koder), "2023")
  expect_equal(m[[4]]$latest, "2023")
  expect_null(m[[1]]$latest)
  # saknade variabler berörs inte
  b <- es_parse_oecd_available(mock_avail_xml(tom = TRUE))
  expect_equal(vapply(es_available_message(t, b, function(i) paste0("v", i)), `[[`, "", "id"), "v1")
})

test_that("gränssnittet visar antalet värden och stoppar ett urval utan data", {
  mock_calls$data <- NULL
  tom <- FALSE
  anrop <- 0
  old <- options(intstatgui.cache_file = tempfile(fileext = ".rds"),
                 intstatgui.backend = mock_backend(available = function(st, filters) {
                   anrop <<- anrop + 1
                   es_parse_oecd_available(mock_avail_xml(tom = tom))
                 }))
  on.exit(options(old))
  meddelanden <- list()
  testServer(es_app_server(), {
    session$sendCustomMessage <- function(type, message) meddelanden[[type]] <<- message
    session$setInputs(kalla = "oecd", sprak = "en", flikar = "variabler")
    session$flushReact()
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = "SE")
    session$elapse(2000)
    session$flushReact()
    expect_equal(mojligt()$obs, 30)
    expect_gte(anrop, 1)
    expect_match(paste(as.character(output$cell_info), collapse = ""), "Antal värden med data enligt OECD: 30")
    session$setInputs(hamta_data = 1)
    expect_length(mock_calls$data, 1)

    # ett urval utan data går inte att hämta
    tom <<- TRUE
    session$setInputs(t1_varden_3 = "NO")
    session$elapse(2000)
    session$flushReact()
    expect_equal(mojligt()$obs, 0)
    expect_match(paste(as.character(output$cell_info), collapse = ""), "Urvalet saknar data")
    session$setInputs(hamta_data = 2)
    expect_length(mock_calls$data, 1)

    # samma urval igen hämtas från det sparade svaret
    n <- anrop
    tom <<- FALSE
    session$setInputs(t1_varden_3 = "SE")
    session$elapse(2000)
    session$flushReact()
    expect_equal(anrop, n)
  })
})

test_that("ett mycket långt urval avslutar inte appen", {
  # Många värden gav en för lång nyckel i cachen. Felet i en observer stängde appen, som i RStudio
  # såg ut som en krasch.
  mock_calls$data <- NULL
  mange <- paste0("REGION", 1:3000)
  st <- function(id, lang = "en") {
    s <- mock_structure(id, lang)
    i <- match("geo", s$variables$code)
    s$values$geo <- data.frame(code = mange, label = mange, stringsAsFactors = FALSE)
    s$variables$size[i] <- length(mange)
    s
  }
  old <- options(intstatgui.cache_file = tempfile(fileext = ".rds"),
                 intstatgui.backend = mock_backend(
                   structure = st,
                   available = function(st, filters) es_parse_oecd_available(mock_avail_xml())))
  on.exit(options(old))
  testServer(es_app_server(), {
    session$setInputs(kalla = "oecd", sprak = "en", flikar = "variabler")
    session$flushReact()
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = mange[1:2600])
    session$elapse(2000)
    session$flushReact()
    expect_false(is.null(mojligt()))
    # adressen räknar variabeln som alla värden, så OECD:s antal gäller inte urvalet
    expect_true(is.na(mojligt()$obs))
    expect_false(any(grepl("Antal värden med data", as.character(output$cell_info), fixed = TRUE)))
    session$setInputs(hamta_data = 1)
    expect_length(mock_calls$data[[1]]$filters$geo, 2600)
  })
})

test_that("långa listor filtreras efter hämtningen från OECD", {
  anrop <- local_fake_oecd()
  m <- c("SWE", paste0("X", 1:2000))
  stt <- mock_oecd_st()
  stt$values$REF_AREA <- data.frame(code = m, label = m, stringsAsFactors = FALSE)
  local_mocked_bindings(es_get_structure = function(id, lang = "en", source = "eurostat") stt)
  d <- es_get_data("x", filters = list(REF_AREA = m), source = "oecd", na_rm = FALSE)
  expect_null(anrop$filters[[1]]$REF_AREA)
  # NOR saknas i listan och väljs bort efter hämtningen
  expect_equal(unique(d$REF_AREA), "SWE")
})

test_that("frågor skickas bara när flik 2 är öppen", {
  anrop <- 0
  old <- options(intstatgui.cache_file = tempfile(fileext = ".rds"),
                 intstatgui.backend = mock_backend(available = function(st, filters) {
                   anrop <<- anrop + 1
                   es_parse_oecd_available(mock_avail_xml())
                 }))
  on.exit(options(old))
  testServer(es_app_server(), {
    session$setInputs(kalla = "oecd", sprak = "en", flikar = "tabell")
    session$flushReact()
    session$setInputs(vald_tabell = "demo_gind")
    session$elapse(3000)
    session$flushReact()
    expect_equal(anrop, 0)
    session$setInputs(flikar = "variabler")
    session$elapse(1000)
    session$flushReact()
    # väntar fortfarande
    expect_equal(anrop, 0)
    session$elapse(1000)
    session$flushReact()
    expect_equal(anrop, 1)
        # flik 3 frågar om antalet för den valda perioden, och bara en gång
    session$setInputs(flikar = "data")
    session$elapse(2000)
    session$flushReact()
    expect_equal(anrop, 2)
    expect_true(mojligt()$med_tid)
    session$setInputs(flikar = "variabler")
    session$elapse(2000)
    session$flushReact()
    session$setInputs(flikar = "data")
    session$elapse(2000)
    session$flushReact()
    expect_equal(anrop, 2)
  })
})

test_that("en spärr från OECD stoppar fler frågor och förklaras på svenska", {
  anrop <- 0
  old <- options(intstatgui.cache_file = tempfile(fileext = ".rds"),
                 intstatgui.backend = mock_backend(
                   available = function(st, filters) {
                     anrop <<- anrop + 1
                     stop("Eurostat svarade med status 429")
                   },
                   get_data = function(...) stop("OECD HTTP 429: OECD rate limit reached.")))
  on.exit(options(old))
  testServer(es_app_server(), {
    session$setInputs(kalla = "oecd", sprak = "en", flikar = "variabler")
    session$flushReact()
    session$setInputs(vald_tabell = "demo_gind")
    session$elapse(2000)
    session$flushReact()
    expect_equal(anrop, 1)
    expect_true(mojligt_sparr())
    expect_match(paste(as.character(output$variabel_rubrik), collapse = ""), "tillfälligt spärrat")

    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = "SE")
    session$elapse(2000)
    session$flushReact()
    expect_equal(anrop, 1)

    # hämtningen går att försöka med, och felet är begripligt
    session$setInputs(hamta_data = 1)
    expect_match(data_fel(), "tillfälligt spärrat")
  })
})

test_that("andra fel från OECD visas som de är, och stoppar inte frågorna", {
  expect_equal(es_error_text(simpleError("HTTP 400")), "HTTP 400")
  expect_match(es_error_text(simpleError("OECD HTTP 429")), "spärrat")
  expect_false(es_is_rate_limit(simpleError("HTTP 500")))
})

test_that("OECDdata försöker inte igen vid fel", {
  skip_if_not_installed("OECDdata")
  expect_equal(es_with_oecd("en", getOption("OECDdata.retries")), 0)
})

test_that("Not applicable tas med när ett annat värde är valt", {
  vals <- list(PRICES = data.frame(code = c("Q", "V", "_Z")), MEASURE = data.frame(code = c("GDP", "POP")),
               time = data.frame(code = "2024"))
  expect_equal(es_add_not_applicable(list(PRICES = "Q", MEASURE = "GDP", time = "2024"), vals),
               list(PRICES = c("Q", "_Z"), MEASURE = "GDP", time = "2024"))
  # redan valt, ingen sådan kod eller inget filter
  expect_equal(es_add_not_applicable(list(PRICES = "_Z"), vals), list(PRICES = "_Z"))
  expect_equal(es_add_not_applicable(list(MEASURE = "GDP"), vals), list(MEASURE = "GDP"))
  expect_null(es_add_not_applicable(NULL, vals))
  # koden syns inte i beskedet om värden utan rader
  st <- mock_oecd_st()
  st$values$SEX <- data.frame(code = c("_T", "F", "_Z"), label = c("Total", "Female", "NA"))
  d <- data.frame(SEX = "_T", values = 1)
  expect_length(es_missing_values(d, list(SEX = c("_T", "_Z")), st), 0)
})

test_that("valda värden utan rader rapporteras", {
  st <- mock_oecd_st()
  d <- data.frame(REF_AREA = "SWE", SEX = "_T", time = "2024", values = 1)
  expect_length(es_missing_values(d, list(REF_AREA = "SWE"), st), 0)
  expect_equal(es_missing_values(d, list(REF_AREA = c("SWE", "NOR"), time = c("2023", "2024")), st),
               "Reference area: Norway")
  m <- paste0("K", 1:8)
  stt <- st
  stt$values$REF_AREA <- data.frame(code = m, label = m)
  expect_match(es_missing_values(d, list(REF_AREA = m), stt), "K5 och 3 till", fixed = TRUE)
})

test_that("es_get_data säger till när ett valt värde inte gav någon rad", {
  skip_if_not_installed("OECDdata")
  local_mocked_bindings(
    importData = function(id, filters = NULL) {
      data.frame(REF_AREA = "SWE", SEX = "_T", FREQ = "M", TIME_PERIOD = "2024-01", value_ = 1, flags_ = "A")
    }, .package = "OECDdata")
  local_mocked_bindings(es_get_structure = function(id, lang = "en", source = "eurostat") mock_oecd_st())
  expect_message(d <- es_get_data("x", filters = list(REF_AREA = c("SWE", "NOR")), source = "oecd"),
                 "Reference area: Norway")
  expect_equal(attr(d, "saknas"), "Reference area: Norway")
})

test_that("hos OECD behöver inget väljas, och variabler med ett värde visas inte", {
  mock_calls$data <- NULL
  st <- function(id, lang = "en") es_parse_oecd_structure(mock_oecd_struct(), c("2023-11-01", "2024-02-29"), id, lang)
  old <- options(intstatgui.cache_file = tempfile(fileext = ".rds"),
                 intstatgui.backend = mock_backend(structure = st))
  on.exit(options(old))
  testServer(es_app_server(), {
    session$setInputs(kalla = "oecd", sprak = "en", flikar = "variabler")
    session$flushReact()
    session$setInputs(vald_tabell = "x")
    q <- fraga()
    expect_length(q$problems, 0)
    expect_equal(q$filters, list(time = "2024"))
    html <- paste(as.character(output$variabler), collapse = "")
    expect_match(html, "Reference area", fixed = TRUE)
    # SEX har ett enda möjligt värde
    expect_false(grepl(">Sex<", html, fixed = TRUE))
    expect_false(grepl("Obligatoriskt", html, fixed = TRUE))
    # även tiden kan vara tom
    session$setInputs(t1_varden_4 = character(), t1_varden_4_andrad = TRUE)
    expect_null(fraga()$filters)
    expect_length(fraga()$problems, 0)
  })
})

test_that("Eurostat frågar inte efter möjliga värden", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(kalla = "eurostat", sprak = "en")
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = "SE")
    session$elapse(2000)
    session$flushReact()
    expect_null(mojligt())
  })
})

test_that("regionnivåer finns bara för Eurostat", {
  skip_if_not_installed("OECDdata")
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(kalla = "oecd", sprak = "en")
    session$flushReact()
    session$setInputs(vald_tabell = "demo_gind")
    expect_false(any(grepl("es-indelning", as.character(output$variabler), fixed = TRUE)))
  })
})
