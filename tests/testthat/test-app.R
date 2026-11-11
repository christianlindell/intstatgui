test_that("gränssnittet byggs, med addin-knapparna bara som addin", {
  old <- options(intstatgui.backend = mock_backend())
  on.exit(options(old))
  html <- as.character(es_app_ui(addin = TRUE))
  expect_match(html, "infoga_kod")
  expect_match(html, 'id="stang"')
  expect_false(grepl("infoga_kod", as.character(es_app_ui(addin = FALSE))))
})

test_that("rutan för bara dataset är förvald", {
  html <- paste(as.character(es_app_ui()), collapse = "")
  expect_match(html, '<input id="bara_dataset" type="checkbox"[^>]*checked')
})

test_that("bara en tabell går att välja i trädet", {
  expect_false(grepl("ctrlKey", es_tree_js, fixed = TRUE))
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(sprak = "en")
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(t1_varden_3 = "SE")
    # samma tabell igen behåller valen
    session$setInputs(vald_tabell = "demo_gind")
    expect_equal(tabell()$prefix, "t1_")
    session$setInputs(vald_tabell = "tps00001")
    expect_equal(tabell()$id, "tps00001")
    expect_equal(tabell()$prefix, "t2_")
  })
})

test_that("rutorna för antal ritas utan fel innan en tabell är vald", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(sprak = "en")
    expect_error(output$cell_info, class = "shiny.silent.error")
    expect_no_error(output$mojligt_antal)
  })
})

test_that("addins.dcf pekar på en exporterad funktion", {
  dcf <- read.dcf(system.file("rstudio", "addins.dcf", package = "intstatgui"))
  expect_equal(unname(dcf[, "Binding"]), "es_gui_addin")
  expect_true("es_gui_addin" %in% getNamespaceExports("intstatgui"))
})

test_that("JavaScript och css har de nya prefixen", {
  expect_false(any(grepl("\\bpx[A-Z_-]", c(es_tree_js, es_var_js, es_tree_css, es_var_css))))
})

test_that("språket ger en katalog, och ett val av tabell ger kod och data", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(sprak = "de", sok = "", bara_dataset = FALSE)
    session$flushReact()
    expect_equal(katalog()$lang, "de")
    expect_equal(trad()$n, 3)

    session$setInputs(vald_tabell = "demo_gind")
    expect_equal(tabell()$id, "demo_gind")
    # förval: en enda frekvens och senaste perioden, men geo och indikator saknas
    q <- fraga()
    expect_setequal(q$problems, c("Välj minst ett värde för Indicator.", "Välj minst ett värde för Geo."))

    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = c("SE", "NO"), t1_varden_4 = "2024")
    q <- fraga()
    expect_length(q$problems, 0)
    expect_equal(q$cells, 2)
    expect_match(q$code, 'geo = c("SE", "NO")', fixed = TRUE)
    expect_match(q$code, 'lang = "de"', fixed = TRUE)

    session$setInputs(t1_kodvariabler_0 = "geo", tidsformat = "date", clean_names = FALSE)
    expect_match(fraga()$code, 'code_variables = "geo"', fixed = TRUE)
    expect_match(fraga()$code, 'time_format = "date"', fixed = TRUE)

    session$setInputs(hamta_data = 1)
    expect_equal(mock_calls$data[[1]]$filters$geo, c("SE", "NO"))
    expect_equal(mock_calls$data[[1]]$lang, "de")
    # kodkolumnerna styrs av rutorna, utan ny hämtning
    expect_named(data_visad(), c("geo_code", "geo", "freq", "time", "values"))
    session$setInputs(t1_kodvariabler_0 = character())
    expect_named(data_visad(), c("geo", "freq", "time", "values"))
    expect_length(mock_calls$data, 1)

    # ändrade värden gör data inaktuellt
    session$setInputs(t1_varden_3 = "SE")
    session$flushReact()
    expect_null(data_resultat())
    expect_true(data_inaktuell())
  })
})

test_that("sökningen filtrerar trädet", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(sprak = "en", sok = "gdp", bara_dataset = FALSE)
    session$elapse(500)
    expect_equal(trad()$n, 1)
    session$setInputs(sok = "", bara_dataset = TRUE)
    session$elapse(500)
    expect_equal(trad()$n, 2)
  })
})

test_that("språkbyte nollställer vald tabell", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(sprak = "en")
    session$setInputs(vald_tabell = "demo_gind")
    expect_false(is.null(tabell()))
    session$setInputs(sprak = "fr")
    session$flushReact()
    expect_null(tabell())
    expect_equal(katalog()$lang, "fr")
  })
})

test_that("regionnivå begränsar värdena och ger en uttrycklig lista", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(sprak = "en")
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(es_indelning = list(target = "t1_varden_3", codelist = "land"))
    expect_equal(indelning()$geo$values$code, c("SE", "NO"))
    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = c("SE", "NO"))
    expect_equal(fraga()$query$geo, c("SE", "NO"))
    session$setInputs(es_indelning = list(target = "t1_varden_3", codelist = ""))
    expect_length(indelning(), 0)
  })
})

test_that("fel vid hämtning visas och tabellvalet är kvar", {
  mock_calls$data <- NULL
  old <- options(intstatgui.cache_file = tempfile(fileext = ".rds"),
                 intstatgui.backend = mock_backend(
                   get_data = function(...) stop("Eurostat svarar inte"),
                   structure = function(id, lang = "en") {
                     if (id == "trasig") stop("finns inte")
                     mock_structure(id, lang)
                   }))
  on.exit(options(old))
  testServer(es_app_server(), {
    session$setInputs(sprak = "en")
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(vald_tabell = "trasig")
    expect_equal(tabell()$id, "demo_gind")
    session$setInputs(t1_varden_2 = "JAN", t1_varden_3 = "SE")
    session$setInputs(hamta_data = 1)
    expect_equal(data_fel(), "Eurostat svarar inte")
    expect_null(data_resultat())
  })
})

test_that("informationen hämtas per tabell och raderas när valet ändras", {
  local_mock_app()
  testServer(es_app_server(), {
    session$setInputs(sprak = "en")
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(hamta_kommentarer = 1)
    k <- kommentar_innehall()
    expect_equal(k$id, "demo_gind")
    expect_equal(k$res$rows$text[1], "2026-10-27")
    session$setInputs(vald_tabell = "tps00001")
    session$flushReact()
    expect_null(kommentar_innehall())
  })
})

test_that("informationsrutan länkar bara till http(s) och visar fel", {
  html <- as.character(es_comment_card("x", "T", list(rows = data.frame(
    typ = c("A", "B", "C"), text = c("t", "l", "j"),
    url = c(NA, "https://a.se/", "javascript:alert(1)"), stringsAsFactors = FALSE))))
  expect_match(html, 'href="https://a.se/"', fixed = TRUE)
  expect_false(grepl("javascript:", html, fixed = TRUE))
  expect_match(as.character(es_comment_card("x", "T", list(error = "fel"))), "Kunde inte hämta")
})

test_that("addin: 'Infoga koden' skriver den genererade koden och appen stoppas en gång", {
  local_mock_app()
  inserted <- NULL
  stopp <- 0
  local_mocked_bindings(es_insert_code = function(code) inserted <<- code,
                        es_stop_app = function(...) stopp <<- stopp + 1)
  testServer(es_app_server(addin = TRUE), {
    session$setInputs(sprak = "en")
    session$setInputs(infoga_kod = 1)
    expect_null(inserted)
    session$setInputs(vald_tabell = "demo_gind")
    session$setInputs(t1_varden_3 = "SE")
    session$setInputs(infoga_kod = 2)
    expect_match(inserted, 'geo = "SE"', fixed = TRUE)
    session$setInputs(stang = 1)
  })
  expect_equal(stopp, 1)
})

test_that("Positron känns igen", {
  gamla <- Sys.getenv(c("POSITRON", "POSITRON_VERSION"), unset = NA)
  on.exit(for (n in names(gamla)) {
    if (is.na(gamla[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(gamla[[n]]), n))
  })
  Sys.setenv(POSITRON = "1")
  expect_true(es_is_positron())
  Sys.setenv(POSITRON = "", POSITRON_VERSION = "")
  expect_false(es_is_positron())
})
