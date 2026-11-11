st <- mock_structure("demo_gind")

test_that("valen blir fråga och filter", {
  s <- list(freq = list(values = "A"), indic_de = list(values = "JAN"),
            geo = list(values = c("NO", "SE")), time = list(latest = TRUE))
  q <- es_build_query(st$variables, st$values, s)
  expect_length(q$problems, 0)
  # listans ordning, inte valordningen
  expect_equal(q$query$geo, c("SE", "NO"))
  # en variabel med ett enda värde är "alla värden" och behöver inget filter
  expect_equal(q$query$freq, "*")
  expect_equal(es_query_filters(q$query),
               list(indic_de = "JAN", geo = c("SE", "NO"), lastTimePeriod = 1))
})

test_that("alla värden utelämnas och en tom lista är ett problem", {
  s <- list(freq = list(values = "A"), indic_de = list(values = c("JAN", "GBIRTH")),
            geo = list(), time = list(values = st$values$time$code))
  q <- es_build_query(st$variables, st$values, s)
  expect_equal(q$problems, "Välj minst ett värde för Geo.")
  expect_null(es_query_filters(q$query))
})

test_that("en vald regionnivå ger en uttrycklig lista", {
  s <- list(freq = list(values = "A"), indic_de = list(values = "JAN"),
            geo = list(values = c("SE", "NO"), indelad = TRUE), time = list(latest = TRUE))
  vals <- st$values
  vals$geo <- vals$geo[vals$geo$code %in% c("SE", "NO"), ]
  expect_equal(es_build_query(st$variables, vals, s)$query$geo, c("SE", "NO"))
})

test_that("förvalen är senaste perioden och variabler med ett värde", {
  expect_equal(es_default_selection("time", c("2023", "2024")), "2024")
  expect_equal(es_default_selection(NA, "A"), "A")
  expect_length(es_default_selection(NA, c("A", "Q")), 0)
  expect_length(es_default_selection(NA, character()), 0)
})

test_that("koden skrivs med det som valts", {
  kod <- es_query_code("demo_gind", "Titel", list(geo = c("SE", "NO"), lastTimePeriod = 1),
                       code_variables = "geo", lang = "de", time_format = "date")
  expect_match(kod, "library(intstatgui)", fixed = TRUE)
  expect_match(kod, "# Titel", fixed = TRUE)
  expect_match(kod, 'id = "demo_gind"', fixed = TRUE)
  expect_match(kod, 'geo = c("SE", "NO")', fixed = TRUE)
  expect_match(kod, "lastTimePeriod = 1", fixed = TRUE)
  expect_match(kod, 'code_variables = "geo"', fixed = TRUE)
  expect_match(kod, 'lang = "de"', fixed = TRUE)
  expect_match(kod, 'time_format = "date"', fixed = TRUE)

  ingen <- es_query_code("x", "T", NULL, clean_names = TRUE)
  expect_match(ingen, "filters = NULL", fixed = TRUE)
  expect_match(ingen, "janitor::clean_names(dataset_df)", fixed = TRUE)

  expect_no_error(parse(text = kod))
  expect_no_error(parse(text = ingen))
})

test_that("koden kan bara kräva paketet eurostat när det går", {
  d <- es_query_code("x", "T", list(geo = "SE"), direct = TRUE)
  expect_match(d, "library(eurostat)\n", fixed = TRUE)
  expect_match(d, "get_eurostat(", fixed = TRUE)
  expect_match(d, 'type = "label"', fixed = TRUE)
  expect_match(d, "cache = FALSE", fixed = TRUE)
  expect_no_error(parse(text = d))

  expect_match(es_query_code("x", "T", NULL, code_variables = "geo", direct = TRUE), "es_get_data(", fixed = TRUE)
})

test_that("kodkolumner visas bara för valda variabler", {
  d <- data.frame(geo_code = "SE", geo = "S", sex_code = "T", sex = "Tot", values = 1)
  expect_named(es_keep_code_columns(d, "geo"), c("geo_code", "geo", "sex", "values"))
  expect_named(es_keep_code_columns(d, character()), c("geo", "sex", "values"))
})

test_that("etiketter och filnamn", {
  expect_equal(es_option_labels(data.frame(code = c("A", "2024"), label = c("Annual", "2024"))),
               c("Annual [A]", "2024"))
  expect_null(es_cell_warning(10))
  expect_match(es_cell_warning(2e6), "1 miljon")
})

test_that("export skriver txt", {
  f <- tempfile(fileext = ".txt")
  es_write_data(data.frame(a = 1), f, "txt")
  expect_equal(readLines(f), c("a", "1"))
  expect_null(es_export_missing("txt"))
})
