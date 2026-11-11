st <- mock_structure("demo_gind")

test_that("filtren anpassas till tabellen", {
  tf <- es_table_filters(list(geo = c("SE", "XX"), sex = "T", TIME_PERIOD = 2024, lastTimePeriod = 1),
                         st$variables, st$values)
  expect_equal(tf$filters$geo, "SE")
  expect_equal(tf$filters$time, "2024")
  expect_equal(tf$filters$lastTimePeriod, 1)
  expect_equal(tf$unknown, "sex")
  expect_length(tf$missing, 0)
  expect_equal(es_table_filters(list(geo = "XX"), st$variables, st$values)$missing, "Geo")
})

test_that("antalet celler räknas", {
  v <- st$variables
  expect_equal(es_cell_count(v, st$values, list()), 1 * 2 * 5 * 3)
  expect_equal(es_cell_count(v, st$values, list(geo = c("SE", "NO"), lastTimePeriod = 1)), 1 * 2 * 2 * 1)
  expect_equal(es_cell_count(v, st$values, list(sinceTimePeriod = "2023")), 1 * 2 * 5 * 2)
  expect_equal(es_cell_count(v, st$values, list(sinceTimePeriod = "2023", untilTimePeriod = "2023")), 10)
})

test_that("stora frågor delas och alla värden kommer med en gång", {
  v <- st$variables
  expect_length(es_split_filters(list(geo = "SE"), v, st$values), 1)
  expect_length(es_split_filters(list(), v, st$values), 1)

  delar <- es_split_filters(list(geo = c("SE", "NO", "SE1", "SE11")), v, st$values, max_chars = 20)
  expect_gt(length(delar), 1)
  expect_setequal(unlist(lapply(delar, `[[`, "geo")), c("SE", "NO", "SE1", "SE11"))

  # för många celler: en variabel utan filter skrivs ut och delas
  delar <- es_split_filters(list(indic_de = "JAN"), v, st$values, max_cells = 3)
  expect_setequal(unlist(lapply(delar, `[[`, "geo")), st$values$geo$code)
  expect_true(all(vapply(delar, function(f) es_cell_count(v, st$values, f) <= 3, TRUE)))

  # går inte att dela mer: variablerna har ett värde var
  expect_length(es_split_filters(list(geo = "SE", indic_de = "JAN", time = "2024"), v, st$values, max_chars = 1), 1)
})

test_that("rader väljs när hela tabellen har hämtats", {
  d <- data.frame(geo = c("SE", "SE", "NO"), time = c("2023", "2024", "2024"), values = 1:3)
  expect_equal(es_filter_rows(d, list(geo = "SE", lastTimePeriod = 1))$values, 2)
  expect_equal(es_filter_rows(d, list(geo = c("SE", "NO"), time = "2024"))$values, 2:3)
})

test_that("koder blir namn, med kodkolumn för valda variabler", {
  d <- data.frame(freq = "A", geo = c("SE", "ZZ"), time = "2024", values = 1:2, stringsAsFactors = FALSE)
  res <- es_label_data(d, st, code_variables = "geo")
  expect_named(res, c("freq", "geo_code", "geo", "time", "values"))
  expect_equal(res$geo, c("Sweden en", "ZZ"))
  expect_equal(res$geo_code, c("SE", "ZZ"))
  expect_equal(res$freq[1], "Annual")
  expect_identical(es_label_data(d, st, type = "code"), d)
})

test_that("tidsformaten fungerar och ett okänt stoppas", {
  expect_equal(es_convert_time("2023-Q2", "raw"), "2023-Q2")
  expect_equal(es_convert_time("2023-Q2", "date"), as.Date("2023-04-01"))
  expect_equal(es_convert_time("2023-Q2", "date_last"), as.Date("2023-06-30"))
  expect_equal(es_convert_time("2023-Q2", "num"), 2023.25)
  expect_error(es_convert_time("2023", "x"), "time_format")
})

# get_eurostat() som svarar med tre rader och sparar frågorna
local_fake_eurostat <- function(env = parent.frame()) {
  anrop <- new.env()
  anrop$filters <- list()
  fake <- function(id, filters = NULL, ...) {
    anrop$filters <- c(anrop$filters, list(filters))
    tibble::tibble(freq = "A", indic_de = "JAN", geo = c("SE", "NO", "SE"),
                   TIME_PERIOD = factor(c("2023", "2023", "2024")), values = c(1, NA, 3))
  }
  local_mocked_bindings(get_eurostat = fake, .package = "eurostat", .env = env)
  local_mocked_bindings(es_get_structure = mock_structure, .env = env)
  anrop
}

test_that("es_get_data ger namn, tid som text och tar bort rader utan värde", {
  anrop <- local_fake_eurostat()
  d <- es_get_data("demo_gind", filters = list(geo = c("SE", "NO"), time = 2023:2024), code_variables = "geo")
  expect_s3_class(d, "tbl_df")
  expect_named(d, c("freq", "indic_de", "geo_code", "geo", "time", "values"))
  expect_equal(d$geo, c("Sweden en", "Sweden en"))
  expect_equal(d$time, c("2023", "2024"))
  expect_equal(anrop$filters[[1]]$time, c("2023", "2024"))

  expect_equal(nrow(es_get_data("demo_gind", filters = list(geo = "SE"), na_rm = FALSE)), 3)
  expect_equal(es_get_data("demo_gind", filters = list(geo = "SE"), type = "code")$geo, c("SE", "SE"))
})

test_that("utan filter hämtas hela tabellen", {
  anrop <- local_fake_eurostat()
  es_get_data("demo_gind")
  expect_null(anrop$filters[[1]])
})

test_that("es_get_data stoppar för okända variabler och värden med en tabell", {
  local_fake_eurostat()
  expect_error(es_get_data("demo_gind", filters = list(sex = "T")), "Variablerna är")
  expect_error(es_get_data("demo_gind", filters = list(geo = "XX")), "inga giltiga värden")
})

test_that("es_get_data tar en enda tabell", {
  expect_error(es_get_data(c("a", "b")), "en enda tabell")
})

test_that("ett fördröjt svar från Eurostat ger hämtning av hela tabellen", {
  anrop <- new.env()
  anrop$filters <- list()
  fake <- function(id, filters = NULL, ...) {
    anrop$filters <- c(anrop$filters, list(filters))
    if (!is.null(filters)) stop("HTTP status: 413 ASYNCHRONOUS_RESPONSE")
    tibble::tibble(geo = c("SE", "NO"), TIME_PERIOD = c("2024", "2024"), values = 1:2)
  }
  local_mocked_bindings(get_eurostat = fake, .package = "eurostat")
  expect_message(d <- es_fetch_part("x", list(geo = "SE")), "Hämtar hela x")
  expect_equal(d$values, 1)
  expect_length(anrop$filters, 2)
  expect_null(anrop$filters[[2]])
})

test_that("ett litet urval utan data laddar inte ner hela tabellen", {
  anrop <- new.env()
  anrop$n <- 0
  fake <- function(id, filters = NULL, ...) {
    anrop$n <- anrop$n + 1
    stop("HTTP status: 200, but the dataset didn't have any values.")
  }
  local_mocked_bindings(get_eurostat = fake, .package = "eurostat")
  local_mocked_bindings(es_get_structure = mock_structure)
  expect_message(d <- es_get_data("demo_gind", filters = list(geo = "SE")), "Urvalet gav inga värden")
  expect_equal(nrow(d), 0)
  expect_equal(anrop$n, 1)

  # ett stort urval kan vara ett fördröjt svar: då provas hela tabellen
  expect_error(es_fetch_part("x", list(geo = "SE"), stor = TRUE), "didn't have any values")
  expect_equal(anrop$n, 3)
})

test_that("andra fel från Eurostat går vidare", {
  local_mocked_bindings(get_eurostat = function(...) stop("HTTP status: 400 bad"), .package = "eurostat")
  expect_error(es_fetch_part("x", list(geo = "SE")), "400 bad")
})
