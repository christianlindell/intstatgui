test_that("TOC blir tabeller, mappar och platser", {
  k <- es_parse_toc(mock_toc())
  expect_setequal(k$tables$id, c("demo_gind", "tps00001", "nama_gdp"))
  expect_equal(k$tables$first_period[k$tables$id == "demo_gind"], "2022")
  # en tabell under ett dataset hamnar i samma mapp som datasetet
  mapp <- function(namn) k$folders$key[k$folders$label == namn]
  expect_setequal(k$items$table_id[k$items$key == mapp("Population")], c("demo_gind", "tps00001"))
  # samma tabell kan stå i flera mappar
  expect_setequal(k$items$key[k$items$table_id == "demo_gind"], c(mapp("Population"), mapp("Economy")))
})

test_that("mappar utan namn blir ingen egen nivå", {
  rad <- function(title, code, type, h) {
    data.frame(title = title, code = code, type = type, last.update.of.data = " ",
               last.table.structure.change = " ", data.start = " ", data.end = " ",
               values = NA, hierarchy = h, stringsAsFactors = FALSE)
  }
  k <- es_parse_toc(rbind(
    rad("Employment", "emp", "folder", 0),
    rad("---", "emp_", "folder", 1),
    rad("Rates", "a", "dataset", 2),
    rad("Rate table", "a1", "table", 3),
    rad("___", "emp__", "folder", 1),
    rad("Sub", "sub", "folder", 2),
    rad("In sub", "b", "dataset", 3),
    rad("Direct", "c", "dataset", 1)
  ))
  expect_equal(k$folders$label, c("Employment", "Sub"))
  t <- es_build_tree(k)
  expect_equal(t$children[[1]]$label, "Employment")
  expect_equal(t$children[[1]]$tables, c("a", "a1", "c"))
  expect_equal(t$children[[1]]$children[[1]]$tables, "b")
  expect_equal(t$n, 4)
})

test_that("en cachefil från en äldre version används inte", {
  fil <- tempfile(fileext = ".rds")
  saveRDS(list(tables = "gammal"), fil)
  anrop <- 0
  b <- list(toc = function(lang) {
    anrop <<- anrop + 1
    mock_toc(lang)
  })
  k <- es_fetch_catalogue(b, cache_file = fil)
  expect_equal(anrop, 1)
  expect_equal(k$version, es_catalogue_version)
})

test_that("två mappar med samma kod hålls isär", {
  toc <- mock_toc()
  extra <- toc[c(1, 3), ]
  extra$title[1] <- "Cross cutting"
  extra$hierarchy[2] <- 1
  k <- es_parse_toc(rbind(toc, extra))
  t <- es_build_tree(k)
  expect_equal(vapply(t$children, `[[`, "", "label"), c("Themes en", "Cross cutting"))
  expect_equal(t$children[[2]]$tables, "demo_gind")
  expect_equal(t$children[[1]]$n, 3)
})

test_that("trädet tar bort tomma mappar och räknar varje tabell en gång", {
  k <- es_parse_toc(mock_toc())
  t <- es_build_tree(k)
  expect_equal(t$n, 3)
  etiketter <- vapply(t$children[[1]]$children, `[[`, "", "label")
  expect_equal(etiketter, c("Population", "Economy"))
  expect_equal(es_tree_stats(t)$folders, 3)

  bara <- es_build_tree(k, "nama_gdp")
  expect_equal(bara$n, 1)
  expect_length(bara$children[[1]]$children, 1)
})

test_that("sökningen kräver alla ord och typen går att välja", {
  t <- es_parse_toc(mock_toc())$tables
  expect_equal(es_filter_tables(t, "POPULATION january")$id, "tps00001")
  expect_equal(es_filter_tables(t, "tps00001")$label, "Population on 1 January")
  expect_setequal(es_filter_tables(t, "", only_datasets = TRUE)$id, c("demo_gind", "nama_gdp"))
})

test_that("trädet escapas och markerar valda tabeller", {
  k <- es_parse_toc(mock_toc())
  html <- as.character(es_tree_html(es_build_tree(k), k$tables, open = TRUE, selected = "nama_gdp"))
  expect_match(html, "GDP &amp; main components", fixed = TRUE)
  expect_match(html, 'class="es-table es-selected" data-id="nama_gdp"', fixed = TRUE)
  expect_match(html, "<details class=\"es-node\" open>", fixed = TRUE)
  expect_match(html, "2023-Q1–2023-Q2", fixed = TRUE)
})

test_that("katalogen sparas per språk och läses från cachen", {
  anrop <- 0
  b <- list(toc = function(lang) {
    anrop <<- anrop + 1
    mock_toc(lang)
  })
  fil <- tempfile(fileext = ".rds")
  k1 <- es_fetch_catalogue(b, lang = "de", cache_file = fil)
  k2 <- es_fetch_catalogue(b, lang = "de", cache_file = fil)
  expect_equal(anrop, 1)
  expect_equal(k2$folders$label[1], "Themes de")
  es_fetch_catalogue(b, lang = "de", force = TRUE, cache_file = fil)
  expect_equal(anrop, 2)

  old <- options(intstatgui.cache_file = "c:/x/y.rds")
  on.exit(options(old))
  expect_equal(es_cache_file("fr"), "c:/x/y_fr.rds")
})

test_that("es_search_tables ger svenska kolumnnamn", {
  old <- options(intstatgui.backend = mock_backend(),
                 intstatgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  res <- es_search_tables("gdp")
  expect_equal(res$id, "nama_gdp")
  expect_named(res, c("id", "namn", "typ", "forsta_period", "sista_period", "uppdaterad", "antal_obs"))
  expect_error(es_search_tables("gdp", lang = "sv"), "lang ska vara")
})
