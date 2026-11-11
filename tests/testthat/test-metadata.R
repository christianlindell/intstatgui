test_that("strukturen ger variabler och tabellens egna värden", {
  st <- es_parse_structure(mock_flow_xml(), mock_constraint_xml(), "x", "de")
  expect_equal(st$title, "Titel")
  # tiden sist och med namnet time, även om den står först i svaret
  expect_equal(st$variables$code, c("geo", "time"))
  expect_equal(st$variables$label[1], "Geo Einheit")
  # bara värden som finns i tabellen, och engelska när språket saknas
  expect_equal(st$values$geo$code, c("NO", "SE"))
  expect_equal(st$values$geo$label, c("Norway", "Schweden"))
  expect_equal(st$values$time$code, c("2023", "2024"))
  expect_equal(st$variables$size, c(2, 2))
  expect_false(any(st$variables$elimination))
})

test_that("utan contentconstraint används kodlistan och tiden är okänd", {
  st <- es_parse_structure(mock_flow_xml(), NULL, "x", "en")
  expect_equal(st$values$geo$code, c("SE", "NO", "DK"))
  expect_length(st$values$time$code, 0)
  expect_equal(st$variables$elimination, c(FALSE, TRUE))
})

test_that("uppgifterna om tabellen tolkas", {
  st <- es_parse_structure(mock_flow_xml(), mock_constraint_xml(), "x", "de")
  k <- es_structure_comments(st)
  expect_equal(k$text[k$typ == "Data uppdaterad"], "2026-10-22")
  expect_equal(k$url[k$typ == "Metadata (ESMS)"], "https://ec.europa.eu/m.htm")
  expect_equal(k$text[k$typ == "Källa"], "Eurostat")
  expect_equal(k$url[k$typ == "DOI"], "https://doi.org/10.2908/X")
  expect_match(k$url[k$typ == "Eurostat Data Browser"], "view/x/default/table?lang=de", fixed = TRUE)
})

test_that("regionkoder får rätt nivå", {
  expect_equal(es_geo_level(c("SE", "SE1", "SE11", "SE110", "DEA", "EU27_2020", "EA20", "EFTA", "EA", "SEZZZ")),
               c("land", "nuts1", "nuts2", "nuts3", "nuts1", "ovriga", "ovriga", "ovriga", "ovriga", "nuts3"))
  # länder utanför NUTS, och aggregat som börjar som ett land (EE)
  expect_equal(es_geo_level(c("BY", "EEA30", "EEA", "EE001", "CIS", "DE_TOT")),
               c("land", "ovriga", "ovriga", "nuts3", "ovriga", "ovriga"))
  val <- es_geo_choices(data.frame(code = c("SE", "NO", "EU27_2020"), label = "x"))
  expect_equal(val$id, c("land", "ovriga"))
  expect_equal(val$label[1], "Länder")
  expect_null(es_geo_choices(data.frame(code = c("SE", "NO"), label = "x")))
})

test_that("es_get_codes ger koder och stoppar för okänd variabel", {
  local_mocked_bindings(es_get_structure = mock_structure)
  d <- es_get_codes("demo_gind", lang = "de")
  expect_named(d, c("var_kod", "var_namn", "kod", "namn"))
  expect_equal(d$namn[d$kod == "SE"], "Sweden de")
  expect_equal(unique(es_get_codes("demo_gind", variable = "geo")$var_kod), "geo")
  expect_error(es_get_codes("demo_gind", variable = "sex"), "Variablerna är")
})

test_that("es_get_comments ger en rad per uppgift och tabell", {
  local_mocked_bindings(es_get_structure = mock_structure)
  k <- es_get_comments(c("demo_gind", "tps00001"))
  expect_equal(unique(k$tabell), c("demo_gind", "tps00001"))
  expect_named(k, c("tabell", "typ", "text", "url"))
})
