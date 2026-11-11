# Kräver internet första gången. Listan sparas sedan i 24 timmar.
if (eurostat::check_access_to_data()) {
  # Alla tabeller med båda orden i namnet eller id:t
  es_search_tables("population nuts")

  # Bara dataset, med namnen på tyska
  es_search_tables("bevölkerung", lang = "de", only_datasets = TRUE)
}

# Hos OECD
if (interactive() && requireNamespace("OECDdata", quietly = TRUE)) {
  es_search_tables("unemployment", source = "oecd")
}
