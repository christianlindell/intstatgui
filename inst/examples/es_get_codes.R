# Kräver internet
if (eurostat::check_access_to_data()) {
  # Alla variabler och värden i en tabell
  es_get_codes("demo_gind")

  # Bara regionerna, med namnen på franska
  es_get_codes("demo_gind", variable = "geo", lang = "fr")
}

# Hos OECD
if (interactive() && requireNamespace("OECDdata", quietly = TRUE)) {
  es_get_codes("DSD_LFS@DF_IALFS_UNE_M", variable = "REF_AREA", source = "oecd")
}
