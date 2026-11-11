# Kräver internet
if (eurostat::check_access_to_data()) {
  # Uppdateringsdatum, perioder och länkar till metadata för en tabell
  es_get_comments("demo_gind")

  # Flera tabeller, med länken till Data Browser på tyska
  es_get_comments(c("demo_gind", "tps00001"), lang = "de")
}

# Hos OECD
if (interactive() && requireNamespace("OECDdata", quietly = TRUE)) {
  es_get_comments("DSD_LFS@DF_IALFS_UNE_M", source = "oecd")
}
