# Kräver internet. Exemplen hämtar bara några få värden.
if (eurostat::check_access_to_data()) {
  # Folkmängden i två länder under två år. Hela tabellen hämtas om filters utelämnas.
  es_get_data("demo_gind",
              filters = list(geo = c("SE", "NO"), indic_de = "JAN", time = 2023:2024))

  # Senaste perioden, med namn på tyska och en kolumn med landskoden
  es_get_data("demo_gind",
              filters = list(geo = c("SE", "NO"), indic_de = "JAN", lastTimePeriod = 1),
              code_variables = "geo", lang = "de")

  # Bara koder, och tiden som datum
  es_get_data("demo_gind",
              filters = list(geo = "SE", indic_de = "JAN", time = 2024),
              type = "code", time_format = "date")
}

# Från OECD. Körs bara interaktivt, eftersom OECD tillåter 60 hämtningar per timme.
if (interactive() && requireNamespace("OECDdata", quietly = TRUE)) {
  es_get_data("DSD_LFS@DF_IALFS_UNE_M",
              filters = list(REF_AREA = c("SWE", "NOR"), ADJUSTMENT = "N", SEX = "_T",
                             AGE = "Y_GE15", time = "2024-01"),
              source = "oecd")
}
