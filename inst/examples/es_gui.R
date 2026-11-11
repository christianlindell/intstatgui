# Gränssnittet kräver en interaktiv R-session och internet
if (interactive()) {
  # Öppna gränssnittet, i eget fönster i RStudio och annars i webbläsaren
  es_gui()

  # I webbläsaren, med tabellnamn och värden på tyska
  es_gui(viewer = "browser", lang = "de")

  # Med OECD förvalt
  es_gui(source = "oecd")

  # Med knappen som skriver koden i det aktiva skriptet
  kod <- es_gui(addin = TRUE)
}
