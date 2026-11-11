# Det gränssnittet behöver från en datakälla. Testerna byter ut det med
# options(intstatgui.backend = ...), så att de går utan nätverk.
es_backend <- function(source = "eurostat") {
  mock <- getOption("intstatgui.backend")
  if (!is.null(mock)) return(mock)
  list(
    toc = if (source == "oecd") es_oecd_toc else function(lang) eurostat::get_eurostat_toc(lang = lang),
    parse = if (source == "oecd") es_parse_oecd_toc else es_parse_toc,
    structure = function(id, lang = "en") es_get_structure(id, lang, source = source),
    get_data = function(...) es_get_data(..., source = source),
    comments = function(id, lang = "en") es_get_comments(id, lang, source = source),
    available = if (source == "oecd") es_oecd_available
  )
}
