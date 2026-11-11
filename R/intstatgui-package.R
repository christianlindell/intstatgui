#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @import bslib
#' @importFrom DT DTOutput renderDT datatable
## usethis namespace: end
NULL

`%||%` <- function(a, b) if (is.null(a)) b else a

es_sources <- c("Eurostat" = "eurostat", "OECD" = "oecd")

# Språken som datakällan har tabellnamn och värden på
es_languages <- function(source = "eurostat") {
  if (source == "oecd") return(c("English" = "en", "Français" = "fr"))
  c("English" = "en", "Deutsch" = "de", "Français" = "fr")
}

es_check_source <- function(source) {
  source <- tolower(source)
  if (length(source) != 1 || !source %in% es_sources) {
    stop("source ska vara \"eurostat\" eller \"oecd\".", call. = FALSE)
  }
  source
}

es_check_lang <- function(lang, source = "eurostat") {
  lang <- tolower(lang)
  val <- es_languages(source)
  if (length(lang) != 1 || !lang %in% val) {
    stop("lang ska vara ", paste0("\"", val, "\"", collapse = " eller "), ".", call. = FALSE)
  }
  lang
}
