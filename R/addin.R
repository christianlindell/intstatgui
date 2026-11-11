#' Grafiskt gränssnitt för Eurostat och OECD
#'
#' Öppnar ett fönster där du väljer datakälla och språk, bläddrar bland eller söker i
#' tabellerna, väljer variabler och värden och får färdig [es_get_data()]-kod. Data kan
#' också hämtas direkt och laddas ner som txt, Excel eller parquet. Knappen
#' "Information" i flik 1 ger fliken "4. Information" med uppgifter om de valda
#' tabellerna och länkar till datakällans metadata.
#'
#' Tabellnamn och värden finns på engelska och franska, och hos Eurostat också på tyska.
#' Själva gränssnittet är på svenska. OECD kräver paketet OECDdata och tillåter 60
#' hämtningar av data per timme.
#'
#' @param viewer Var fönstret visas: `"dialog"` (eget fönster i RStudio),
#'   `"pane"` (Viewer-panelen) eller `"browser"` (webbläsaren). I Positron
#'   visas appen i Positrons Viewer, om inte
#'   `options(intstatgui.positron_viewer = ...)` anger något annat.
#' @param addin Om `TRUE` finns knappen "Infoga koden i skriptet och stäng",
#'   som skriver koden i det aktiva skriptet i RStudio.
#' @param lang Förvalt språk, `"en"`, `"de"` (bara Eurostat) eller `"fr"`. Standard är
#'   `getOption("intstatgui.lang", "en")`.
#' @param source Förvald datakälla, `"eurostat"` eller `"oecd"`. Standard är
#'   `getOption("intstatgui.source", "eurostat")`.
#'
#' @return Den genererade koden (osynligt) om den infogades, annars `NULL`.
#' @export
#' @example inst/examples/es_gui.R
es_gui <- function(viewer = c("dialog", "pane", "browser"), addin = FALSE,
                   lang = getOption("intstatgui.lang", "en"),
                   source = getOption("intstatgui.source", "eurostat")) {
  viewer <- match.arg(viewer)
  source <- es_check_source(source)
  old <- options(intstatgui.lang = es_check_lang(lang, source), intstatgui.source = source)
  on.exit(options(old), add = TRUE)
  # dialog och pane kräver RStudio; annars webbläsaren. I Positron används
  # "browser", som Positron själv visar i sin Viewer (eller det som anges med
  # options(intstatgui.positron_viewer = ...)).
  if (es_is_positron() && viewer != "browser") {
    viewer <- match.arg(getOption("intstatgui.positron_viewer", "browser"),
                        c("browser", "pane", "dialog"))
  }
  if (viewer != "browser" && !es_rstudio_available()) viewer <- "browser"
  v <- switch(
    viewer,
    dialog  = shiny::dialogViewer("Eurostat och OECD (intstatgui)", width = 1400, height = 950),
    pane    = shiny::paneViewer(minHeight = "maximize"),
    browser = shiny::browserViewer()
  )
  app <- shiny::shinyApp(es_app_ui(addin = addin), es_app_server(addin = addin))
  shiny::runGadget(app, viewer = v, stopOnCancel = FALSE)
}

#' RStudio-addin: tabell från Eurostat eller OECD till R-kod
#'
#' Anropas från menyn Addins i RStudio. Samma som
#' `es_gui(viewer = "dialog", addin = TRUE)`.
#'
#' @return Den infogade koden (osynligt), eller `NULL` om fönstret stängdes
#'   utan att någon kod infogades.
#' @export
#' @example inst/examples/es_gui_addin.R
es_gui_addin <- function() {
  es_gui(viewer = "dialog", addin = TRUE)
}

es_is_positron <- function() {
  identical(Sys.getenv("POSITRON"), "1") || nzchar(Sys.getenv("POSITRON_VERSION"))
}

es_rstudio_available <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

# Skriver koden vid markören i det aktiva skriptet. Finns inget öppet skript
# skapas ett nytt. Utanför RStudio (eller utan paketet rstudioapi) skrivs
# koden ut i konsolen.
es_insert_code <- function(code) {
  if (!es_rstudio_available()) {
    cat(code)
    return(invisible(code))
  }
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(ctx) || is.null(ctx$id) || !nzchar(ctx$id)) {
    rstudioapi::documentNew(code, type = "r")
  } else {
    rstudioapi::insertText(text = code, id = ctx$id)
  }
  invisible(code)
}
