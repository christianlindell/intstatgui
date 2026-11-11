# Gränssnittet: es_app_ui() och es_app_server() används både av es_gui()
# (fristående eller som RStudio-addin) och av testerna.
#
# 1. Välj datakälla (Eurostat eller OECD) och språk och bläddra i tabellerna
# 2. Välj variabler och värden
# 3. Få färdig es_get_data()-kod och/eller hämta data direkt
# 4. Läs uppgifter om de valda tabellerna, efter klick på knappen i flik 1

es_tree_js <- "
// Den valda tabellen. Markeringen finns kvar när en sökning ritar om trädet.
var esVald = null;
function esMarkTable() {
  $('.es-tree a.es-selected').removeClass('es-selected');
  $('.es-tree a.es-table').filter(function() {
    return this.getAttribute('data-id') === esVald;
  }).addClass('es-selected');
}
$(document).on('click', '.es-tree a.es-table', function(e) {
  e.preventDefault();
  esVald = this.getAttribute('data-id');
  esMarkTable();
  Shiny.setInputValue('vald_tabell', esVald, {priority: 'event'});
});
Shiny.addCustomMessageHandler('esSetTable', function(m) {
  esVald = m.id || null;
  esMarkTable();
});
// Fliken Information är gråtonad och går inte att klicka på när den saknar innehåll
function esKommentarFlik(aktiv) {
  $('a[data-value=\"kommentarer\"]').toggleClass('disabled', !aktiv)
    .attr('aria-disabled', aktiv ? null : 'true');
}
$(function() { esKommentarFlik(false); });
Shiny.addCustomMessageHandler('esKommentarFlik', esKommentarFlik);
$(document).on('click', '#expandera_alla', function() {
  $('.es-tree details').prop('open', true);
});
$(document).on('click', '#fall_ihop_alla', function() {
  $('.es-tree details').prop('open', false);
});
// Kopiering: urklipps-API:t fungerar inte i alla fönster (t.ex. RStudios
// Viewer), så vid behov provas execCommand och sist servern (clipr).
function esCopied(ok) {
  $('#kopiera_kod').text(ok ? 'Kopierad!' : 'Kopiera koden');
  if (ok) setTimeout(function() { $('#kopiera_kod').text('Kopiera koden'); }, 1500);
}
function esCopyFallback(txt) {
  var ta = $('<textarea readonly>').val(txt)
    .css({position: 'fixed', top: 0, left: 0, opacity: 0}).appendTo('body');
  ta[0].focus(); ta[0].select();
  var ok = false;
  try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
  ta.remove();
  if (ok) esCopied(true);
  else Shiny.setInputValue('kopiera_via_server', Math.random());
}
$(document).on('click', '#kopiera_kod', function() {
  var txt = $('#kod').text();
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(txt).then(function() { esCopied(true); },
                                            function() { esCopyFallback(txt); });
  } else {
    esCopyFallback(txt);
  }
});
Shiny.addCustomMessageHandler('esCopied', function(ok) { esCopied(ok); });
// efter Stäng/Infoga: visa att appen är stängd (t.ex. i Positrons Viewer,
// där sidan annars ligger kvar) och stäng fliken om webbläsaren tillåter det
Shiny.addCustomMessageHandler('esClose', function(x) {
  var d = document.createElement('div');
  d.style.cssText = 'font-family: system-ui, sans-serif; padding: 3rem; text-align: center; color: #555;';
  var h = document.createElement('h3');
  h.textContent = 'intstatgui är stängt';
  var p = document.createElement('p');
  p.textContent = (x && x.inserted ? 'Koden har infogats i skriptet. ' : '') +
    'Du kan stänga den här fliken eller panelen.';
  d.appendChild(h); d.appendChild(p);
  document.body.innerHTML = '';
  document.body.appendChild(d);
  setTimeout(function() { try { window.close(); } catch (e) {} }, 150);
});
"

es_tree_css <- "
.es-tree { font-size: 0.95rem; }
.es-tree details { margin-left: 0.2rem; }
.es-tree summary { cursor: pointer; padding: 2px 0; }
.es-tree summary:hover .es-folder { text-decoration: underline; }
.es-children { margin-left: 1.1rem; border-left: 1px solid var(--bs-border-color); padding-left: 0.6rem; }
.es-count { margin-left: 0.4rem; font-size: 0.8em; color: var(--bs-secondary-color);
            background: var(--bs-tertiary-bg); border-radius: 0.6em; padding: 0 0.45em; }
.es-tables { list-style: none; padding-left: 0.2rem; margin: 0.15rem 0 0.4rem 0; }
.es-tables li { padding: 1px 0; }
.es-table { text-decoration: none; }
.es-table:hover .es-table-label { text-decoration: underline; }
.es-table-id { font-family: var(--bs-font-monospace); font-size: 0.85em; margin-right: 0.4rem;
               color: var(--bs-secondary-color); }
.es-selected { background: var(--bs-warning-bg-subtle); border-radius: 3px; }
.es-period { margin-left: 0.4rem; font-size: 0.8em; color: var(--bs-secondary-color); }
.es-badge { margin-left: 0.4rem; font-size: 0.75em; color: var(--bs-secondary-color); }
#kod { font-size: 0.85rem; user-select: all; }
.es-kommentarer-sida { flex: 1 1 auto; min-height: 0; overflow-y: auto; }
.es-kommentar-tabell { flex: none; overflow: visible; }
.es-kommentar { padding: 0.35rem 0; border-top: 1px solid var(--bs-border-color); }
.es-kommentar-varde { font-weight: 600; font-size: 0.9rem; }
"

es_time_formats <- c("Datakällans text, t.ex. 2023-Q1" = "raw",
                     "Datum, periodens första dag" = "date",
                     "Datum, periodens sista dag" = "date_last",
                     "Tal, t.ex. 2023.25" = "num")

# addin = TRUE lägger till knapparna "Infoga koden i skriptet" och "Stäng",
# som avslutar gadgeten och återgår till RStudio.
es_app_ui <- function(addin = FALSE) {
  page_navbar(
    title = "intstatgui",
    id = "flikar",
    theme = bs_theme(version = 5),
    header = tags$head(tags$script(HTML(es_tree_js)), tags$script(HTML(es_var_js)),
                       tags$style(HTML(es_tree_css)), tags$style(HTML(es_var_css))),

    nav_panel(
      "1. Hitta tabell", value = "tabell",
      layout_sidebar(
        sidebar = sidebar(
          width = 340,
          selectInput("kalla", "Datakälla", choices = es_sources,
                      selected = getOption("intstatgui.source", "eurostat"), selectize = FALSE),
          selectInput("sprak", "Språk på tabeller och värden",
                      choices = es_languages(getOption("intstatgui.source", "eurostat")),
                      selected = getOption("intstatgui.lang", "en"), selectize = FALSE),
          uiOutput("kalla_info"),
          hr(),
          textInput("sok", "Sök i tabellnamn eller tabell-id",
                    placeholder = "t.ex. population"),
          conditionalPanel(
            "input.kalla == 'eurostat'",
            checkboxInput("bara_dataset", "Visa bara dataset, inte färdiga tabeller", TRUE)
          ),
          div(
            class = "d-flex gap-2",
            actionButton("expandera_alla", "Expandera alla", class = "btn-sm"),
            actionButton("fall_ihop_alla", "Fäll ihop alla", class = "btn-sm")
          ),
          hr(),
          uiOutput("vald_tabell_info"),
          hr(),
          uiOutput("katalog_status"),
          actionButton("uppdatera", "Hämta tabellistan på nytt",
                       class = "btn-sm btn-outline-secondary")
        ),
        card(
          full_screen = TRUE,
          # i rubriken syns knappen alltid; sidopanelen behöver ofta rullas
          card_header(class = "d-flex justify-content-between align-items-center",
                      "Ämnesområden och tabeller",
                      uiOutput("kommentarer_knapp")),
          uiOutput("trad")
        )
      )
    ),

    nav_panel(
      "2. Välj variabler", value = "variabler",
      uiOutput("variabel_rubrik"),
      uiOutput("variabler")
    ),

    nav_panel(
      "3. Kod och data", value = "data",
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("R-kod"),
          uiOutput("fraga_status"),
          div(actionButton("valj_alla", "Välj alla värden",
                           class = "btn-sm btn-outline-secondary")),
          uiOutput("kod_variabler_ui"),
          selectInput("tidsformat", "Tid", choices = es_time_formats, selectize = FALSE),
          checkboxInput("clean_names",
                        tagList("R-kompatibla kolumnnamn med janitor::clean_names()"),
                        value = getOption("intstatgui.clean_names", FALSE)),
          uiOutput("clean_names_info"),
          conditionalPanel(
            "input.kalla == 'eurostat'",
            checkboxInput("direkt", "Kod som bara kräver paketet eurostat", FALSE),
            uiOutput("direkt_info")
          ),
          verbatimTextOutput("kod"),
          div(
            class = "d-flex gap-2",
            tags$button(id = "kopiera_kod", type = "button",
                        class = "btn btn-sm btn-outline-primary", "Kopiera koden"),
            downloadButton("ladda_ner_skript", "Ladda ner som .R", class = "btn-sm")
          ),
          if (addin) {
            actionButton("infoga_kod", "Infoga koden i skriptet och stäng",
                         class = "btn-primary mt-3")
          }
        ),
        card(
          full_screen = TRUE,
          card_header("Data"),
          div(
            class = "d-flex gap-2 mb-2",
            actionButton("hamta_data", "Hämta data", class = "btn-primary"),
            div(class = "ms-auto d-flex gap-2 align-items-center",
                selectInput("format", NULL, choices = es_export_formats, width = "230px",
                            selectize = FALSE),
                downloadButton("ladda_ner_data", "Ladda ner", class = "btn-sm"))
          ),
          uiOutput("format_info"),
          uiOutput("data_status"),
          uiOutput("data_tabell_ruta")
        )
      )
    ),

    # gråtonad tills knappen "Information" i flik 1 har gett den innehåll
    nav_panel(
      "4. Information", value = "kommentarer",
      div(class = "es-kommentarer-sida", uiOutput("kommentarer"))
    ),

    nav_spacer(),
    if (addin) {
      nav_item(actionButton("stang", "Stäng", class = "btn-sm btn-outline-secondary"))
    }
  )
}

es_app_server <- function(addin = FALSE) {
  function(input, output, session) {
    # --- Avslut ------------------------------------------------------------------
    # Appen stoppas bara en gång. Stängs fönstret eller fliken (t.ex. i
    # Positron eller webbläsaren) avslutas den också, så att R inte blir
    # hängande i en gadget utan fönster.
    # Vid Stäng/Infoga skickas först meddelandet till sidan, och appen
    # stoppas först när det har gått iväg (onFlushed).
    stoppad <- FALSE
    es_stop <- function(value = NULL, inserted = FALSE, now = FALSE) {
      if (stoppad) return(invisible(NULL))
      stoppad <<- TRUE
      if (now) return(try(es_stop_app(value), silent = TRUE))
      try(session$sendCustomMessage("esClose", list(inserted = inserted)), silent = TRUE)
      session$onFlushed(function() try(es_stop_app(value), silent = TRUE), once = TRUE)
    }
    session$onSessionEnded(function() es_stop(invisible(NULL), now = TRUE))

    # --- Addin: infoga koden i RStudio och stäng -------------------------------
    if (addin) {
      observeEvent(input$stang, es_stop(invisible(NULL)))
      observeEvent(input$infoga_kod, {
        if (is.null(tabell())) {
          showNotification("Välj först en tabell.", type = "warning")
          return()
        }
        code <- fraga()$code
        es_insert_code(code)
        es_stop(invisible(code), inserted = TRUE)
      })
    }

    # --- Datakälla och språk -----------------------------------------------------
    kalla <- reactive(input$kalla %||% getOption("intstatgui.source", "eurostat"))

    observeEvent(kalla(), {
      val <- es_languages(kalla())
      cur <- isolate(input$sprak)
      updateSelectInput(session, "sprak", choices = val,
                        selected = if (!is.null(cur) && cur %in% val) cur else "en")
    }, ignoreInit = TRUE)

    # väntar tills språket finns hos den valda datakällan
    site <- reactive({
      req(input$sprak, input$sprak %in% es_languages(kalla()))
      list(source = kalla(), lang = input$sprak)
    })

    output$kalla_info <- renderUI({
      if (kalla() != "oecd") return(NULL)
      if (!requireNamespace("OECDdata", quietly = TRUE)) {
        return(div(class = "text-warning small mb-2",
                   "OECD kräver paketet OECDdata. Installera med ",
                   tags$code('install.packages("OECDdata")'), "."))
      }
      div(class = "small text-muted mb-2", tags$strong("OECD. "),
          "Tillåter 60 hämtningar av data per timme. Tabellistan saknar perioder, och alla ",
          "perioder i listorna behöver inte ha data.")
    })

    # --- Tabellkatalog ---------------------------------------------------------
    katalog <- reactiveVal(NULL)
    katalog_fel <- reactiveVal(NULL)

    lasa_katalog <- function(force = FALSE) {
      s <- isolate(site())
      withProgress(message = paste0("Hämtar tabellistan från ", names(es_sources)[es_sources == s$source], "…"),
                   value = 0.3, {
        res <- tryCatch(es_fetch_catalogue(es_backend(s$source), lang = s$lang, force = force,
                                           source = s$source),
                        error = function(e) e)
      })
      if (inherits(res, "error")) {
        katalog(NULL)
        katalog_fel(es_error_text(res))
      } else {
        katalog_fel(NULL)
        katalog(res)
      }
    }

    # ny datakälla eller nytt språk: ny tabellista, och vald tabell nollställs
    observeEvent(site(), {
      tabell(NULL)
      session$sendCustomMessage("esSetTable", list())
      indelning(list())
      rensa_data()
      katalog(NULL)
      lasa_katalog()
    })
    observeEvent(input$uppdatera, lasa_katalog(force = TRUE))

    sok <- debounce(reactive(input$sok), 400)

    filtrerade <- reactive({
      req(katalog())
      es_filter_tables(katalog()$tables, sok(), isTRUE(input$bara_dataset))
    })

    trad <- reactive({
      req(katalog())
      es_build_tree(katalog(), filtrerade()$id)
    })

    output$trad <- renderUI({
      if (!is.null(katalog_fel())) {
        return(div(class = "text-danger", "Kunde inte hämta tabellistan: ", katalog_fel()))
      }
      if (is.null(katalog())) return(p("Hämtar tabellistan…"))
      t <- trad()
      if (t$n == 0) return(p("Inga tabeller matchar sökningen."))
      # vid sökning fälls hela det (filtrerade) trädet ut
      es_tree_html(t, katalog()$tables, open = nzchar(trimws(sok() %||% "")),
                   selected = isolate(tabell()$id))
    })

    output$katalog_status <- renderUI({
      k <- katalog()
      if (is.null(k)) return(NULL)
      s <- es_tree_stats(trad())
      tags$small(
        class = "text-muted",
        sprintf("%d mappar och %d tabeller visas. ", s$folders, s$tables),
        sprintf("Tabellistan hämtades %s.", format(k$fetched, "%Y-%m-%d %H:%M"))
      )
    })

    # --- Vald tabell -----------------------------------------------------------
    tabell <- reactiveVal(NULL)
    laddningar <- reactiveVal(0)
    struktur_cache <- new.env(parent = emptyenv())

    las_tabell <- function(id, s) {
      key <- paste(s$source, s$lang, id)
      if (is.null(struktur_cache[[key]])) {
        struktur_cache[[key]] <- es_backend(s$source)$structure(id, lang = s$lang)
      }
      struktur_cache[[key]]
    }

    observeEvent(input$vald_tabell, {
      id <- as.character(input$vald_tabell)
      # samma tabell igen ska inte radera valen
      if (identical(id, tabell()$id)) return()
      s <- site()
      res <- withProgress(message = paste("Hämtar metadata för", id), value = 0.5, {
        tryCatch(las_tabell(id, s), error = function(e) e)
      })
      if (inherits(res, "error")) {
        showNotification(paste("Kunde inte hämta tabellen:", es_error_text(res)),
                         type = "error", duration = NULL)
        # allt är som före klicket, även markeringen i trädet
        session$sendCustomMessage("esSetTable", list(id = tabell()$id))
        return()
      }
      res$id <- id
      res$lang <- s$lang
      res$source <- s$source
      res$defaults <- es_defaults(res)
      indelning(list())
      rensa_data()
      laddningar(laddningar() + 1)
      res$prefix <- paste0("t", laddningar(), "_")
      tabell(res)
    })

    output$vald_tabell_info <- renderUI({
      t <- tabell()
      if (is.null(t)) {
        return(tags$small(class = "text-muted", "Klicka på en tabell i trädet för att välja den."))
      }
      tagList(
        div(class = "mb-1", tags$strong(t$id), br(), t$title),
        tags$small(class = "text-muted",
                   paste(nrow(t$variables), "variabler:",
                         paste(t$variables$label, collapse = ", "))),
        div(class = "mt-2",
            actionButton("till_variabler", "Välj variabler →", class = "btn-primary btn-sm"))
      )
    })

    observeEvent(input$till_variabler, nav_select("flikar", "variabler"))

    # --- Variabelval -----------------------------------------------------------
    var_id <- function(t, kind, i) paste0(t$prefix, kind, "_", i)

    output$variabel_rubrik <- renderUI({
      t <- tabell()
      if (is.null(t)) return(p("Välj först en tabell under flik 1."))
      tagList(
        h4("Välj variabler"),
        p(class = "small text-muted",
          "Klick väljer ett värde. Ctrl+klick lägger till eller tar bort ett värde. Skift+klick väljer ett intervall."),
        p(class = "mb-1", t$title, " ", tags$code(t$id)),
        if (t$source == "oecd") {
          tagList(
            p(class = "small text-muted",
              "En tom lista betyder alla möjliga värden. Gråa värden går inte att kombinera med övriga val: avmarkera ett val för att låsa upp dem. Not applicable [_Z] tas alltid med när ett annat värde är valt, så att mått där variabeln inte gäller blir kvar."),
            uiOutput("mojligt_antal")
          )
        },
        if (mojligt_sparr()) {
          div(class = "alert alert-warning py-2 px-3 small",
              "OECD har tillfälligt spärrat anropen, så listorna kontrolleras inte längre. ",
              "Alla kombinationer går inte att hämta. Öppna gränssnittet på nytt om en stund.")
        }
      )
    })

    observeEvent(input$till_data, nav_select("flikar", "data"))

    # --- Information -----------------------------------------------------------
    kommentar_cache <- new.env(parent = emptyenv())
    # det som visas i fliken: list(id, title, res), eller NULL när den är tom
    kommentar_innehall <- reactiveVal(NULL)

    output$kommentarer_knapp <- renderUI({
      knapp <- actionButton("hamta_kommentarer", "Information",
                            class = "btn-sm btn-outline-secondary")
      if (is.null(tabell())) tagAppendAttributes(knapp, disabled = NA) else knapp
    })

    observeEvent(input$hamta_kommentarer, {
      t <- tabell()
      req(t)
      res <- withProgress(message = "Hämtar uppgifter…", value = 0.5, {
        es_table_comments(es_backend(t$source), t$id, t$lang, kommentar_cache)
      })
      kommentar_innehall(list(id = t$id, title = t$title, res = res))
      nav_select("flikar", "kommentarer")
    })

    # uppgifterna gäller ett visst val: är det ändrat eller tomt raderas de
    observeEvent(tabell(), kommentar_innehall(NULL), ignoreNULL = FALSE, ignoreInit = TRUE)

    observe(es_kommentar_flik(session, !is.null(kommentar_innehall())))

    output$kommentarer <- renderUI({
      k <- kommentar_innehall()
      req(k)
      tagList(
        h4("Uppgifter om vald tabell"),
        es_comment_card(k$id, k$title, k$res)
      )
    })

    # en rensning ska nå sidan direkt, även när fliken inte visas
    outputOptions(output, "kommentarer", suspendWhenHidden = FALSE)

    output$variabler <- renderUI({
      t <- tabell()
      req(t)
      v <- t$variables
      # hos OECD visas inte variabler där det inte finns något att välja
      vis <- t$source != "oecd" | v$size > 1
      box <- function(i) {
        es_var_box(var_id(t, "varden", i), v$label[i], t$values[[v$code[i]]],
                   v$role[i], v$elimination[i],
                   codelists = if (t$source == "eurostat" && identical(v$role[i], "geo")) {
                     es_geo_choices(t$values[[v$code[i]]])
                   },
                   selected = t$defaults[[v$code[i]]])
      }
      tagList(
        div(class = "es-vars", lapply(which(vis), box)),
        div(
          class = "mt-4",
          actionButton("till_data", "Fortsätt", class = "btn-primary px-3"),
          uiOutput("cell_info", class = "mt-2")
        )
      )
    })

    # listorna behövs även om flik 2 inte har öppnats, för "Välj alla värden"
    outputOptions(output, "variabler", suspendWhenHidden = FALSE)

    observeEvent(input$valj_alla, {
      t <- tabell()
      if (is.null(t)) {
        showNotification("Välj först en tabell.", type = "warning")
        return()
      }
      session$sendCustomMessage("esMarkAll", as.list(vapply(
        seq_len(nrow(t$variables)), function(i) var_id(t, "varden", i), "")))
    })

    # Vald regionnivå per variabel: variabelkod -> list(id, values)
    indelning <- reactiveVal(list())

    observeEvent(input$es_indelning, {
      t <- tabell()
      req(t)
      ev <- input$es_indelning
      i <- match(ev$target, vapply(seq_len(nrow(t$variables)),
                                   function(k) var_id(t, "varden", k), ""))
      req(!is.na(i))
      var <- t$variables$code[i]
      cur <- indelning()
      alla <- t$values[[var]]
      if (!nzchar(ev$codelist %||% "")) {
        cur[[var]] <- NULL
        values <- alla
      } else {
        values <- alla[es_geo_level(alla$code) == ev$codelist, , drop = FALSE]
        cur[[var]] <- list(id = ev$codelist, values = values)
      }
      indelning(cur)
      session$sendCustomMessage("esSetOptions", list(
        id = ev$target,
        html = es_options_html(values$code, es_option_labels(values)),
        total = nrow(values)
      ))
    })

    fraga <- reactive({
      t <- tabell()
      req(t)
      v <- t$variables
      ind <- indelning()
      values <- t$values
      for (var in names(ind)) values[[var]] <- ind[[var]]$values
      selections <- lapply(seq_len(nrow(v)), function(i) {
        id <- var_id(t, "varden", i)
        vals <- input[[id]]
        # innan webbläsaren skickat något gäller förvalen
        if (is.null(vals) && is.null(input[[paste0(id, "_andrad")]])) {
          vals <- t$defaults[[v$code[i]]]
        }
        list(values = vals, latest = isTRUE(input[[paste0(id, "_senaste")]]),
             indelad = v$code[i] %in% names(ind))
      })
      names(selections) <- v$code
      q <- es_build_query(v, values, selections)
      q$filters <- es_query_filters(q$query)
      if (t$source == "oecd") q$filters <- es_add_not_applicable(q$filters, t$values)
      q$cells <- es_cell_count(t$variables, t$values, q$filters)
      # variabler som ska få en kodkolumn <variabel>_code; standard är bara klartext.
      # Valen är variablernas koder, som är lika på alla språk.
      q$included <- stats::setNames(v$code[v$code != "time"], v$label[v$code != "time"])
      q$code_variables <- unname(q$included[q$included %in% input[[var_id(t, "kodvariabler", 0)]]])
      q$code <- es_query_code(t$id, t$title, q$filters, code_variables = q$code_variables,
                              lang = t$lang, time_format = input$tidsformat %||% "raw",
                              clean_names = isTRUE(input$clean_names),
                              direct = isTRUE(input$direkt), source = t$source)
      q
    })

    output$clean_names_info <- renderUI({
      if (isTRUE(input$clean_names) && !requireNamespace("janitor", quietly = TRUE)) {
        div(class = "text-warning small mb-2",
            "Paketet janitor är inte installerat. Koden använder det, men data i",
            "gränssnittet får sina ursprungliga kolumnnamn. Installera med",
            tags$code('install.packages("janitor")'), ".")
      }
    })

    output$direkt_info <- renderUI({
      t <- tabell()
      if (!isTRUE(input$direkt) || is.null(t)) return(NULL)
      if (length(fraga()$code_variables) > 0) {
        div(class = "text-warning small mb-2",
            "Kodkolumner kräver es_get_data(), så koden använder den ändå.")
      }
    })

    output$kod_variabler_ui <- renderUI({
      t <- tabell()
      req(t)
      v <- t$variables[t$variables$code != "time", , drop = FALSE]
      med <- stats::setNames(v$code, v$label)
      id <- var_id(t, "kodvariabler", 0)
      div(
        class = "es-kodval mb-2",
        checkboxGroupInput(
          id,
          label = tagList("Variabler som även ska få koder",
                          tags$small(class = "text-muted fw-normal",
                                     " (kolumnen får namnet variabel_code, övriga visas i klartext)")),
          choices = med, inline = TRUE
        )
      )
    })

    cell_text <- function(q) {
      paste0("Antal valda celler är: ", format(q$cells, big.mark = " ", scientific = FALSE),
             ". Alla behöver inte ha data.")
    }

    # --- Möjliga kombinationer (OECD) --------------------------------------------
    # Listorna grånas efter val i flik 2, med en fördröjning så att varje klick inte blir ett anrop.
    # Svaren sparas under sessionen. Vid en spärr från OECD skickas inga fler frågor under sessionen.
    # Eurostat har ingen sådan fråga.
    mojligt <- reactiveVal(NULL)
    mojligt_sparr <- reactiveVal(FALSE)
    mojligt_cache <- new.env(parent = emptyenv())

    mojligt_urval <- debounce(reactive({
      t <- tabell()
      req(t, t$source == "oecd", input$flikar %in% c("variabler", "data"),
          !mojligt_sparr())
      list(id = t$id, lang = t$lang, filters = fraga()$filters, flik = input$flikar)
    }), 1500)

    observeEvent(mojligt_urval(), {
      t <- tabell()
      u <- mojligt_urval()
      req(t, identical(t$id, u$id))
      hamta <- es_backend(t$source)$available
      if (is.null(hamta)) return()
      if (isolate(mojligt_sparr())) return()
      fraga_oecd <- function(filters) {
        # en hash: ett namn i en environment får vara högst 10 000 byte, och ett fel i en
        # observer avslutar appen
        key <- digest::digest(list(u$lang, u$id, filters))
        if (is.null(mojligt_cache[[key]])) {
          svar <- tryCatch(hamta(t, filters), error = function(e) {
            if (es_is_rate_limit(e)) mojligt_sparr(TRUE)
            NULL
          })
          if (!is.null(svar)) mojligt_cache[[key]] <- svar
        }
        mojligt_cache[[key]]
      }
      # Listorna bygger på valen utom tiden: annars skulle ett förvalt år utan data, till
      # exempel en framskrivning till 2035, göra nästan allt annat grått.
      utan_tid <- u$filters[setdiff(names(u$filters), c("time", es_time_params))]
      # Flik 2 frågar bara om listorna, med antalet för alla perioder. Flik 3 frågar i stället om
      # antalet för de valda perioderna. Det ger en fråga per ändring.
      med_tid <- u$flik == "data" && !is.null(u$filters$time)
      a <- fraga_oecd(if (med_tid) u$filters else utan_tid)
      # ett fel här ska inte hindra hämtningen; listorna blir bara inte grå
      if (is.null(a)) return()
      a$med_tid <- med_tid
      # en för lång lista räknas i frågan som alla värden, så antalet gäller inte urvalet
      if (any(vapply(utan_tid, es_oecd_too_long, TRUE))) a$obs <- NA_real_
      a$filters <- u$filters
      a$id <- u$id
      mojligt(a)
      if (u$flik == "variabler") {
        session$sendCustomMessage("esAvailable", es_available_message(
          t, a, function(i) var_id(t, "varden", i)))
      }
    })

    # svaret för just det aktuella urvalet, annars NULL
    mojligt_nu <- function(q) {
      a <- mojligt()
      t <- tabell()
      if (!is.null(a) && !is.null(t) && identical(a$id, t$id) && identical(a$filters, q$filters)) a
    }

    # Antalet värden enligt OECD. I flik 2 gäller det alla perioder, om en period är vald.
    antal_ui <- function(q) {
      a <- mojligt_nu(q)
      if (is.null(a)) return(NULL)
      if (is.na(a$obs)) {
        return(p(class = "text-muted", "OECD kan inte räkna urvalet, eftersom listan med värden är för lång."))
      }
      if (a$obs == 0) return(NULL)
      alla <- !isTRUE(a$med_tid) && (!is.null(q$filters$time) || !is.null(q$filters$lastTimePeriod))
      tagList(
        p(class = "text-muted", paste0("Antal värden med data enligt OECD: ",
                                       format(a$obs, big.mark = " ", scientific = FALSE),
                                       if (alla) " (alla perioder)", ".")),
        if (!alla && a$obs > 1e5) {
          div(class = "alert alert-warning py-2 px-3 small",
              "Urvalet är stort. Hämtningen kan ta lång tid, så avgränsa gärna regioner eller perioder.")
        }
      )
    }

    output$mojligt_antal <- renderUI(antal_ui(fraga()))
    outputOptions(output, "mojligt_antal", suspendWhenHidden = FALSE)

    fraga_info <- function(q) {
      t <- tabell()
      req(t)
      a <- mojligt_nu(q)
      # hos OECD är OECD:s antal bättre än antalet celler i rutnätet
      celler <- t$source != "oecd"
      tagList(
        if (celler) p(class = "text-muted", cell_text(q)),
        antal_ui(q),
        if (!is.null(a) && a$obs %in% 0) {
          div(class = "alert alert-danger py-2 px-3 small",
              "Urvalet saknar data: de valda värdena går inte att kombinera. ",
              "Röda värden i listorna finns inte tillsammans med övriga val.")
        },
        if (celler && !is.null(es_cell_warning(q$cells))) {
          div(class = "alert alert-warning py-2 px-3 small", es_cell_warning(q$cells))
        },
        if (length(q$problems) > 0) div(class = "text-danger", lapply(q$problems, p))
      )
    }

    output$cell_info <- renderUI(fraga_info(fraga()))

    # ritas inne i "variabler", som ritas innan fliken visas: utan det här räknas
    # texten som dold och uppdateras inte
    outputOptions(output, "cell_info", suspendWhenHidden = FALSE)

    # --- Kod och data ----------------------------------------------------------
    output$fraga_status <- renderUI({
      if (is.null(tabell())) return(p("Välj först en tabell under flik 1."))
      fraga_info(fraga())
    })

    output$kod <- renderText({
      req(tabell())
      fraga()$code
    })

    # sista utvägen för "Kopiera koden": appen körs lokalt, så urklippet i R
    # är användarens urklipp
    observeEvent(input$kopiera_via_server, {
      ok <- requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available() &&
        !inherits(try(clipr::write_clip(fraga()$code), silent = TRUE), "try-error")
      session$sendCustomMessage("esCopied", ok)
      if (!ok) {
        showNotification(paste("Kunde inte kopiera automatiskt. Markera koden och tryck Ctrl+C,",
                               "eller installera paketet clipr."), type = "warning")
      }
    })

    output$ladda_ner_skript <- downloadHandler(
      filename = function() paste0("hamta_", tabell()$id %||% "tabell", ".R"),
      content = function(file) writeLines(fraga()$code, file, useBytes = TRUE)
    )

    data_resultat <- reactiveVal(NULL)
    data_fel <- reactiveVal(NULL)
    data_inaktuell <- reactiveVal(FALSE)
    # frågan som det hämtade datat (eller felet) gäller
    data_fraga <- reactiveVal(NULL)
    # valda värden som inte gav någon rad
    data_saknas <- reactiveVal(NULL)

    rensa_data <- function() {
      if (!is.null(isolate(data_fraga()))) data_inaktuell(TRUE)
      data_resultat(NULL)
      data_fel(NULL)
      data_fraga(NULL)
      data_saknas(NULL)
    }

    # allt som avgör vad som hämtas; kodkolumner och kolumnnamn styr bara vad som visas
    fraga_nu <- reactive({
      t <- tabell()
      req(t)
      list(id = t$id, source = t$source, lang = t$lang, filters = fraga()$filters,
           time_format = input$tidsformat %||% "raw")
    })

    # tryCatch: ett fel i en observer skulle avsluta appen, och fraga() visar själv sina fel
    observe({
      req(data_fraga())
      nu <- tryCatch(fraga_nu(), error = function(e) NULL)
      if (!is.null(nu) && !identical(nu, data_fraga())) rensa_data()
    })

    observeEvent(input$hamta_data, {
      t <- tabell()
      if (is.null(t)) {
        showNotification("Välj först en tabell.", type = "warning")
        return()
      }
      q <- fraga()
      if (length(q$problems) > 0) {
        showNotification(paste(q$problems, collapse = " "), type = "warning")
        return()
      }
      # en hämtning som bara kan bli tom ska inte ta av OECD:s kvot
      if (isTRUE(mojligt_nu(q)$obs == 0)) {
        showNotification("Urvalet saknar data: de valda värdena går inte att kombinera.", type = "warning")
        return()
      }
      # Kodkolumner hämtas för alla variabler, så att rutorna kan visa och dölja dem
      # utan ny hämtning: frågan till API:et blir densamma.
      res <- withProgress(message = paste("Hämtar data från", t$id),
                          value = 0.5, {
        tryCatch(es_backend(t$source)$get_data(t$id, filters = q$filters, code_variables = unname(q$included),
                                  lang = t$lang, time_format = input$tidsformat %||% "raw",
                                  quiet = TRUE),
                 error = function(e) e)
      })
      data_inaktuell(FALSE)
      data_fraga(fraga_nu())
      if (inherits(res, "error")) {
        data_fel(es_error_text(res))
        data_resultat(NULL)
      } else {
        data_fel(NULL)
        data_saknas(attr(res, "saknas"))
        data_resultat(as.data.frame(res))
      }
    })

    output$data_status <- renderUI({
      if (!is.null(data_fel())) {
        return(div(class = "text-danger", "Hämtningen misslyckades: ", data_fel()))
      }
      d <- data_visad()
      if (is.null(d)) {
        return(p(class = "text-muted",
                 if (data_inaktuell()) {
                   "Valen har ändrats. Klicka på \"Hämta data\" för att hämta data på nytt."
                 } else {
                   "Ingen data hämtad ännu."
                 }))
      }
      if (nrow(d) == 0) {
        return(div(class = "text-warning",
                   "Urvalet gav inga värden. Kombinationen av valda värden saknar data, ",
                   "så prova att ändra valen i flik 2."))
      }
      tagList(
        p(class = "text-muted", sprintf("%s rader, %d kolumner.",
                                        format(nrow(d), big.mark = " "), ncol(d))),
        if (length(data_saknas()) > 0) {
          div(class = "alert alert-warning py-2 px-3 small",
              paste0("Inga värden med övriga val för ", paste(data_saknas(), collapse = "; "), "."))
        }
      )
    })

    # data som visas och laddas ner: med de valda kodkolumnerna, och med janitor-namn
    # om rutan är kryssad
    data_visad <- reactive({
      d <- data_resultat()
      if (is.null(d)) return(NULL)
      d <- es_keep_code_columns(d, fraga()$code_variables)
      if (isTRUE(input$clean_names)) es_clean_names(d) else d
    })

    # finns bara när det finns data: en DT-tabell töms inte när den blir utan innehåll
    output$data_tabell_ruta <- renderUI({
      req(data_resultat())
      DTOutput("data_tabell")
    })

    output$data_tabell <- renderDT({
      d <- data_visad()
      req(d)
      datatable(d, rownames = FALSE, filter = "top",
                options = list(pageLength = 25, scrollX = TRUE,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/sv-SE.json")))
    })

    # en rensning ska nå sidan direkt, även när fliken inte visas
    outputOptions(output, "data_status", suspendWhenHidden = FALSE)
    outputOptions(output, "data_tabell_ruta", suspendWhenHidden = FALSE)

    output$format_info <- renderUI({
      msg <- es_export_missing(input$format %||% "txt")
      if (!is.null(msg)) div(class = "text-warning small mb-2", msg)
    })

    output$ladda_ner_data <- downloadHandler(
      filename = function() paste0(tabell()$id %||% "data", ".", input$format),
      content = function(file) {
        d <- data_visad()
        if (is.null(d)) d <- data.frame()
        es_write_data(d, file, input$format)
      }
    )
  }
}

# egen funktion så att avslutet går att testa
es_stop_app <- function(value = NULL) shiny::stopApp(value)

# egen funktion så att flikens läge går att testa
es_kommentar_flik <- function(session, aktiv) session$sendCustomMessage("esKommentarFlik", aktiv)
