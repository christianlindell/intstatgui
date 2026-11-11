# Variabelrutor i samma stil som i pxweb2rgui: rubrik med "(Obligatoriskt)",
# Markera alla / Avmarkera alla, "Valda x Totalt y", sökfält för långa
# listor och en lista med samma klick som en vanlig flervalslista.
# Listan är en egen komponent (div-element med en Shiny-inputbindning) i
# stället för en <select multiple>: webbläsarens listruta hoppar till toppen
# vid klick i vissa fönster, t.ex. RStudios Viewer. Beteendet finns i
# es_var_js nedan.

# Vilka värden som är förvalda: den senaste perioden och variabler med ett enda
# värde. Övriga variabler börjar utan val.
es_default_selection <- function(role, codes) {
  if (length(codes) == 0) return(character())
  if (identical(role, "time")) return(utils::tail(codes, 1))
  if (length(codes) == 1) return(codes)
  character()
}

# Etiketter i listan: namnet, med koden efter när den inte redan är namnet, så att
# koderna går att söka på.
es_option_labels <- function(values) {
  codes <- values$code
  labels <- ifelse(is.na(values$label) | values$label == "", codes, values$label)
  ifelse(labels == codes, labels, paste0(labels, " [", codes, "]"))
}

es_options_html <- function(codes, labels, selected = character(), reverse = FALSE) {
  ord <- if (reverse) rev(seq_along(codes)) else seq_along(codes)
  is_sel <- codes[ord] %in% selected
  paste0(
    '<div class="es-opt', ifelse(is_sel, " sel", ""), '" role="option" tabindex="-1"',
    ' aria-selected="', ifelse(is_sel, "true", "false"), '"',
    ' data-value="', htmltools::htmlEscape(codes[ord], attribute = TRUE), '"',
    ' title="', htmltools::htmlEscape(codes[ord], attribute = TRUE), '">',
    htmltools::htmlEscape(labels[ord]), "</div>",
    collapse = ""
  )
}

# codelists: data.frame med id, label och type från es_geo_choices(). Finns det
# några visas "Välj indelning", t.ex. Länder/NUTS 2 för region.
es_var_box <- function(id, label, values, role, elimination,
                       codelists = NULL, search_min = 10, selected = NULL) {
  codes <- values$code
  labels <- es_option_labels(values)
  selected <- selected %||% es_default_selection(role, codes)

  # tidsvariabeln visas med senaste perioden överst
  options <- es_options_html(codes, labels, selected, reverse = identical(role, "time"))

  shiny::div(
    class = paste("es-var", if (!isTRUE(elimination)) "es-var-required",
                  if (!isTRUE(elimination) && length(selected) == 0) "es-var-missing"),
    shiny::div(
      class = "es-var-head",
      shiny::tags$strong(label),
      if (!isTRUE(elimination)) {
        shiny::tags$small(class = "es-required",
                          title = "Minst ett värde måste väljas för den här variabeln",
                          " (Obligatoriskt)")
      }
    ),
    if (!is.null(codelists) && nrow(codelists) > 0) {
      shiny::tags$select(
        class = "form-select form-select-sm es-indelning mt-1", `data-target` = id,
        shiny::tags$option(value = "", "Välj nivå (alla värden i tabellen)"),
        lapply(seq_len(nrow(codelists)), function(k) {
          shiny::tags$option(value = codelists$id[k], `data-type` = codelists$type[k],
                             codelists$label[k])
        })
      )
    },
    shiny::div(
      class = "es-var-links",
      shiny::tags$a(href = "#", class = "es-mark-all", `data-target` = id, "Markera alla"),
      shiny::tags$a(href = "#", class = "es-unmark-all", `data-target` = id, "Avmarkera alla")
    ),
    if (length(codes) >= search_min) {
      shiny::div(
        class = "es-var-search",
        shiny::tags$label(
          class = "es-word-start",
          shiny::tags$input(type = "checkbox", class = "es-search-start", `data-target` = id),
          " Början av ord"
        ),
        shiny::tags$input(type = "search", class = "form-control form-control-sm es-search",
                          placeholder = "Sök här", `data-target` = id)
      )
    },
    shiny::div(
      class = "es-var-count",
      "Valda ", shiny::tags$span(id = paste0(id, "_valda"), length(selected)),
      " Totalt ", shiny::tags$span(id = paste0(id, "_totalt"), length(codes)),
      shiny::tags$span(id = paste0(id, "_mojliga"))
    ),
    shiny::div(
      id = id, class = "es-list", role = "listbox", `aria-multiselectable` = "true",
      `aria-label` = label, tabindex = "0",
      shiny::HTML(options)
    ),
    if (identical(role, "time")) {
      shiny::div(
        class = "form-check mt-1",
        shiny::tags$input(type = "checkbox", class = "form-check-input",
                          id = paste0(id, "_senaste")),
        shiny::tags$label(class = "form-check-label small", `for` = paste0(id, "_senaste"),
                          "Hämta alltid senaste perioden (lastTimePeriod i koden)")
      )
    }
  )
}

es_var_css <- "
.es-vars { display: flex; flex-wrap: wrap; gap: 1rem; align-items: flex-start; }
.es-var { background: var(--bs-tertiary-bg); padding: 0.9rem; width: 360px; max-width: 100%; }
.es-var-head small { color: var(--bs-body-color); }
.es-var-missing { outline: 2px solid var(--bs-danger); }
.es-var-missing .es-required { color: var(--bs-danger); font-weight: 600; }
.es-var-links { margin: 0.4rem 0; font-size: 0.85rem; }
.es-var-links a { margin-right: 0.6rem; }
.es-var-search { margin-bottom: 0.3rem; }
.es-word-start { font-size: 0.8rem; display: block; margin-bottom: 0.2rem; }
.es-var-count { font-size: 0.8rem; margin-bottom: 0.2rem; }
.es-list { width: 100%; height: 12.4rem; overflow-y: auto; border: 1px solid var(--bs-border-color);
           background: var(--bs-body-bg); font-size: 0.85rem; user-select: none; }
.es-opt { padding: 0.35rem 0.5rem; cursor: pointer; white-space: nowrap; overflow: hidden;
          text-overflow: ellipsis; }
.es-opt:hover { background: var(--bs-secondary-bg); }
.es-opt.sel { background: #222; color: #fff; }
.es-list.disabled { opacity: 0.5; pointer-events: none; }
.es-opt.es-tom { color: var(--bs-tertiary-color); cursor: not-allowed; background: var(--bs-tertiary-bg); }
.es-opt.es-tom.sel { color: #fff; background: var(--bs-danger); cursor: pointer; }
"

es_var_js <- "
// Shiny-inputbindning för listan: värdet är koderna för markerade rader
var esListBinding = new Shiny.InputBinding();
$.extend(esListBinding, {
  find: function(scope) { return $(scope).find('.es-list'); },
  getValue: function(el) {
    return $(el).find('.es-opt.sel').map(function() {
      return this.getAttribute('data-value');
    }).get();
  },
  subscribe: function(el, callback) {
    $(el).on('change.esList', function() { callback(); });
  },
  unsubscribe: function(el) { $(el).off('.esList'); }
});
Shiny.inputBindings.register(esListBinding, 'intstatgui.list');

function esVarCount(list) {
  var n = $(list).find('.es-opt.sel').length;
  $('#' + list.id + '_valda').text(n);
  // obligatorisk variabel utan val markeras med röd ram, som en påminnelse
  var box = $(list).closest('.es-var');
  var latest = $('#' + list.id + '_senaste').prop('checked');
  box.toggleClass('es-var-missing', box.hasClass('es-var-required') && n === 0 && !latest);
}
function esToggle(opt, on) {
  $(opt).toggleClass('sel', on).attr('aria-selected', $(opt).hasClass('sel'));
}
// Grått eftersom värdet inte går att kombinera med övriga val. Ett grått värde som redan är
// markerat går att avmarkera.
function esInteValbar(opt) {
  return $(opt).hasClass('es-tom') && !$(opt).hasClass('sel');
}
// Klicken fungerar som i en vanlig flervalslista, till exempel hos SCB. Ett klick väljer värdet
// och avmarkerar övriga, även de som ett sökord döljer. Ctrl+klick (Cmd på Mac) lägger till eller
// tar bort ett värde. Skift+klick väljer intervallet från det senast klickade värdet, och
// Ctrl+Skift+klick lägger till intervallet. Håller man ner musknappen och drar väljs intervallet
// från första raden. Drar man förbi listans kant scrollar den. Listan flyttar sig aldrig av sig
// själv. Gråa värden går inte att välja och hoppas över i ett intervall. Ett klick på ett rött
// värde (valt men inte längre möjligt) avmarkerar bara det värdet.
var esDrag = null;
function esDragApply(opt) {
  var i = esDrag.opts.index(opt);
  if (i < 0 || i === esDrag.last) return;
  esDrag.last = i;
  var lo = Math.min(esDrag.start, i), hi = Math.max(esDrag.start, i);
  esDrag.opts.each(function(k) {
    // inom intervallet: dragläget; utanför: som före draget
    esToggle(this, (k >= lo && k <= hi) ? esDrag.mark : esDrag.before[k]);
  });
  esVarCount(esDrag.list[0]);
}
function esDragAt(y) {
  // raden vid y-positionen, begränsad till listans synliga del
  var r = esDrag.list[0].getBoundingClientRect();
  var yy = Math.max(r.top + 1, Math.min(r.bottom - 1, y));
  var el = document.elementFromPoint(r.left + 5, yy);
  var opt = el ? $(el).closest('.es-opt')[0] : null;
  if (opt) esDragApply(opt);
}
$(document).on('mousedown', '.es-list .es-opt', function(e) {
  if (e.button !== 0) return;
  e.preventDefault();
  var grå = $(this).hasClass('es-tom');
  if (grå && !$(this).hasClass('sel')) return;
  var list = $(this).closest('.es-list');
  list.attr('data-user', '1').removeAttr('data-alla');
  if (grå) {
    esToggle(this, false);
    list.trigger('change');
    return;
  }
  var ctrl = !!(e.ctrlKey || e.metaKey);
  var opts = list.find('.es-opt').filter(function() {
    return this.style.display !== 'none' && !esInteValbar(this) && !$(this).hasClass('es-tom');
  });
  var ankare = e.shiftKey ? opts.index(list.data('esAnkare')) : -1;
  var before = opts.map(function() { return ctrl && $(this).hasClass('sel'); }).get();
  if (!ctrl) list.find('.es-opt.sel').not(opts).each(function() { esToggle(this, false); });
  if (!e.shiftKey) list.data('esAnkare', this);
  esDrag = {
    list: list, opts: opts, start: ankare >= 0 ? ankare : opts.index(this), last: -1,
    mark: !(ctrl && !e.shiftKey && $(this).hasClass('sel')), y: e.clientY, timer: null,
    before: before
  };
  esDragApply(this);
  esDrag.timer = setInterval(function() {
    if (!esDrag) return;
    var r = esDrag.list[0].getBoundingClientRect();
    var step = esDrag.y < r.top ? -Math.min(40, r.top - esDrag.y) :
               esDrag.y > r.bottom ? Math.min(40, esDrag.y - r.bottom) : 0;
    if (step !== 0) { esDrag.list[0].scrollTop += step; esDragAt(esDrag.y); }
  }, 50);
});
$(document).on('mousemove', function(e) {
  if (!esDrag) return;
  esDrag.y = e.clientY;
  esDragAt(e.clientY);
});
$(document).on('mouseup', function() {
  if (!esDrag) return;
  clearInterval(esDrag.timer);
  var list = esDrag.list;
  esDrag = null;
  list.trigger('change');
});
$(document).on('keydown', '.es-list .es-opt', function(e) {
  if (e.key === ' ' || e.key === 'Enter') {
    e.preventDefault();
    if ($(this).hasClass('es-tom') && !$(this).hasClass('sel')) return;
    $(this).closest('.es-list').attr('data-user', '1').removeAttr('data-alla').data('esAnkare', this);
    esToggle(this);
    $(this).closest('.es-list').trigger('change');
  }
});
// Shiny skickar NULL både före första valet och när allt avmarkerats;
// '_andrad' skiljer de två fallen åt
$(document).on('change', '.es-list', function() {
  esVarCount(this);
  Shiny.setInputValue(this.id + '_andrad', true);
});
$(document).on('click', '.es-mark-all, .es-unmark-all', function(e) {
  e.preventDefault();
  var list = $('#' + $(this).data('target'));
  var mark = $(this).hasClass('es-mark-all');
  list.attr('data-user', '1').removeAttr('data-alla');
  if (!mark) {
    list.find('.es-opt').each(function() { esToggle(this, false); });
  } else {
    // bara möjliga värden. Utan sökord följer markeringen sedan de möjliga värdena (data-alla)
    var dolda = list.find('.es-opt').filter(function() { return this.style.display === 'none'; });
    if (dolda.length === 0) list.attr('data-alla', '1');
    esMarkPossible(list.find('.es-opt').not(dolda));
  }
  list.trigger('change');
});
function esMarkPossible(opts) {
  opts.each(function() {
    if ($(this).hasClass('es-tom')) esToggle(this, false);
    else if (!esInteValbar(this)) esToggle(this, true);
  });
}
// 'Välj alla värden': alla möjliga rader markeras, även de som ett sökord döljer,
// och 'senaste perioden' tas bort eftersom den annars går före markeringen
Shiny.addCustomMessageHandler('esMarkAll', function(ids) {
  ids.forEach(function(id) {
    var latest = $('#' + id + '_senaste');
    if (latest.prop('checked')) latest.prop('checked', false).trigger('change');
    var list = $('#' + id);
    list.attr('data-user', '1').attr('data-alla', '1');
    esMarkPossible(list.find('.es-opt'));
    list.trigger('change');
  });
});
function esFilter(id) {
  var box = $('.es-search[data-target=\"' + id + '\"]');
  var q = (box.val() || '').toLowerCase().trim();
  var start = $('.es-search-start[data-target=\"' + id + '\"]').prop('checked');
  $('#' + id).find('.es-opt').each(function() {
    var t = this.textContent.toLowerCase();
    var hit = q === '' || (start ?
      t.split(/[\\s\\-\\/(),]+/).some(function(w) { return w.indexOf(q) === 0; }) :
      t.indexOf(q) >= 0);
    this.style.display = hit ? '' : 'none';
  });
}
$(document).on('input', '.es-search', function() { esFilter($(this).data('target')); });
$(document).on('change', '.es-search-start', function() { esFilter($(this).data('target')); });
// byte av indelning: servern hämtar kodlistan och skickar nya värden
$(document).on('change', 'select.es-indelning', function() {
  Shiny.setInputValue('es_indelning', {target: $(this).data('target'),
    codelist: this.value, nonce: Math.random()});
});
// Värdena som går att kombinera med övriga val. Övriga blir gråa.
Shiny.addCustomMessageHandler('esAvailable', function(vars) {
  vars.forEach(function(v) {
    var list = $('#' + v.id);
    var ok = {};
    v.koder.forEach(function(k) { ok[k] = true; });
    list.find('.es-opt').each(function() {
      $(this).toggleClass('es-tom', !ok[this.getAttribute('data-value')]);
    });
    $('#' + v.id + '_mojliga').text(' Möjliga ' + v.koder.length);
    var fore = list.find('.es-opt.sel').length;
    // 'Markera alla' gäller de värden som är möjliga just nu
    if (list.attr('data-alla')) {
      esMarkPossible(list.find('.es-opt'));
      if (list.find('.es-opt.sel').length !== fore) list.trigger('change');
      return;
    }
    // det användaren själv valt rörs inte; förval som inte längre går att kombinera byts ut
    if (list.attr('data-user')) return;
    list.find('.es-opt.sel.es-tom').each(function() { esToggle(this, false); });
    if (v.latest && list.find('.es-opt.sel').length === 0) {
      list.find('.es-opt').filter(function() {
        return this.getAttribute('data-value') === v.latest;
      }).each(function() { esToggle(this, true); });
    }
    if (list.find('.es-opt.sel').length !== fore || v.latest) list.trigger('change');
  });
});
Shiny.addCustomMessageHandler('esSetOptions', function(m) {
  var sel = $('#' + m.id);
  sel.html(m.html).scrollTop(0);
  $('.es-search[data-target=\"' + m.id + '\"]').val('');
  $('#' + m.id + '_totalt').text(m.total);
  sel.trigger('change');
});
// 'Hämta alltid senaste perioden' låser listan
$(document).on('change', 'input[id$=\"_senaste\"]', function() {
  var id = this.id.replace(/_senaste$/, '');
  $('#' + id).toggleClass('disabled', this.checked);
  esVarCount(document.getElementById(id));
  Shiny.setInputValue(this.id, this.checked);
});
"
