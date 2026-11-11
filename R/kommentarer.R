# Fliken "4. Information": uppgifterna om de valda tabellerna, en ruta per tabell.

# Uppgifterna om en tabell, som list(rows, error). Ett fel gäller bara den tabellen.
es_table_comments <- function(backend, id, lang, cache) {
  key <- paste(lang, id)
  if (!is.null(cache[[key]])) return(cache[[key]])
  res <- tryCatch(list(rows = as.data.frame(backend$comments(id, lang = lang))),
                  error = function(e) list(error = es_error_text(e)))
  if (is.null(res$error)) cache[[key]] <- res
  res
}

es_comment_item <- function(row) {
  div(
    class = "es-kommentar",
    div(class = "es-kommentar-varde", row$typ),
    if (!is.na(row$url) && grepl("^https?://", row$url)) {
      tags$a(href = row$url, target = "_blank", rel = "noopener noreferrer",
             if (is.na(row$text)) row$url else row$text)
    } else {
      tags$span(row$text)
    }
  )
}

es_comment_card <- function(id, title, res) {
  card(
    class = "es-kommentar-tabell mb-3", fill = FALSE,
    card_header(tags$code(class = "me-2", id), title),
    card_body(
      fill = FALSE, fillable = FALSE,
      if (!is.null(res$error)) {
        div(class = "text-danger", paste0("Kunde inte hämta uppgifterna om ", id, ": ", res$error))
      } else {
        lapply(seq_len(nrow(res$rows)), function(i) es_comment_item(res$rows[i, , drop = FALSE]))
      }
    )
  )
}
