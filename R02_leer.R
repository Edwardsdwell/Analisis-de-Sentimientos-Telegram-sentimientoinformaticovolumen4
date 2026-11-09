library(tidyverse); library(jsonlite); library(lubridate); library(digest); library(rlang)
source("R01_config.R")

tmp <- file.path(tempdir(), "tg"); dir.create(tmp, showWarnings = FALSE)
unzip(RUTA_ZIP, files = RUTA_JSON_DENTRO, exdir = tmp, overwrite = TRUE)
ruta_json <- file.path(tmp, RUTA_JSON_DENTRO)

hash_zip <- digest(RUTA_ZIP, algo = "sha256", file = TRUE)
cat("SHA-256 del zip:", hash_zip, "\n")

J <- fromJSON(ruta_json, simplifyVector = FALSE)
msgs <- J$messages
cat("Mensajes leidos:", length(msgs), "\n")

campo <- function(m, nm) { v <- m[[nm]]; if (is.null(v) || length(v) == 0) NA else v[[1]] }

aplanar <- function(t) {
  if (is.null(t)) return("")
  if (is.character(t)) return(paste(t, collapse = ""))
  if (is.list(t)) {
    partes <- vapply(t, function(x) {
      if (is.character(x)) paste(x, collapse = "")
      else if (is.list(x) && !is.null(x$text)) as.character(x$text)
      else ""
    }, character(1))
    return(paste(partes, collapse = ""))
  }
  ""
}
n_links <- function(t) {
  if (!is.list(t)) return(0L)
  sum(vapply(t, function(x) is.list(x) && !is.null(x$type) && x$type %in% c("link", "text_link"), logical(1)))
}
n_reac <- function(r) if (is.list(r) && length(r) > 0) sum(vapply(r, function(x) as.numeric(x$count %||% 0), numeric(1))) else 0
tabla_reac <- function(m) {
  r <- m$reactions
  if (!is.list(r) || length(r) == 0) return(NULL)
  tibble(id = m$id,
         emoji = vapply(r, function(x) as.character(x$emoji %||% NA_character_), character(1)),
         n = vapply(r, function(x) as.numeric(x$count %||% 0), numeric(1)))
}

d <- map_dfr(msgs, function(m) {
  tibble(
    id = m$id, tipo = m$type,
    fecha = ymd_hms(m$date, tz = "America/La_Paz"),
    unix = as.numeric(m$date_unixtime),
    from_id = if (!is.null(m$from_id)) m$from_id else campo(m, "actor_id"),
    nombre  = if (!is.null(m$from)) m$from else campo(m, "actor"),
    action = campo(m, "action"), titulo = campo(m, "title"),
    reply_id = campo(m, "reply_to_message_id"),
    reenviado = !is.null(m$forwarded_from),
    media_type = campo(m, "media_type"), sticker_emoji = campo(m, "sticker_emoji"),
    tiene_foto = !is.null(m$photo), tiene_archivo = !is.null(m$file),
    editado = !is.null(m$edited_unixtime),
    texto_lista = is.list(m$text),
    texto = aplanar(m$text), n_links = n_links(m$text), n_reac = n_reac(m$reactions)
  )
})
reac <- map_dfr(msgs, tabla_reac)

cat("Filas x columnas:", dim(d), "\n")
print(table(d$tipo))
cat("Rango:", format(min(d$fecha)), "->", format(max(d$fecha)), "\n")
cat("Textos tipo lista aplanados:", sum(d$texto_lista), "\n")
cat("Mensajes con reacciones:", sum(d$n_reac > 0), "\n")
cat("Ids duplicados:", sum(duplicated(d$id)), "\n")

registrar("1. Ingesta", paste("Lectura de result.json; SHA256 zip", substr(hash_zip, 1, 12)),
          length(msgs), nrow(d), "base.rds")
saveRDS(list(d = d, reac = reac, hash = hash_zip), file.path(DIR_PROC, "base.rds"))