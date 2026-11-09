library(tidyverse); library(lubridate); library(digest); library(stringi)
source("R01_config.R")

B <- readRDS(file.path(DIR_PROC, "base.rds")); d <- B$d
n0 <- nrow(d)

hash_id <- function(x) {
  out <- rep(NA_character_, length(x)); ok <- !is.na(x)
  out[ok] <- unname(substr(vapply(paste0(SAL, x[ok]), digest, character(1),
                                  algo = "sha256", serialize = FALSE), 1, 10))
  out
}
URL_RE <- "(https?://|www\\.)\\S+"

d <- d %>%
  mutate(
    usuario     = hash_id(from_id),
    es_msg      = tipo == "message",
    texto_final = if_else(texto == "" & !is.na(sticker_emoji), sticker_emoji, texto),
    n_emojis    = stri_count_regex(texto_final, "\\p{So}"),
    emojis      = map_chr(stri_extract_all_regex(texto_final, "\\p{So}"),
                          ~ if (is.na(.x[1])) "" else paste(.x, collapse = " ")),
    texto_limpio = texto_final %>%
      str_to_lower() %>%
      str_remove_all(URL_RE) %>%
      str_remove_all("[@#]\\w+") %>%
      str_remove_all("\\p{So}") %>%
      str_replace_all("[^\\p{L}\\p{N}\\s]", " ") %>%
      str_squish(),
    n_palabras  = str_count(texto_limpio, "\\S+")
  ) %>%
  mutate(
    sin_texto       = es_msg & str_trim(texto_final) == "",
    solo_multimedia = sin_texto & (tiene_foto | tiene_archivo | !is.na(media_type)),
    solo_link       = es_msg & n_links > 0 & texto_limpio == "",
    solo_emoji      = es_msg & n_emojis > 0 & texto_limpio == "" & n_links == 0,
    muy_corto       = es_msg & !sin_texto & !solo_link & !solo_emoji & n_palabras <= 2
  )

# Repetidos: 3+ palabras; se conserva la primera aparicion
d <- d %>% arrange(unix, id) %>%
  mutate(clave_rep   = if_else(es_msg & n_palabras >= 3, texto_limpio, NA_character_),
         es_repetido = !is.na(clave_rep) & duplicated(clave_rep))

# Rafagas: 5+ mensajes seguidos del mismo usuario con pausas <= 10 s
raf <- d %>% filter(es_msg, !is.na(from_id)) %>% arrange(from_id, unix, id) %>%
  group_by(from_id) %>%
  mutate(corrida = cumsum(is.na(lag(unix)) | (unix - lag(unix)) > 10)) %>%
  group_by(from_id, corrida) %>% mutate(rafaga = n() >= 5) %>% ungroup() %>%
  select(id, rafaga)
d <- d %>% left_join(raf, by = "id") %>% mutate(rafaga = coalesce(rafaga, FALSE))

d <- d %>% mutate(
  apto_texto = es_msg & n_palabras >= 1 & !es_repetido & !solo_link,
  apto_emoji = es_msg & n_emojis > 0
)

# Nombres: solo en un archivo LOCAL (para inferir sexo luego). NO subir a GitHub.
usuarios_nombres <- d %>% filter(!is.na(usuario)) %>%
  group_by(usuario) %>% summarise(nombre = first(na.omit(nombre)), .groups = "drop")
saveRDS(usuarios_nombres, file.path(DIR_PROC, "_nombres_LOCAL.rds"))
d <- d %>% select(-nombre, -from_id)

# ---- Resumen ----
res <- tibble(
  marca = c("servicio", "sin_texto", "solo_multimedia", "solo_link", "solo_emoji",
            "muy_corto", "es_repetido", "rafaga", "apto_texto", "apto_emoji"),
  filas = c(sum(!d$es_msg), sum(d$sin_texto), sum(d$solo_multimedia), sum(d$solo_link),
            sum(d$solo_emoji), sum(d$muy_corto), sum(d$es_repetido), sum(d$rafaga),
            sum(d$apto_texto), sum(d$apto_emoji)))
print(res)
cat("Filas antes:", n0, "| despues:", nrow(d), "\n")
cat("Usuarios distintos:", n_distinct(d$usuario, na.rm = TRUE), "\n")

stopifnot(nrow(d) == n0)
registrar("2. Limpieza", "Marcas de ruido, repetidos, rafagas y hash de usuario; sin borrar filas",
          n0, nrow(d), "limpio.rds")
saveRDS(list(d = d, reac = B$reac, hash = B$hash), file.path(DIR_PROC, "limpio.rds"))
cat("OK: limpio.rds guardado.\n")