library(tidyverse)
source("R01_config.R")
conv <- function(w) {
  if (startsWith(w, "~")) return(substring(w, 2))
  if (grepl("\\*$", w))   return(paste0("\\b", sub("\\*$", "", w), "\\w*"))
  paste0("\\b", w, "\\b")
}
L <- read_csv("lexicon_sentimientos.csv", show_col_types = FALSE)
n0 <- nrow(L)

quitar <- tribble(
  ~sentimiento,   ~tipo,     ~w,
  "esperanza",    "palabra", "se puede",
  "esperanza",    "palabra", "pronto",
  "esperanza",    "palabra", "adelante",
  "seguridad",    "palabra", "seguro",
  "seguridad",    "palabra", "segura",
  "seguridad",    "palabra", "seguros",
  "enojo",        "palabra", "reclam*",
  "decepción",    "palabra", "mentira*",
  "cansancio",    "palabra", "ya no puedo",
  "preocupación", "palabra", "plazo*",
  "admiración",   "palabra", "ejemplo",
  "confusión",    "palabra", "alguien sabe",
  "confusión",    "palabra", "como se hace",
  "confusión",    "palabra", "como hago",
  "confusión",    "palabra", "me explican",
  "confusión",    "palabra", "alguien me explica",
  "admiración",   "emoji",   "🌟",
  "admiración",   "emoji",   "⭐",
  "esperanza",    "emoji",   "✨"
) %>% mutate(patron = if_else(tipo == "palabra", map_chr(w, conv), w)) %>% select(-w)

agregar <- tribble(
  ~sentimiento, ~tipo,     ~w,
  "seguridad",  "palabra", "seguro que",
  "seguridad",  "palabra", "segurisimo*",
  "curiosidad", "palabra", "alguien sabe",
  "curiosidad", "palabra", "como se hace",
  "curiosidad", "palabra", "como hago",
  "curiosidad", "palabra", "me explican",
  "curiosidad", "palabra", "alguien me explica"
) %>% mutate(patron = map_chr(w, conv), peso = 1) %>% select(sentimiento, tipo, patron, peso)

L <- L %>% anti_join(quitar, by = c("sentimiento", "tipo", "patron")) %>% bind_rows(agregar) %>% distinct()
write_excel_csv(L, "lexicon_sentimientos.csv")
cat("Entradas antes:", n0, "| despues:", nrow(L), "\n")
registrar("4b. Lexico", "Ajuste del diccionario tras revision de 30 ejemplos (quitar ambiguas, mover info-seeking a curiosidad)", n0, nrow(L), "lexicon_sentimientos.csv")