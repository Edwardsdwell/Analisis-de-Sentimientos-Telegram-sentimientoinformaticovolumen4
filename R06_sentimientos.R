library(tidyverse); library(stringi); library(scales)
source("R01_config.R")

L <- read_csv("lexicon_sentimientos.csv", show_col_types = FALSE)
B <- readRDS(file.path(DIR_PROC, "listo.rds")); d <- B$d
sent <- unique(L$sentimiento)

d$usable   <- (d$apto_texto | d$solo_emoji) & d$n_palabras <= 50   # anuncios largos se marcan, no se analizan
d$texto_sa <- stri_trans_general(d$texto_limpio, "Latin-ASCII")

rx_pal <- L %>% filter(tipo == "palabra") %>% group_by(sentimiento) %>%
  summarise(rx = paste(patron, collapse = "|"))
rx_emo <- L %>% filter(tipo == "emoji") %>% group_by(sentimiento) %>%
  summarise(rx = paste0("[", paste(patron, collapse = ""), "]"))

for (s in sent) {
  rp <- rx_pal$rx[rx_pal$sentimiento == s]; re <- rx_emo$rx[rx_emo$sentimiento == s]
  cp <- if (length(rp) == 1) str_count(d$texto_sa, rp) else 0L
  ce <- if (length(re) == 1) str_count(d$emojis, re) else 0L
  d[[paste0("s_", s)]] <- (cp + ce) * d$usable
}

cols <- paste0("s_", sent)
M <- as.matrix(d[, cols])
d$n_senal  <- rowSums(M)
d$sent_dom <- ifelse(!d$usable, NA_character_,
                     ifelse(d$n_senal == 0, "Sin señal", sent[max.col(M, ties.method = "first")]))

res <- tibble(sentimiento = sent, menciones = colSums(M), mensajes = colSums(M > 0)) %>%
  mutate(pct_mensajes = mensajes / sum(d$usable)) %>% arrange(desc(mensajes))
print(res)
cat("Mensajes analizados:", sum(d$usable), "| con al menos 1 sentimiento:",
    sum(d$usable & d$n_senal > 0), "(",
    percent(sum(d$usable & d$n_senal > 0) / sum(d$usable), 0.1), ")\n")

g <- ggplot(res, aes(reorder(sentimiento, mensajes), mensajes)) +
  geom_col(fill = "#2171b5") +
  geom_text(aes(label = percent(pct_mensajes, accuracy = 0.1)), hjust = -0.1, size = 3.2) +
  coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Sentimientos en el chat (diccionario propio, 15 categorias)",
       x = NULL, y = "Mensajes con el sentimiento", caption = "Base: mensajes analizables") +
  theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_sentimientos_general.png"), g, width = 9, height = 6, dpi = 150)
print(g)
write_csv(res, file.path(DIR_TAB, "sentimientos_general.csv"))

# Revision de calidad: 2 mensajes al azar por sentimiento (solo en tu consola, no se guarda)
set.seed(SEMILLA)
for (s in sent) {
  ej <- d %>% filter(.data[[paste0("s_", s)]] > 0) %>% slice_sample(n = 2)
  cat("\n[", s, "]\n", paste0("  - ", substr(ej$texto_final, 1, 90), collapse = "\n"), "\n")
}

registrar("5. Sentimientos", "Conteo de 15 sentimientos (palabras y emojis) por mensaje", nrow(d), nrow(d), "listo_sent.rds")
saveRDS(list(d = d, reac = B$reac, hash = B$hash, sent = sent), file.path(DIR_PROC, "listo_sent.rds"))
cat("OK: listo_sent.rds guardado y grafico g_sentimientos_general.png creado.\n")