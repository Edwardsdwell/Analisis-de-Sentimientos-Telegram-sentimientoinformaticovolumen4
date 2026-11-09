library(tidyverse); library(stringi); library(scales); library(tidytext)
source("R01_config.R")

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
cols <- paste0("s_", sent)
L <- read_csv("lexicon_sentimientos.csv", show_col_types = FALSE)

# A. NRC / syuzhet (8 emociones) vs diccionario propio (15) 
bt <- d %>% filter(usable, n_palabras >= 1)          # misma base para ambos metodos
ruta_nrc <- file.path(DIR_PROC, "nrc.rds")
if (file.exists(ruta_nrc)) {
  nrc <- readRDS(ruta_nrc)
} else {
  cat("Corriendo syuzhet sobre", nrow(bt), "mensajes (puede tardar unos minutos)...\n")
  nrc <- syuzhet::get_nrc_sentiment(bt$texto_limpio, language = "spanish")
  saveRDS(nrc, ruta_nrc)
}
stopifnot(nrow(nrc) == nrow(bt))

lab <- c(anger = "enojo (NRC)", anticipation = "anticipación (NRC)", disgust = "aversión (NRC)",
         fear = "miedo (NRC)", joy = "alegría (NRC)", sadness = "tristeza (NRC)",
         surprise = "sorpresa (NRC)", trust = "confianza (NRC)")
syu <- tibble(sentimiento = unname(lab[names(lab)]),
              mensajes = colSums(nrc[, names(lab)] > 0), fuente = "NRC / syuzhet (8)")
own <- tibble(sentimiento = sent, mensajes = colSums(bt[, cols] > 0), fuente = "Diccionario propio (15)")
comp <- bind_rows(syu, own) %>% mutate(pct = mensajes / nrow(bt))
print(comp %>% arrange(fuente, desc(pct)), n = 30)
cat("\nPolaridad NRC: positivos", percent(mean(nrc$positive > nrc$negative), 0.1),
    "| negativos", percent(mean(nrc$negative > nrc$positive), 0.1), "\n")

g1 <- ggplot(comp, aes(reorder_within(sentimiento, pct, fuente), pct, fill = fuente)) +
  geom_col(show.legend = FALSE) + coord_flip() + facet_wrap(~ fuente, scales = "free_y") +
  scale_x_reordered() + scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#969696", "#2171b5")) +
  labs(title = "Sentimientos: libreria NRC (8) vs diccionario propio (15)",
       x = NULL, y = "% de mensajes analizados") + theme_minimal(base_size = 11)
ggsave(file.path(DIR_GRAF, "g_nrc_vs_propio.png"), g1, width = 12, height = 6, dpi = 150); print(g1)
write_csv(comp, file.path(DIR_TAB, "comparacion_nrc_propio.csv"))

# B. Reacciones (emojis que la gente pone a los mensajes) 
Lemo <- L %>% filter(tipo == "emoji") %>% distinct(patron, .keep_all = TRUE) %>%
  select(emoji = patron, sentimiento)
reac <- B$reac %>% filter(!is.na(emoji)) %>%
  mutate(emoji = stri_replace_all_regex(emoji, "[\\uFE0F\\x{1F3FB}-\\x{1F3FF}]", "")) %>%
  left_join(Lemo, by = "emoji") %>%
  mutate(sentimiento = if_else(emoji == "❤\u200D🔥", "admiración", sentimiento)) %>%
  left_join(d %>% select(id, periodo, periodo_tipo, mes), by = "id")

cat("\nReacciones totales:", sum(reac$n), "| en", n_distinct(reac$id), "mensajes\n")
top_e <- reac %>% group_by(emoji, sentimiento) %>% summarise(n = sum(n), .groups = "drop") %>% arrange(desc(n))
print(head(top_e, 15))
cat("Reacciones con sentimiento asignado:",
    percent(sum(reac$n[!is.na(reac$sentimiento)]) / sum(reac$n), 0.1), "\n")

re_s <- reac %>% filter(!is.na(sentimiento)) %>% group_by(sentimiento) %>%
  summarise(reacciones = sum(n), .groups = "drop") %>% arrange(desc(reacciones))
g2 <- ggplot(re_s, aes(reorder(sentimiento, reacciones), reacciones)) +
  geom_col(fill = "#f16913") + coord_flip() +
  labs(title = "Sentimiento de las REACCIONES (emojis puestos a los mensajes)", x = NULL, y = "Reacciones") +
  theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_reacciones_sentimiento.png"), g2, width = 9, height = 6, dpi = 150); print(g2)
write_csv(re_s, file.path(DIR_TAB, "reacciones_por_sentimiento.csv"))

registrar("7. Comparacion y reacciones", "NRC/syuzhet (8) vs propio (15) y sentimiento de reacciones",
          nrow(bt), nrow(bt), "g_nrc_vs_propio.png, g_reacciones_sentimiento.png")
cat("OK.\n")