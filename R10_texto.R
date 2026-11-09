library(tidyverse); library(tidytext); library(stopwords); library(stringi); library(scales)
source("R01_config.R")

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d
bt <- d %>% filter(usable, n_palabras >= 1) %>%
  select(id, periodo, periodo_tipo, mes, texto_sa, n_palabras)

# ---- Stopwords: snowball sin tildes, CONSERVANDO negaciones + muletillas del chat ----
sw <- stri_trans_general(stopwords("es", source = "snowball"), "Latin-ASCII")
sw <- setdiff(sw, c("no", "ni", "sin", "pero", "nada", "nunca"))
extra <- c("q", "x", "d", "xq", "pq", "pa", "ahi", "aqui", "buenas", "buenos", "dias", "tardes",
           "noches", "hola", "holaa", "favor", "porfa", "alguien", "vez", "hacer", "puede", "pueden",
           "tengo", "tiene", "tienen", "hay", "va", "van", "ver", "solo", "tan", "mas", "muy", "asi",
           "bien", "ahora", "aun", "ya", "si", "ser", "ahi", "creo", "gente", "xd",
           "lic", "licenciado", "sabe", "saben", "disculpen", "pero", "hoy", "dia", "buen")
stop_all <- unique(c(sw, extra))

tokens <- bt %>% unnest_tokens(palabra, texto_sa) %>%
  filter(!palabra %in% stop_all,
         nchar(palabra) >= 3,
         !str_detect(palabra, "^j[aeiou]j"))
cat("Tokens utiles:", nrow(tokens), "| palabras distintas:", n_distinct(tokens$palabra), "\n")
saveRDS(tokens, file.path(DIR_PROC, "tokens.rds"))

# ---- 1. Palabras mas frecuentes ----
frec <- tokens %>% count(palabra, sort = TRUE)
print(head(frec, 25), n = 25)
g1 <- frec %>% slice_head(n = 25) %>%
  ggplot(aes(reorder(palabra, n), n)) + geom_col(fill = "#2171b5") + coord_flip() +
  labs(title = "25 palabras mas frecuentes del chat", x = NULL, y = "Frecuencia") + theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_top_palabras.png"), g1, width = 8, height = 7, dpi = 150); print(g1)
write_csv(frec, file.path(DIR_TAB, "frecuencias_palabras.csv"))

# ---- 2. TF-IDF por tipo de temporada y por periodo ----
tf_tipo <- tokens %>% count(periodo_tipo, palabra) %>% filter(n >= 5) %>%
  bind_tf_idf(palabra, periodo_tipo, n) %>% filter(tf_idf > 0)
g2 <- tf_tipo %>% group_by(periodo_tipo) %>% slice_max(tf_idf, n = 12, with_ties = FALSE) %>% ungroup() %>%
  ggplot(aes(reorder_within(palabra, tf_idf, periodo_tipo), tf_idf, fill = periodo_tipo)) +
  geom_col(show.legend = FALSE) + coord_flip() + facet_wrap(~ periodo_tipo, scales = "free_y") +
  scale_x_reordered() + scale_fill_manual(values = c("Invierno" = "#2171b5", "Verano" = "#f16913", "Fuera de temporada" = "#969696")) +
  labs(title = "Palabras distintivas (TF-IDF) por tipo de temporada", x = NULL, y = "TF-IDF") + theme_minimal(base_size = 11)
ggsave(file.path(DIR_GRAF, "g_tfidf_temporada.png"), g2, width = 12, height = 5.5, dpi = 150); print(g2)

tf_per <- tokens %>% count(periodo, palabra) %>% filter(n >= 5) %>%
  bind_tf_idf(palabra, periodo, n) %>% filter(tf_idf > 0)
g3 <- tf_per %>% group_by(periodo) %>% slice_max(tf_idf, n = 7, with_ties = FALSE) %>% ungroup() %>%
  ggplot(aes(reorder_within(palabra, tf_idf, periodo), tf_idf)) +
  geom_col(fill = "#6a51a3") + coord_flip() + facet_wrap(~ periodo, scales = "free_y", ncol = 3) +
  scale_x_reordered() + labs(title = "Palabras distintivas (TF-IDF) por periodo", x = NULL, y = "TF-IDF") +
  theme_minimal(base_size = 10)
ggsave(file.path(DIR_GRAF, "g_tfidf_periodos.png"), g3, width = 13, height = 9, dpi = 150); print(g3)
write_csv(tf_per, file.path(DIR_TAB, "tfidf_periodos.csv"))

# ---- 3. Bigramas ----
bi <- bt %>% unnest_tokens(bigrama, texto_sa, token = "ngrams", n = 2) %>% filter(!is.na(bigrama)) %>%
  separate(bigrama, c("w1", "w2"), sep = " ", remove = FALSE) %>%
  filter(!w1 %in% stop_all, !w2 %in% stop_all, nchar(w1) >= 2, nchar(w2) >= 2,
         !str_detect(bigrama, "j[aeiou]j")) %>%
  count(bigrama, sort = TRUE)
print(head(bi, 20), n = 20)
g4 <- bi %>% slice_head(n = 20) %>%
  ggplot(aes(reorder(bigrama, n), n)) + geom_col(fill = "#238b45") + coord_flip() +
  labs(title = "20 bigramas mas frecuentes", x = NULL, y = "Frecuencia") + theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_bigramas.png"), g4, width = 8, height = 7, dpi = 150); print(g4)
write_csv(bi, file.path(DIR_TAB, "bigramas.csv"))

# ---- 4. Linguistica superficial: riqueza lexica y ley de Zipf ----
riq <- tokens %>% count(periodo_tipo, palabra) %>% group_by(periodo_tipo) %>%
  summarise(tokens = sum(n), tipos = n(), hapax = sum(n == 1), ttr = round(tipos / tokens, 3), .groups = "drop")
lon <- bt %>% group_by(periodo_tipo) %>%
  summarise(mensajes = n(), palabras_media = round(mean(n_palabras), 1), mediana = median(n_palabras), .groups = "drop")
cat("\n--- Riqueza lexica ---\n"); print(riq); cat("\n--- Longitud de mensajes ---\n"); print(lon)
write_csv(left_join(riq, lon, by = "periodo_tipo"), file.path(DIR_TAB, "linguistica_superficial.csv"))

zipf <- bt %>% unnest_tokens(palabra, texto_sa) %>% count(palabra, sort = TRUE) %>% mutate(rango = row_number())
g5 <- ggplot(zipf, aes(rango, n)) + geom_point(alpha = 0.4, color = "#08519c") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Ley de Zipf en el chat (todas las palabras)", x = "Rango (log)", y = "Frecuencia (log)") +
  theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_zipf.png"), g5, width = 8, height = 5.5, dpi = 150); print(g5)

registrar("9. Texto", "Frecuencias, TF-IDF (temporada y periodo), bigramas, riqueza lexica y Zipf",
          nrow(bt), nrow(tokens), "tokens.rds, g_top_palabras/tfidf/bigramas/zipf")
cat("OK.\n")