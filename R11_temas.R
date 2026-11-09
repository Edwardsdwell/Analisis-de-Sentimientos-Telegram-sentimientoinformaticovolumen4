library(tidyverse); library(tidytext); library(topicmodels); library(scales)
source("R01_config.R")

K <- 6                                   # numero de temas (se puede cambiar)
B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
cols <- paste0("s_", sent)
tokens <- readRDS(file.path(DIR_PROC, "tokens.rds"))

# ---- Matriz documento-termino ----
voc <- tokens %>% count(palabra) %>% filter(n >= 8)
cnt <- tokens %>% semi_join(voc, by = "palabra") %>% count(id, palabra)
ndoc <- cnt %>% group_by(id) %>% summarise(nt = sum(n)) %>% filter(nt >= 3)
cnt <- cnt %>% semi_join(ndoc, by = "id")
dtm <- cast_dtm(cnt, document = id, term = palabra, value = n)
cat("DTM:", nrow(dtm), "mensajes x", ncol(dtm), "palabras\n")

ruta_lda <- file.path(DIR_PROC, paste0("lda_k", K, ".rds"))
if (file.exists(ruta_lda)) {
  lda <- readRDS(ruta_lda)
} else {
  cat("Entrenando LDA con K =", K, "(1-3 min)...\n")
  lda <- LDA(dtm, k = K, method = "Gibbs", control = list(seed = SEMILLA, iter = 400, burnin = 100))
  saveRDS(lda, ruta_lda)
}

# ---- Palabras de cada tema ----
beta <- tidy(lda, matrix = "beta")
top  <- beta %>% group_by(topic) %>% slice_max(beta, n = 10, with_ties = FALSE) %>% ungroup()
etq  <- top %>% group_by(topic) %>% slice_head(n = 3) %>%
  summarise(tema = paste0("T", first(topic), ": ", paste(term, collapse = " / ")), .groups = "drop")
nombres <- c("Materias y temporada", "Inscripción y pagos", "Clases y comunicados",
             "Adición, retiro y horarios", "Docentes y contenidos", "Grupos y paralelos")
etq$tema <- paste0("T", etq$topic, ": ", nombres[etq$topic])
cat("\n--- Palabras principales de cada tema ---\n")
print(top %>% group_by(topic) %>% summarise(palabras = paste(term, collapse = ", ")), n = K, width = 200)

g1 <- top %>% left_join(etq, by = "topic") %>%
  ggplot(aes(reorder_within(term, beta, tema), beta)) + geom_col(fill = "#2171b5") +
  coord_flip() + facet_wrap(~ tema, scales = "free_y", ncol = 3) + scale_x_reordered() +
  labs(title = paste("Temas del chat (LDA, K =", K, ")"), x = NULL, y = "Peso de la palabra en el tema") +
  theme_minimal(base_size = 10)
ggsave(file.path(DIR_GRAF, "g_temas_lda.png"), g1, width = 13, height = 7, dpi = 150); print(g1)

# ---- Tema dominante de cada mensaje ----
gam <- tidy(lda, matrix = "gamma") %>% group_by(document) %>%
  slice_max(gamma, n = 1, with_ties = FALSE) %>% ungroup() %>%
  rename(topic_dom = topic, gamma_max = gamma)
dj <- d %>% filter(usable) %>% mutate(document = as.character(id)) %>%
  inner_join(gam, by = "document") %>% left_join(etq, by = c("topic_dom" = "topic"))
cat("\n--- Mensajes por tema ---\n")
print(dj %>% count(tema) %>% mutate(pct = percent(n / sum(n), 0.1)), width = 150)
saveRDS(dj %>% select(id, topic_dom, tema, gamma_max), file.path(DIR_PROC, "temas.rds"))

# ---- Tema x sentimiento ----
glob <- tibble(sentimiento = sent, tasa_glob = colMeans(dj[cols] > 0) * 100)
th <- dj %>% pivot_longer(all_of(cols), names_to = "sentimiento", values_to = "n") %>%
  mutate(sentimiento = sub("^s_", "", sentimiento)) %>%
  group_by(tema, sentimiento) %>% summarise(m = sum(n > 0), .groups = "drop") %>%
  left_join(dj %>% count(tema, name = "nt"), by = "tema") %>%
  left_join(glob, by = "sentimiento") %>%
  mutate(tasa100 = m / nt * 100, lift = tasa100 / tasa_glob)
write_csv(th, file.path(DIR_TAB, "tema_x_sentimiento.csv"))

g2 <- ggplot(th, aes(sentimiento, tema, fill = tasa100)) + geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f", tasa100)), size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c", name = "% de\nmensajes") +
  labs(title = "Sentimiento segun el tema del mensaje", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave(file.path(DIR_GRAF, "g_tema_sentimiento.png"), g2, width = 12, height = 5.5, dpi = 150); print(g2)

cat("\n--- Combinaciones tema-sentimiento mas sobrerrepresentadas (lift, >=10 mensajes) ---\n")
print(th %>% filter(m >= 10) %>% arrange(desc(lift)) %>%
        transmute(tema, sentimiento, mensajes = m, pct = round(tasa100, 1), veces_mas_que_el_promedio = round(lift, 1)) %>%
        head(10), width = 150)

# ---- Tema por periodo ----
orden <- intersect(c(read_csv(file.path(DIR_PROC, "periodos.csv"), show_col_types = FALSE)$etiqueta, "Fuera de temporada"),
                   unique(dj$periodo))
tp <- dj %>% count(periodo, tema) %>% group_by(periodo) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  mutate(periodo = factor(periodo, levels = orden))
g3 <- ggplot(tp, aes(periodo, p, fill = tema)) + geom_col() +
  scale_y_continuous(labels = percent) + scale_fill_brewer(palette = "Set2") +
  labs(title = "De que se habla en cada periodo", x = NULL, y = "% de mensajes", fill = NULL) +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom")
ggsave(file.path(DIR_GRAF, "g_tema_periodo.png"), g3, width = 12, height = 6.5, dpi = 150); print(g3)

registrar("10. Temas", paste("LDA con K =", K, "; tema dominante por mensaje; cruce con sentimiento y periodo"),
          nrow(dtm), nrow(dj), "temas.rds, g_temas_lda, g_tema_sentimiento, g_tema_periodo")
cat("OK.\n")