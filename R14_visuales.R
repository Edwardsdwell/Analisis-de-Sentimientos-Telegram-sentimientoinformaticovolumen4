library(tidyverse); library(ggraph)
source("R01_config.R")

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
tokens <- readRDS(file.path(DIR_PROC, "tokens.rds"))

# ---- Nubes de palabras ----
sel <- intersect(c("preocupación", "frustración", "confusión", "gratitud", "alegría", "curiosidad"), sent)
wc <- map_dfr(sel, function(s) {
  ids <- d$id[d$usable & d[[paste0("s_", s)]] > 0]
  tokens %>% filter(id %in% ids) %>% count(palabra, sort = TRUE) %>% slice_head(n = 25) %>% mutate(sentimiento = s)
})
set.seed(SEMILLA)
g_wc <- ggplot(wc, aes(label = palabra, size = n, color = sentimiento)) +
  ggwordcloud::geom_text_wordcloud(rm_outside = FALSE, area_corr = TRUE, shape = "square") +
  scale_size_area(max_size = 9) + facet_wrap(~ sentimiento, ncol = 3) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Palabras que acompanan a cada sentimiento")
ggsave(file.path(DIR_GRAF, "g_nubes_sentimientos.png"), g_wc, width = 14, height = 9, dpi = 150, bg = "white")

# ---- Red de bigramas ----
bi <- read_csv(file.path(DIR_TAB, "bigramas.csv"), show_col_types = FALSE) %>% slice_head(n = 70) %>%
  separate(bigrama, c("de", "a"), sep = " ")
gr <- igraph::graph_from_data_frame(bi)
set.seed(SEMILLA)
g_net <- ggraph(gr, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), color = "#2171b5", show.legend = FALSE) +
  geom_node_point(size = 3, color = "#08306b") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
  theme_void() + theme(plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Red de palabras: los 70 bigramas mas frecuentes")
ggsave(file.path(DIR_GRAF, "g_red_bigramas.png"), g_net, width = 12, height = 8, dpi = 150, bg = "white")

registrar("14b. Ajuste visual", "Nubes de palabras y red de bigramas con fondo blanco", nrow(wc), nrow(wc), "g_nubes_sentimientos.png, g_red_bigramas.png")
cat("OK. Abre los dos PNG en outputs/graficos y revisa que se vean bien.\n")