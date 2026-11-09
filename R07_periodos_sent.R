library(tidyverse); library(scales); library(ggalluvial); library(networkD3); library(htmlwidgets)
source("R01_config.R")

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
cols <- paste0("s_", sent)
base <- d %>% filter(usable)

orden <- c(read_csv(file.path(DIR_PROC, "periodos.csv"), show_col_types = FALSE)$etiqueta, "Fuera de temporada")
orden <- intersect(orden, unique(base$periodo))

long <- base %>% select(periodo, periodo_tipo, mes, all_of(cols)) %>%
  pivot_longer(all_of(cols), names_to = "sentimiento", values_to = "n") %>%
  mutate(sentimiento = sub("^s_", "", sentimiento))

# ---- 1. Tabla por periodo ----
tab <- long %>% group_by(periodo, sentimiento) %>%
  summarise(menciones = sum(n), mens_con = sum(n > 0), .groups = "drop") %>%
  left_join(base %>% count(periodo, name = "n_base"), by = "periodo") %>%
  mutate(tasa100 = mens_con / n_base * 100, periodo = factor(periodo, levels = orden))
write_csv(tab, file.path(DIR_TAB, "sentimientos_por_periodo.csv"))
orden_s <- tab %>% group_by(sentimiento) %>% summarise(t = sum(mens_con)) %>% arrange(t) %>% pull(sentimiento)

g1 <- ggplot(tab, aes(periodo, factor(sentimiento, levels = orden_s), fill = tasa100)) +
  geom_tile(color = "white") + geom_text(aes(label = sprintf("%.1f", tasa100)), size = 3) +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c", name = "% de\nmensajes") +
  labs(title = "Sentimientos por periodo (% de mensajes que lo expresan)", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave(file.path(DIR_GRAF, "g_heatmap_periodos.png"), g1, width = 11, height = 6.5, dpi = 150); print(g1)

# ---- 2. Invierno vs Verano vs Fuera de temporada ----
tab_t <- long %>% group_by(periodo_tipo, sentimiento) %>% summarise(m = sum(n > 0), .groups = "drop") %>%
  left_join(base %>% count(periodo_tipo, name = "nb"), by = "periodo_tipo") %>% mutate(tasa100 = m / nb * 100)
g2 <- ggplot(tab_t, aes(factor(sentimiento, levels = orden_s), tasa100, fill = periodo_tipo)) +
  geom_col(position = "dodge") + coord_flip() +
  scale_fill_manual(values = c("Invierno" = "#2171b5", "Verano" = "#f16913", "Fuera de temporada" = "#969696"), name = NULL) +
  labs(title = "Invierno vs Verano vs Fuera de temporada", x = NULL, y = "% de mensajes") + theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_invierno_verano.png"), g2, width = 9, height = 6.5, dpi = 150); print(g2)

# ---- 3. SANKEY periodo -> sentimiento ----
tab_s <- tab %>% filter(mens_con > 0)
tryCatch({
  g3 <- ggplot(tab_s, aes(axis1 = periodo, axis2 = sentimiento, y = mens_con)) +
    geom_alluvium(aes(fill = sentimiento), width = 1/10, alpha = 0.8) +
    geom_stratum(width = 1/10, fill = "grey95", color = "grey40") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    scale_x_discrete(limits = c("Periodo", "Sentimiento"), expand = c(.05, .05)) +
    labs(title = "Sankey: de cada periodo a los sentimientos", y = "Mensajes") +
    theme_minimal(base_size = 12) + theme(legend.position = "none")
  ggsave(file.path(DIR_GRAF, "g_sankey_periodos.png"), g3, width = 12, height = 8, dpi = 150); print(g3)
}, error = function(e) cat("Sankey estatico fallo:", conditionMessage(e), "\n"))

nodos <- tibble(name = c(as.character(orden), sent))
enl <- tab_s %>% mutate(source = match(as.character(periodo), nodos$name) - 1,
                        target = match(sentimiento, nodos$name) - 1, value = mens_con)
sk <- sankeyNetwork(Links = as.data.frame(enl), Nodes = as.data.frame(nodos),
                    Source = "source", Target = "target", Value = "value", NodeID = "name",
                    fontSize = 12, nodeWidth = 22, sinksRight = TRUE)
print(sk)
tryCatch(saveWidget(sk, file.path(getwd(), DIR_GRAF, "sankey_interactivo.html"), selfcontained = TRUE),
         error = function(e) saveWidget(sk, file.path(getwd(), DIR_GRAF, "sankey_interactivo.html"), selfcontained = FALSE))

# ---- 4. Series de tiempo mensuales y PICOS ----
men_base <- base %>% count(mes, name = "n_base")
mensual <- long %>% group_by(mes, sentimiento) %>% summarise(m = sum(n > 0), .groups = "drop") %>%
  left_join(men_base, by = "mes") %>% mutate(tasa = m / n_base * 100)
g4 <- ggplot(mensual %>% filter(n_base >= 50), aes(mes, tasa)) +
  geom_line(color = "#2171b5") + geom_point(size = 0.8) +
  facet_wrap(~ sentimiento, scales = "free_y", ncol = 3) +
  labs(title = "Evolucion mensual de cada sentimiento (% de mensajes del mes)", x = NULL, y = "%") +
  theme_minimal(base_size = 10)
ggsave(file.path(DIR_GRAF, "g_series_mensuales.png"), g4, width = 12, height = 9, dpi = 150); print(g4)

picos <- mensual %>% filter(n_base >= 100, m >= 10) %>% group_by(sentimiento) %>%
  slice_max(tasa, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(sentimiento, mes, mensajes_con = m, mensajes_mes = n_base, tasa) %>% arrange(desc(tasa))
cat("\n--- MES PICO de cada sentimiento (meses con >=100 mensajes) ---\n"); print(picos)
write_csv(picos, file.path(DIR_TAB, "picos_mensuales.csv"))

registrar("6. Periodos", "Sentimientos por periodo: heatmap, Sankey, series mensuales y picos", nrow(base), nrow(base), "graficos")
cat("OK. Graficos en outputs/graficos\n")