library(tidyverse); library(lubridate); library(scales)
source("R01_config.R")

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
cols <- paste0("s_", sent)

POS <- intersect(c("alegría", "gratitud", "esperanza", "seguridad", "admiración", "tranquilidad"), sent)
NEG <- intersect(c("tristeza", "enojo", "decepción", "cansancio", "preocupación", "frustración", "confusión"), sent)

base <- d %>% filter(usable) %>%
  mutate(pos = rowSums(across(all_of(paste0("s_", POS)))),
         neg = rowSums(across(all_of(paste0("s_", NEG)))))
orden <- intersect(c(read_csv(file.path(DIR_PROC, "periodos.csv"), show_col_types = FALSE)$etiqueta, "Fuera de temporada"),
                   unique(base$periodo))

#A. INDICE EMOCIONAL = (pos - neg) / (pos + neg) 
cat("Indice emocional GENERAL:", round((sum(base$pos) - sum(base$neg)) / (sum(base$pos) + sum(base$neg)), 3),
    "| positivos:", sum(base$pos), "| negativos:", sum(base$neg), "\n")

mens <- base %>% group_by(mes) %>% summarise(n = n(), pos = sum(pos), neg = sum(neg), .groups = "drop") %>%
  filter(n >= 50) %>% mutate(indice = (pos - neg) / pmax(pos + neg, 1), pos100 = pos / n * 100, neg100 = neg / n * 100)
write_csv(mens, file.path(DIR_TAB, "indice_emocional_mensual.csv"))

ev <- d %>% filter(action == "edit_group_title") %>% transmute(f = as_date(fecha))
g1 <- ggplot(mens, aes(mes, indice)) +
  geom_vline(data = ev, aes(xintercept = f), color = "grey70", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = "#2171b5", linewidth = 1) + geom_point(color = "#2171b5") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "Indice emocional mensual  (+1 = todo positivo, -1 = todo negativo)",
       subtitle = "Lineas punteadas = cambios de nombre del grupo (cambio de temporada)", x = NULL, y = "Indice") +
  theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_indice_emocional.png"), g1, width = 11, height = 5, dpi = 150); print(g1)

g2 <- mens %>% pivot_longer(c(pos100, neg100), names_to = "tipo", values_to = "v") %>%
  mutate(tipo = recode(tipo, pos100 = "Positivo", neg100 = "Negativo")) %>%
  ggplot(aes(mes, v, color = tipo)) + geom_line(linewidth = 1) +
  scale_color_manual(values = c(Positivo = "#2ca25f", Negativo = "#d6604d"), name = NULL) +
  labs(title = "Emociones positivas vs negativas por mes", x = NULL, y = "Menciones por 100 mensajes") +
  theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_pos_vs_neg.png"), g2, width = 11, height = 5, dpi = 150); print(g2)

ip <- base %>% group_by(periodo) %>% summarise(n = n(), indice = (sum(pos) - sum(neg)) / pmax(sum(pos) + sum(neg), 1), .groups = "drop") %>%
  mutate(periodo = factor(periodo, levels = orden))
cat("\n--- Indice por periodo ---\n"); print(ip %>% arrange(periodo))
g3 <- ggplot(ip, aes(periodo, indice, fill = indice > 0)) + geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#d6604d", "#2ca25f")) + geom_hline(yintercept = 0) +
  labs(title = "Indice emocional por periodo", x = NULL, y = "Indice") +
  theme_minimal(base_size = 12) + theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave(file.path(DIR_GRAF, "g_indice_periodos.png"), g3, width = 10, height = 5, dpi = 150); print(g3)

#  B. NUCLEO (100+ mensajes) vs RESTO 
us_n <- d %>% filter(es_msg, !is.na(usuario)) %>% count(usuario, name = "n_msgs")
nucleo <- us_n %>% filter(n_msgs >= 100) %>% pull(usuario)
cat("\nUsuarios del nucleo (100+ mensajes):", length(nucleo), "\n")      # esperado ~26
base$grupo <- if_else(base$usuario %in% nucleo, "Nucleo (100+ mensajes)", "Resto")

res_g <- base %>% group_by(grupo) %>% summarise(mensajes = n(), con_sent = round(mean(n_senal > 0) * 100, 1),
                                                indice = round((sum(pos) - sum(neg)) / pmax(sum(pos) + sum(neg), 1), 3), .groups = "drop")
print(res_g)

cmp <- base %>% select(grupo, all_of(cols)) %>% pivot_longer(-grupo, names_to = "sentimiento", values_to = "n") %>%
  mutate(sentimiento = sub("^s_", "", sentimiento)) %>% group_by(grupo, sentimiento) %>%
  summarise(pct = mean(n > 0) * 100, .groups = "drop")
write_csv(cmp, file.path(DIR_TAB, "nucleo_vs_resto.csv"))
g4 <- ggplot(cmp, aes(reorder(sentimiento, pct), pct, fill = grupo)) + geom_col(position = "dodge") + coord_flip() +
  scale_fill_manual(values = c("#08519c", "#9ecae1"), name = NULL) +
  labs(title = "Nucleo activo vs resto de usuarios", x = NULL, y = "% de sus mensajes") + theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_nucleo_vs_resto.png"), g4, width = 9, height = 6.5, dpi = 150); print(g4)

# Quien expresa mas (usuarios anonimos con 50+ mensajes analizados) 
ul <- base %>% filter(!is.na(usuario)) %>% group_by(usuario) %>%
  summarise(mensajes = n(), con_sent = sum(n_senal > 0), across(all_of(cols), ~ sum(.x > 0)), .groups = "drop") %>%
  mutate(pct = con_sent / mensajes * 100, diversidad = rowSums(across(all_of(cols)) > 0)) %>%
  filter(mensajes >= 50) %>% arrange(desc(pct)) %>% slice_head(n = 10) %>%
  mutate(alias = sprintf("U%02d", row_number()))
cat("\n--- Top 10 usuarios (anonimos) que MAS expresan sentimientos ---\n")
print(ul %>% select(alias, mensajes, con_sent, pct, diversidad) %>% mutate(pct = round(pct, 1)))
g5 <- ggplot(ul, aes(pct, reorder(alias, pct), size = diversidad)) + geom_point(color = "#d6604d") +
  labs(title = "Usuarios que mas expresan sentimientos (anonimizados)",
       subtitle = "Tamano = cantidad de sentimientos distintos de los 15", x = "% de sus mensajes con algun sentimiento", y = NULL) +
  theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_top_usuarios.png"), g5, width = 9, height = 5, dpi = 150); print(g5)

registrar("12. Indice y nucleo", "Indice emocional mensual/periodo; nucleo 100+ vs resto; top usuarios anonimos",
          nrow(base), nrow(base), "g_indice_emocional, g_nucleo_vs_resto, g_top_usuarios")
cat("OK.\n")