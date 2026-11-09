library(tidyverse); library(lubridate); library(scales); library(stringi)
source("R01_config.R")

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
tokens <- readRDS(file.path(DIR_PROC, "tokens.rds"))
base <- d %>% filter(usable) %>% mutate(dia = as_date(fecha), hora = hour(fecha), dsem = wday(fecha, week_start = 1))
DIAS <- c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom")

#  A. DIAS PICO y que los explica 
for (ds in c("preocupación", "frustración", "confusión")) {
  col <- paste0("s_", ds)
  dia <- base %>% group_by(dia) %>% summarise(n_dia = n(), m = sum(.data[[col]] > 0), .groups = "drop") %>%
    mutate(pct = round(m / n_dia * 100, 1))
  pk <- dia %>% filter(m >= 8) %>% arrange(desc(m)) %>% slice_head(n = 8)
  pk$palabras_clave <- map_chr(pk$dia, function(f) {
    ids <- base$id[base$dia == f & base[[col]] > 0]
    tokens %>% filter(id %in% ids) %>% count(palabra, sort = TRUE) %>% slice_head(n = 6) %>%
      pull(palabra) %>% paste(collapse = ", ")
  })
  cat("\n=== DIAS PICO de", ds, "===\n"); print(pk, width = 200)
  write_csv(pk, file.path(DIR_TAB, paste0("picos_dia_", stri_trans_general(ds, "Latin-ASCII"), ".csv")))
  g <- ggplot(dia, aes(dia, m)) + geom_col(fill = "#d6604d", width = 1) +
    geom_text(data = pk %>% slice_head(n = 5), aes(label = format(dia, "%d-%m-%y")), vjust = -0.4, size = 3) +
    labs(title = paste("Mensajes con", ds, "por dia"), x = NULL, y = "Mensajes") + theme_minimal(base_size = 12)
  ggsave(file.path(DIR_GRAF, paste0("g_picos_dia_", stri_trans_general(ds, "Latin-ASCII"), ".png")), g, width = 11, height = 4.5, dpi = 150)
  print(g)
}

# B. HORA x DIA y sentimientos por hora 
act <- base %>% count(dsem, hora)
g1 <- ggplot(act, aes(hora, factor(dsem, levels = 7:1, labels = rev(DIAS)), fill = n)) +
  geom_tile(color = "white") + scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Mensajes") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Cuando se escribe: hora x dia de la semana", x = "Hora (La Paz)", y = NULL) + theme_minimal(base_size = 12)
ggsave(file.path(DIR_GRAF, "g_heatmap_hora_dia.png"), g1, width = 10, height = 4.5, dpi = 150); print(g1)

sel <- c("preocupación", "cansancio", "curiosidad", "frustración", "gratitud", "alegría")
ph <- base %>% select(hora, all_of(paste0("s_", sel))) %>%
  pivot_longer(-hora, names_to = "sentimiento", values_to = "n") %>% mutate(sentimiento = sub("^s_", "", sentimiento)) %>%
  group_by(hora, sentimiento) %>% summarise(tasa = mean(n > 0) * 100, nn = n(), .groups = "drop") %>% filter(nn >= 30)
g2 <- ggplot(ph, aes(hora, tasa)) + geom_line(color = "#2171b5") + geom_point(size = 1) +
  facet_wrap(~ sentimiento, scales = "free_y") + scale_x_continuous(breaks = seq(0, 23, 6)) +
  labs(title = "Sentimientos segun la hora del dia", x = "Hora", y = "% de mensajes") + theme_minimal(base_size = 11)
ggsave(file.path(DIR_GRAF, "g_sentimiento_hora.png"), g2, width = 11, height = 6, dpi = 150); print(g2)

# C. Velocidad de respuesta segun el sentimiento 
orig <- d %>% select(id, unix_o = unix, usuario_o = usuario)
rep1 <- d %>% filter(es_msg, !is.na(reply_id)) %>% select(reply_id, unix_r = unix, usuario_r = usuario) %>%
  inner_join(orig, by = c("reply_id" = "id")) %>%
  filter(is.na(usuario_r) | is.na(usuario_o) | usuario_r != usuario_o) %>%      # sin auto-respuestas
  group_by(reply_id) %>% summarise(t_first = min(unix_r), .groups = "drop")
b2 <- base %>% left_join(rep1, by = c("id" = "reply_id")) %>%
  mutate(resp = !is.na(t_first), t_min = (t_first - unix) / 60)
resumen <- function(x, nombre) tibble(grupo = nombre, mensajes = nrow(x),
                                      pct_respondido = round(mean(x$resp) * 100, 1), mediana_min = round(median(x$t_min[x$resp], na.rm = TRUE), 1))
rs <- bind_rows(resumen(b2, "TODOS"), map_dfr(sent, ~ resumen(b2[b2[[paste0("s_", .x)]] > 0, ], .x)))
cat("\n=== Respuesta recibida segun el sentimiento del mensaje ===\n"); print(rs %>% arrange(desc(pct_respondido)), n = 20)
write_csv(rs, file.path(DIR_TAB, "respuesta_por_sentimiento.csv"))

g3 <- rs %>% pivot_longer(c(pct_respondido, mediana_min), names_to = "metrica", values_to = "valor") %>%
  mutate(metrica = recode(metrica, pct_respondido = "% que recibe respuesta", mediana_min = "Minutos hasta la 1a respuesta (mediana)"),
         grupo = fct_reorder(grupo, valor, .fun = max)) %>%
  ggplot(aes(grupo, valor, fill = grupo == "TODOS")) + geom_col(show.legend = FALSE) + coord_flip() +
  facet_wrap(~ metrica, scales = "free_x") + scale_fill_manual(values = c("#2171b5", "#969696")) +
  labs(title = "Quien recibe respuesta y que tan rapido, segun el sentimiento", x = NULL, y = NULL) +
  theme_minimal(base_size = 11)
ggsave(file.path(DIR_GRAF, "g_respuesta_sentimiento.png"), g3, width = 12, height = 6, dpi = 150); print(g3)

registrar("11. Picos y respuesta", "Dias pico con palabras clave, hora x dia, sentimiento por hora y velocidad de respuesta por sentimiento",
          nrow(base), nrow(base), "g_picos_dia_*, g_heatmap_hora_dia, g_respuesta_sentimiento")
cat("OK.\n")