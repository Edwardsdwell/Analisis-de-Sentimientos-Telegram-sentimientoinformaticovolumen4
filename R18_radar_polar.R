
source("R01_config.R")
suppressPackageStartupMessages({library(tidyverse); library(plotly); library(htmlwidgets); library(lubridate)})
set.seed(42)

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
cols <- paste0("s_", sent)
base <- d %>% filter(usable)
orden <- c(read_csv(file.path(DIR_PROC, "periodos.csv"), show_col_types = FALSE)$etiqueta, "Fuera de temporada")
orden <- intersect(orden, unique(base$periodo))

# valencia de cada sentimiento (por clave sin tildes)
k    <- stringi::stri_trans_general(sent, "Latin-ASCII")
POS  <- sent[k %in% c("alegria", "gratitud", "esperanza", "seguridad", "admiracion", "tranquilidad")]
NEU  <- sent[k %in% c("sorpresa", "curiosidad")]
NEGS <- setdiff(sent, c(POS, NEU))
CUR  <- sent[k == "curiosidad"]

long <- base %>% select(periodo, periodo_tipo, all_of(cols)) %>%
  pivot_longer(all_of(cols), names_to = "sentimiento", values_to = "n") %>%
  mutate(sentimiento = sub("^s_", "", sentimiento))
tab_per <- long %>% group_by(periodo, sentimiento) %>% summarise(tasa = mean(n > 0) * 100, .groups = "drop") %>%
  mutate(periodo = factor(periodo, levels = orden))
tab_tip <- long %>% group_by(periodo_tipo, sentimiento) %>% summarise(tasa = mean(n > 0) * 100, .groups = "drop")
write_csv(tab_per, file.path(DIR_TAB, "radar_periodos.csv"))
write_csv(tab_tip, file.path(DIR_TAB, "radar_temporadas.csv"))

guardar_w <- function(w, nombre) tryCatch(
  saveWidget(w, file.path(getwd(), DIR_GRAF, nombre), selfcontained = FALSE, libdir = "lib_widgets"),
  error = function(e) message("No se pudo guardar ", nombre, ": ", conditionMessage(e)))

#1. RADAR interactivo (plotly) 
radar_plotly <- function(tab, grupo, titulo, excluir = character()) {
  tt    <- tab %>% filter(!sentimiento %in% excluir)
  ejes  <- tt %>% group_by(sentimiento) %>% summarise(t = sum(tasa)) %>% arrange(desc(t)) %>% pull(sentimiento)
  p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
  for (g in unique(as.character(tt[[grupo]]))) {
    z <- tt %>% filter(as.character(.data[[grupo]]) == g) %>% arrange(match(sentimiento, ejes))
    p <- add_trace(p, r = c(z$tasa, z$tasa[1]), theta = c(z$sentimiento, z$sentimiento[1]),
                   name = g, opacity = 0.65, hovertemplate = "%{theta}: %{r:.1f}%<extra>%{fullData.name}</extra>")
  }
  layout(p, title = titulo,
         polar = list(radialaxis = list(ticksuffix = "%", angle = 90),
                      angularaxis = list(categoryarray = ejes, categoryorder = "array", direction = "clockwise")),
         legend = list(orientation = "h", y = -0.1))
}
guardar_w(radar_plotly(tab_tip, "periodo_tipo", "Perfil emocional: Invierno vs Verano vs Fuera de temporada"),
          "plotly_radar_temporadas.html")
guardar_w(radar_plotly(tab_per, "periodo", "Perfil emocional por periodo (sin curiosidad; clic en la leyenda para aislar un periodo)",
                       excluir = CUR),
          "plotly_radar_periodos.html")

# 2. RADAR estatico (para el informe)
radar_gg <- function(tab, grupo, titulo, excluir = character(), pal = NULL) {
  tt   <- tab %>% filter(!sentimiento %in% excluir)
  ejes <- tt %>% group_by(sentimiento) %>% summarise(t = sum(tasa)) %>% arrange(desc(t)) %>% pull(sentimiento)
  tt   <- tt %>% mutate(x = match(sentimiento, ejes), g = as.character(.data[[grupo]]))
  cierre <- tt %>% filter(x == 1) %>% mutate(x = length(ejes) + 1)
  tt <- bind_rows(tt, cierre) %>% arrange(g, x)
  ggplot(tt, aes(x, tasa, colour = g, fill = g, group = g)) +
    geom_line(linewidth = 0.9) + geom_point(size = 1.4) +
    coord_polar() +
    scale_x_continuous(breaks = seq_along(ejes), labels = ejes, limits = c(1, length(ejes) + 1)) +
    labs(title = titulo, x = NULL, y = "% de mensajes", colour = NULL, fill = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
}
g1 <- radar_gg(tab_tip, "periodo_tipo", "Radar de emociones: Invierno vs Verano vs Fuera de temporada")
ggsave(file.path(DIR_GRAF, "g_radar_temporadas.png"), g1, width = 8, height = 7.5, dpi = 150, bg = "white")
g2 <- radar_gg(tab_per, "periodo", "Radar de emociones por periodo (sin curiosidad)", excluir = CUR)
ggsave(file.path(DIR_GRAF, "g_radar_periodos.png"), g2, width = 9, height = 8, dpi = 150, bg = "white")

#3. POLAR por hora del dia
mk <- function(v) rowSums(as.matrix(base[, paste0("s_", v), drop = FALSE]) > 0) > 0
b <- base %>% mutate(hora = hour(fecha), pos = mk(POS), neu = mk(NEU), neg = mk(NEGS)) %>%
  mutate(grupo = case_when(neg ~ "negativo", pos ~ "positivo", neu ~ "neutro (curiosidad/sorpresa)", TRUE ~ "sin señal"))
hh <- b %>% count(hora, grupo) %>% complete(hora = 0:23, grupo, fill = list(n = 0))
write_csv(hh, file.path(DIR_TAB, "polar_hora_valencia.csv"))
cols_g <- c("negativo" = "#d62728", "positivo" = "#2ca02c", "neutro (curiosidad/sorpresa)" = "#1f77b4", "sin señal" = "#c7c7c7")
g3 <- ggplot(hh, aes(hora, n, fill = grupo)) +
  geom_col(width = 1, colour = "white", linewidth = 0.2) + coord_polar(start = 0) +
  scale_x_continuous(breaks = seq(0, 21, 3), labels = paste0(seq(0, 21, 3), "h"), limits = c(-0.5, 23.5)) +
  scale_fill_manual(values = cols_g) +
  labs(title = "Reloj de mensajes: actividad y tono por hora del día", x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(DIR_GRAF, "g_polar_hora.png"), g3, width = 8, height = 8, dpi = 150, bg = "white")

p3 <- plot_ly(type = "barpolar")
for (g in names(cols_g)) {
  z <- hh %>% filter(grupo == g) %>% arrange(hora)
  p3 <- add_trace(p3, r = z$n, theta = z$hora * 15 + 7.5, name = g, marker = list(color = cols_g[[g]]),
                  hovertemplate = paste0(g, ": %{r} mensajes<extra></extra>"))
}
p3 <- layout(p3, title = "Reloj de mensajes por hora del día (La Paz)",
             polar = list(angularaxis = list(direction = "clockwise", rotation = 90, tickmode = "array",
                                             tickvals = seq(0, 315, 45), ticktext = paste0(seq(0, 21, 3), "h"))),
             legend = list(orientation = "h", y = -0.1))
guardar_w(p3, "plotly_polar_hora.html")

# 4. POLAR por dia de la semana y tono
sem <- b %>% mutate(dia = wday(fecha, week_start = 1)) %>% count(dia, grupo) %>%
  complete(dia = 1:7, grupo, fill = list(n = 0))
g4 <- ggplot(sem, aes(factor(dia, labels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom")), n, fill = grupo)) +
  geom_col(width = 1, colour = "white") + coord_polar() + scale_fill_manual(values = cols_g) +
  labs(title = "Mensajes por día de la semana y tono", x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(DIR_GRAF, "g_polar_semana.png"), g4, width = 7.5, height = 7.5, dpi = 150, bg = "white")

# Resumen en consola
cat("\nTasas por temporada (% de mensajes), top 6 por temporada:\n")
print(tab_tip %>% group_by(periodo_tipo) %>% slice_max(tasa, n = 6) %>% arrange(periodo_tipo, desc(tasa)), n = 30)
cat("\nHora con más mensajes:", hh %>% group_by(hora) %>% summarise(n = sum(n)) %>% slice_max(n, n = 1) %>% pull(hora), "h\n")
registrar("8. Radar y polar", "Radar de emociones (temporadas y periodos) y relojes polares hora/semana",
          nrow(base), nrow(base), "g_radar_*.png, g_polar_*.png, plotly_radar_*.html, plotly_polar_hora.html")
cat("OK. Graficos en outputs/graficos\n")