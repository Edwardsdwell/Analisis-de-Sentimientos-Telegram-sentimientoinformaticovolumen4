source("R01_config.R")
suppressPackageStartupMessages({library(tidyverse); library(plotly); library(htmlwidgets); library(stringi)})
set.seed(42)

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; reac <- B$reac; sent <- B$sent
tiene <- function(v) v %in% names(d)
guardar_w <- function(w, nombre) tryCatch(
  saveWidget(w, file.path(getwd(), DIR_GRAF, nombre), selfcontained = FALSE, libdir = "lib_widgets"),
  error = function(e) message("No se pudo guardar ", nombre, ": ", conditionMessage(e)))
cs <- paste0("s_", sent)

# 1. EMBUDO de datos 
pasos <- tibble(
  paso = c("Elementos del JSON", "Mensajes (no servicio)", "Mensajes con texto",
           "Usables (sin publicidad ni ruido)", "Usables con al menos 1 sentimiento"),
  n = c(nrow(d),
        sum(d$es_msg, na.rm = TRUE),
        sum(d$es_msg & !d$sin_texto, na.rm = TRUE),
        sum(d$usable, na.rm = TRUE),
        sum(d$usable & d$n_senal > 0, na.rm = TRUE))) %>%
  mutate(pct_del_total = round(100 * n / first(n), 1), perdidos = lag(n, default = first(n)) - n)
write_csv(pasos, file.path(DIR_TAB, "calidad_embudo.csv"))
cat("\n--- EMBUDO ---\n"); print(pasos)
gE <- ggplot(pasos, aes(reorder(paso, -n), n)) +
  geom_col(fill = "#4c78a8") + geom_text(aes(label = format(n, big.mark = " ", scientific = FALSE)), vjust = -0.4, size = 3.5) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 16)) +
  labs(title = "Embudo de datos: de 28 000 elementos a mensajes analizables", x = NULL, y = "Filas") +
  theme_minimal(base_size = 11)
ggsave(file.path(DIR_GRAF, "g_calidad_embudo.png"), gE, width = 9, height = 5, dpi = 150, bg = "white")
guardar_w(ggplotly(gE), "plotly_calidad_embudo.html")

#  2. DOMINIOS de los enlaces (solo el dominio, no el texto) 
urls <- stri_extract_all_regex(d$texto_final, "https?://[^\\s<>\"')]+", omit_no_match = FALSE)
dom  <- unlist(lapply(urls, function(u) if (all(is.na(u))) character() else
  tolower(stri_replace_first_regex(stri_replace_first_regex(u, "^https?://(www\\.)?", ""), "[/?#:].*$", ""))))
tab_dom <- tibble(dominio = dom) %>% count(dominio, sort = TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
write_csv(tab_dom, file.path(DIR_TAB, "calidad_dominios.csv"))
cat("\n--- DOMINIOS MAS COMPARTIDOS (top 10) ---\n"); print(head(tab_dom, 10))
if (nrow(tab_dom) > 0) {
  gD <- tab_dom %>% slice_head(n = 15) %>%
    ggplot(aes(reorder(dominio, n), n)) + geom_col(fill = "#f58518") + coord_flip() +
    labs(title = "Dominios más compartidos en el grupo", x = NULL, y = "Enlaces") + theme_minimal(base_size = 11)
  ggsave(file.path(DIR_GRAF, "g_calidad_dominios.png"), gD, width = 8, height = 5.5, dpi = 150, bg = "white")
  guardar_w(ggplotly(gD), "plotly_calidad_dominios.html")
}

#3. SCORE DE SPAM y sensibilidad del umbral 
RE_INV <- "t\\.me/|chat\\.whatsapp|wa\\.me|forms\\.gle|docs\\.google\\.com/forms|[uú]nete|convocatoria|invitaci[oó]n al grupo"
tx  <- ifelse(is.na(d$texto_final), "", d$texto_final)
let <- stri_replace_all_regex(tx, "[^\\p{L}]", "")
may <- ifelse(nchar(let) > 15, stri_count_regex(let, "\\p{Lu}") / nchar(let), 0)
score <- 2 * (stri_detect_regex(tx, "https?://") | (if (tiene("n_links")) d$n_links > 0 else FALSE)) +
  3 * stri_detect_regex(stri_trans_tolower(tx), RE_INV) +
  1 * (if (tiene("n_emojis")) d$n_emojis >= 4 else FALSE) +
  1 * (may > 0.5) +
  1 * (if (tiene("es_repetido")) d$es_repetido %in% TRUE else FALSE) +
  1 * (nchar(tx) > 400)
ref <- d$es_publicidad %in% TRUE
sens <- map_dfr(1:8, function(u) {
  f <- score >= u; tp <- sum(f & ref); fp <- sum(f & !ref); fn <- sum(!f & ref)
  tibble(umbral = u, marcados = sum(f), coinciden_con_filtro = tp,
         precision = round(tp / max(1, tp + fp), 3), recall = round(tp / max(1, tp + fn), 3),
         pct_del_total = round(100 * sum(f) / length(f), 2))
})
write_csv(sens, file.path(DIR_TAB, "calidad_spam_sensibilidad.csv"))
cat("\n--- SENSIBILIDAD DEL UMBRAL DE SPAM (filtro final = es_publicidad:", sum(ref), "mensajes) ---\n"); print(sens)
gS <- sens %>% pivot_longer(c(precision, recall), names_to = "metrica", values_to = "valor") %>%
  ggplot(aes(umbral, valor, colour = metrica)) + geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = 1:8) + scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Sensibilidad del umbral de spam frente al filtro de publicidad", x = "Umbral del puntaje de spam", y = NULL, colour = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave(file.path(DIR_GRAF, "g_calidad_spam_umbral.png"), gS, width = 8, height = 5, dpi = 150, bg = "white")
guardar_w(ggplotly(gS), "plotly_calidad_spam_umbral.html")

# 4. CHEQUEOS DE INTEGRIDAD (V1..V10) 
v <- function(id, que, ok, detalle = "") tibble(chequeo = id, descripcion = que, resultado = ifelse(isTRUE(ok), "OK", "REVISAR"), detalle = detalle)
nreac_d <- if (tiene("n_reac")) sum(d$n_reac, na.rm = TRUE) else NA
nreac_r <- if ("n" %in% names(reac)) sum(reac$n, na.rm = TRUE) else nrow(reac)
chk <- bind_rows(
  v("V1",  "Ids unicos",                              !anyDuplicated(d$id), paste(nrow(d), "filas")),
  v("V2",  "Se conservan los 28 000 elementos",       nrow(d) == 28000, paste("filas:", nrow(d))),
  v("V3",  "Fechas completas y en orden razonable",   !anyNA(d$fecha) && min(d$fecha) > as.POSIXct("2022-01-01", tz = "UTC"),
    paste(format(min(d$fecha), "%Y-%m-%d"), "a", format(max(d$fecha), "%Y-%m-%d"))),
  v("V4",  "Reacciones: suma por mensaje = tabla de reacciones", isTRUE(nreac_d == nreac_r), paste(nreac_d, "vs", nreac_r)),
  v("V5",  "Ningun mensaje usable es publicidad",     sum(d$usable & d$es_publicidad, na.rm = TRUE) == 0,
    paste(sum(d$es_publicidad, na.rm = TRUE), "publicidades excluidas")),
  v("V6",  "Todo usable tiene texto",                 sum(d$usable & d$sin_texto, na.rm = TRUE) == 0),
  v("V7",  "Conteos s_* son enteros >= 0 y sin NA",   all(sapply(d[cs], function(x) !anyNA(x) && all(x >= 0 & x == round(x))))),
  v("V8",  "sent_dom vacio  <=>  sin ninguna senal",  all(is.na(d$sent_dom) == (rowSums(d[cs] > 0) == 0))),
  v("V9",  "Periodos asignados a todo mensaje",       !anyNA(d$periodo) && !anyNA(d$periodo_tipo)),
  v("V10", "Base de tasas = suma de usables por periodo", sum(table(d$periodo[d$usable])) == sum(d$usable),
    paste(sum(d$usable), "usables"))
)
write_csv(chk, file.path(DIR_TAB, "calidad_chequeos.csv"))
cat("\n--- CHEQUEOS DE INTEGRIDAD ---\n"); print(chk, n = 20, width = 120)
cat("\nResultado:", sum(chk$resultado == "OK"), "de", nrow(chk), "chequeos OK\n")

registrar("9. Calidad de datos", "Embudo, dominios, spam con sensibilidad de umbral y chequeos V1-V10",
          nrow(d), sum(d$usable, na.rm = TRUE), "g_calidad_*.png, plotly_calidad_*.html, calidad_*.csv")
cat("OK. Graficos en outputs/graficos y tablas en outputs/tablas\n")