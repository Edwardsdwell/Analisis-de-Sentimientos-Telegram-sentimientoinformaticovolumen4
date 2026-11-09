
source("R01_config.R")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringi); library(readr); library(tibble)
  library(ggplot2)
})
for (p in c("readxl", "plotly", "htmlwidgets", "networkD3", "ggalluvial"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
set.seed(42)
for (d in c("outputs/tablas", "outputs/graficos", "validacion", "data/processed"))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# 1. Lexico v1 y cambios -> lexico v2
f_lex <- list.files(pattern = "^lexicon_sentimientos\\.csv$", recursive = TRUE)
f_lex <- f_lex[!grepl("^docs/", f_lex)][1]
lex1 <- read_csv(f_lex, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
  mutate(peso = as.numeric(peso)) %>%
  distinct(sentimiento, tipo, patron, .keep_all = TRUE)
SENT <- unique(lex1$sentimiento)
nm <- SENT; names(nm) <- stri_trans_general(SENT, "Latin-ASCII")   # clave ASCII -> nombre real

RESP <- "\U0001F62E\u200d\U0001F4A8"   # emoji 'suspiro' (secuencia con ZWJ)

cam <- tribble(
  ~accion,  ~key,           ~tipo,     ~patron,                                                       ~peso, ~motivo,
  "quitar", "confusion",    "palabra", "\\bayuda\\w*",                                                NA,  "'si alguien necesita ayuda' ofrece ayuda; no es confusión",
  "agregar","confusion",    "palabra", "\\bnecesito ayuda\\b",                                         1,  "pedir ayuda en primera persona",
  "agregar","confusion",    "palabra", "\\bme pueden ayudar\\b",                                       1,  "pedir ayuda en primera persona",
  "quitar", "confusion",    "palabra", "\\bduda\\w*",                                                 NA,  "'cualquier duda escribe' es cortesía",
  "agregar","confusion",    "palabra", "\\b(tengo|tenia|surgio|surge) (una |alguna )?duda\\w*",        1,  "duda propia",
  "quitar", "curiosidad",   "palabra", "\\bpor que\\b",                                               NA,  "'por que' sin tilde suele ser 'porque'; las preguntas se detectan con la regla de pregunta",
  "quitar", "esperanza",    "palabra", "\\bsuerte\\b",                                                NA,  "'mala suerte' no es esperanza",
  "agregar","esperanza",    "palabra", "(?<!mala )(?<!pura )\\bsuerte\\b",                            0.5, "suerte débil; excluye 'mala/pura suerte'",
  "agregar","frustracion",  "palabra", "\\bmala suerte\\b",                                            1,  "mala suerte = frustración",
  "quitar", "sorpresa",     "palabra", "\\ben serio\\b",                                              NA,  "'en serio' es muletilla; solo cuenta con ? o !",
  "quitar", "sorpresa",     "palabra", "\\benserio\\b",                                               NA,  "'enserio' es muletilla",
  "agregar","sorpresa",     "palabra", "\\ben ?serio ?[?!]",                                           1,  "'en serio' con ? o !",
  "quitar", "enojo",        "palabra", "\\bmolest\\w*",                                               NA,  "'disculpen la molestia' es cortesía",
  "agregar","enojo",        "palabra", "\\b(me )?(molesta|molesto|molestan)\\b",                       1,  "molestia propia",
  "quitar", "alegria",      "emoji",   "\U0001F605",                                                  NA,  "😅 suele ser nervios, no alegría",
  "agregar","preocupacion", "emoji",   "\U0001F605",                                                   1,  "😅 = nervios",
  "quitar", "seguridad",    "emoji",   "\U0001F44D",                                                  NA,  "👍 es aprobación débil",
  "agregar","seguridad",    "emoji",   "\U0001F44D",                                                  0.5, "👍 aprobación débil (solo cuenta con otra señal)",
  "quitar", "seguridad",    "emoji",   "\U0001F44C",                                                  NA,  "👌 débil",
  "agregar","seguridad",    "emoji",   "\U0001F44C",                                                  0.5, "👌 aprobación débil",
  "quitar", "alegria",      "palabra", "\\bxd\\b",                                                    NA,  "'xd' suaviza el tono; no es alegría por sí solo",
  "agregar","alegria",      "palabra", "\\bxd\\b",                                                     0.5, "xd débil",
  "quitar", "preocupacion", "palabra", "\\bultimo dia\\b",                                            NA,  "señal débil",
  "agregar","preocupacion", "palabra", "\\bultimo dia\\b",                                             0.5, "señal débil",
  "agregar","frustracion",  "palabra", "\\bsin cupos?\\b",                                             1,  "bigrama 'sin cupo' frecuente en el corpus",
  "agregar","frustracion",  "palabra", "\\bno hay cupos?\\b",                                          1,  "falta de cupos",
  "agregar","frustracion",  "palabra", "\\bya no se puede\\b",                                         1,  "bloqueo de trámite",
  "agregar","frustracion",  "palabra", "\\b(no|nunca) se (abre|abrio|habilita|habilitan)\\b",          1,  "materia que no se abre",
  "agregar","frustracion",  "palabra", "\\bperjudic\\w*",                                              1,  "perjuicio"
)
cam$sentimiento <- unname(nm[cam$key])
stopifnot(!anyNA(cam$sentimiento))

lex2 <- lex1
for (i in seq_len(nrow(cam))) {
  if (cam$accion[i] == "quitar") {
    k <- lex2$sentimiento == cam$sentimiento[i] & lex2$tipo == cam$tipo[i] & lex2$patron == cam$patron[i]
    if (!any(k)) warning("No se encontró para quitar: ", cam$patron[i])
    lex2 <- lex2[!k, ]
  } else {
    lex2 <- bind_rows(lex2, tibble(sentimiento = cam$sentimiento[i], tipo = cam$tipo[i],
                                   patron = cam$patron[i], peso = cam$peso[i]))
  }
}
write_csv(lex2, "lexicon_sentimientos_v2.csv")
write_csv(cam %>% select(accion, sentimiento, tipo, patron, peso, motivo), "outputs/tablas/lexicon_v2_cambios.csv")
cat("Léxico v1:", nrow(lex1), "filas | v2:", nrow(lex2), "filas | cambios:", nrow(cam), "\n")

# 2. Motor de deteccion con contexto 
NEG    <- c("no", "nunca", "ni", "sin", "jamas", "tampoco")
POS_S  <- unname(nm[c("alegria", "gratitud", "esperanza", "seguridad", "admiracion", "tranquilidad")])
S_SEG  <- unname(nm["seguridad"]); S_CUR <- unname(nm["curiosidad"])
RE_AD  <- "(t\\.me/|chat\\.whatsapp|wa\\.me|forms\\.gle|docs\\.google\\.com/forms|\\bunete\\b|\\bconvocatoria\\b|enlace para unirte|invitacion al grupo)"
RE_QI  <- "^(cuando|donde|como|cual|cuales|quien|quienes|que|por ?que|a que|hasta (cuando|que)|cuanto|cuantos)\\b"
RE_QX  <- "\\b(alguien sabe|saben si|alguien tiene|alguien de|alguien con)\\b"
CORT   <- c("\\b(disculp\\w*|perdon\\w*|perdone\\w*) (por )?(la|las) molestias?\\b",
            "\\bcualquier duda\\b", "\\bsi (tienen|tienes|hay) (alguna )?duda\\w*",
            "\\bvale la pena\\b", "\\bpor si acaso\\b")
fold <- function(x) stri_trans_general(stri_trans_nfkc(x), "Any-Lower; NFD; [:Nonspacing Mark:] Remove; NFC")

# spam/preg/ctx = FALSE permite reconstruir cada etapa (ablación)
detectar <- function(raw, lex, spam = TRUE, preg = TRUE, ctx = TRUE) {
  raw[is.na(raw)] <- ""
  tk <- stri_trans_nfkc(raw)
  tn <- fold(raw)
  es_pub <- if (spam) stri_detect_regex(tn, RE_AD) else rep(FALSE, length(tn))
  if (ctx) {
    for (p in CORT) tn <- stri_replace_all_regex(tn, p, " ")
    tk <- stri_replace_all_fixed(tk, RESP, " ")
  }
  es_preg <- stri_detect_fixed(tk, "?") | stri_detect_fixed(tk, "\u00bf") |
    stri_detect_regex(stri_trim_both(tn), RE_QI) | stri_detect_regex(tn, RE_QX)
  sc <- matrix(0, length(tn), length(SENT), dimnames = list(NULL, SENT))
  for (i in seq_len(nrow(lex))) {
    s <- lex$sentimiento[i]; pat <- lex$patron[i]
    if (lex$tipo[i] == "palabra") {
      ini <- stri_locate_first_regex(tn, pat)[, 1]
      hit <- !is.na(ini)
      if (ctx && s %in% POS_S && any(hit)) {          
        idx <- which(hit)
        pre <- stri_sub(tn[idx], 1, ini[idx] - 1)
        ult <- stri_extract_all_regex(pre, "\\w+")
        neg <- vapply(ult, function(z) any(tail(z[!is.na(z)], 3) %in% NEG), logical(1))
        hit[idx[neg]] <- FALSE
      }
      if (ctx && s == S_SEG) hit <- hit & !es_preg    # 'es verdad?' es pregunta, no certeza
    } else {
      hit <- stri_detect_fixed(tk, pat)
    }
    sc[hit, s] <- sc[hit, s] + lex$peso[i]
  }
  pres <- sc >= 1
  if (preg) pres[, S_CUR] <- pres[, S_CUR] | es_preg
  pres[es_pub, ] <- FALSE
  list(pres = pres, score = sc, es_pub = es_pub, es_preg = es_preg)
}

# 3. Validacion contra tu etiquetado manual 
f_et <- "validacion/muestra_100_etiquetada.xlsx"
if (!file.exists(f_et)) stop("Copia tu Excel etiquetado a: ", f_et)
et <- suppressMessages(readxl::read_excel(f_et, sheet = "Etiquetar"))
et$texto[is.na(et$texto)] <- ""
G <- sapply(SENT, function(s) {
  s <- stri_trans_nfc(s)
  stri_trans_nfc(et$etiqueta_1) %in% s | stri_trans_nfc(et$etiqueta_2) %in% s | stri_trans_nfc(et$etiqueta_3) %in% s
})
colnames(G) <- SENT
G[is.na(G)] <- FALSE

metr <- function(pres, gold) {
  TP <- colSums(pres & gold); FP <- colSums(pres & !gold); FN <- colSums(!pres & gold)
  tibble(sentimiento = colnames(pres), TP = TP, FP = FP, FN = FN,
         precision = ifelse(TP + FP > 0, TP / (TP + FP), NA_real_),
         recall    = ifelse(TP + FN > 0, TP / (TP + FN), NA_real_),
         F1        = ifelse(2 * TP + FP + FN > 0, 2 * TP / (2 * TP + FP + FN), NA_real_))
}
glob <- function(pres, gold) {
  TP <- sum(pres & gold); FP <- sum(pres & !gold); FN <- sum(!pres & gold)
  p <- TP / (TP + FP); r <- TP / (TP + FN)
  tibble(precision = p, recall = r, F1 = 2 * p * r / (p + r),
         acierto_binario = mean((rowSums(pres) > 0) == (rowSums(gold) > 0)),
         coincidencia_exacta = mean(rowSums(pres != gold) == 0))
}
etapas <- list(
  list("v1 original",              lex1, FALSE, FALSE, FALSE),
  list("+ filtro de publicidad",   lex1, TRUE,  FALSE, FALSE),
  list("+ preguntas = curiosidad", lex1, TRUE,  TRUE,  FALSE),
  list("+ negación y cortesía",    lex1, TRUE,  TRUE,  TRUE),
  list("+ léxico ajustado (v2)",   lex2, TRUE,  TRUE,  TRUE))
abl <- bind_rows(lapply(etapas, function(e) {
  r <- detectar(et$texto, e[[2]], e[[3]], e[[4]], e[[5]])
  bind_cols(tibble(etapa = e[[1]]), glob(r$pres, G))
}))
abl$etapa <- factor(abl$etapa, levels = abl$etapa)
v1m <- metr(detectar(et$texto, lex1, FALSE, FALSE, FALSE)$pres, G) %>% mutate(version = "v1")
v2m <- metr(detectar(et$texto, lex2)$pres, G)                       %>% mutate(version = "v2")
por_sent <- bind_rows(v1m, v2m)
write_csv(abl, "outputs/tablas/validacion_ablacion.csv")
write_csv(por_sent, "outputs/tablas/validacion_v1_v2_por_sentimiento.csv")
cat("\n=== VALIDACION (100 mensajes etiquetados a mano) ===\n"); print(as.data.frame(abl), digits = 3)

#  4. Aplicar v2 a todo el corpus 
f_v1 <- "data/processed/listo_sent_v1.rds"; f_act <- "data/processed/listo_sent.rds"
if (!file.exists(f_v1)) file.copy(f_act, f_v1)
x  <- readRDS(f_v1)                       # siempre se parte del v1 (idempotente)
d1 <- x$d
cols_s <- grep("^s_", names(d1), value = TRUE)
nm_cols <- stri_trans_nfc(sub("^s_", "", cols_s))
stopifnot(all(stri_trans_nfc(SENT) %in% nm_cols))
col_de <- function(s) cols_s[match(stri_trans_nfc(s), nm_cols)]
idx <- which(d1$usable %in% TRUE)
r2  <- detectar(d1$texto_final[idx], lex2)

# control: ¿reproducimos el v1 guardado? (diagnóstico del texto usado)
r1  <- detectar(d1$texto_final[idx], lex1, FALSE, FALSE, FALSE)$pres
v1g <- sapply(SENT, function(s) d1[[col_de(s)]][idx] > 0)
cat("Coincidencia con el v1 guardado:", round(100 * mean(rowSums(r1 != v1g) == 0), 1), "% de mensajes\n")

cnt <- ifelse(r2$pres, pmax(1, floor(r2$score)), 0)
storage.mode(cnt) <- "integer"
d2 <- d1
for (s in SENT) d2[[col_de(s)]][idx] <- cnt[, s]
d2$es_publicidad <- FALSE; d2$es_publicidad[idx] <- r2$es_pub
d2$es_pregunta   <- FALSE; d2$es_pregunta[idx]   <- r2$es_preg
d2$usable[idx[r2$es_pub]] <- FALSE        # la publicidad sale del análisis
d2$n_senal <- rowSums(as.matrix(d2[, cols_s]))
d2$sent_dom <- NA_character_
dom <- SENT[max.col(cnt + r2$score / 1000, ties.method = "first")]
d2$sent_dom[idx] <- ifelse(rowSums(cnt) > 0, dom, NA_character_)
x2 <- x; x2$d <- d2
saveRDS(x2, f_act)
cat("Publicidad excluida:", sum(r2$es_pub), "mensajes | preguntas:", sum(r2$es_preg), "\n")

# 5. Comparacion v1 vs v2 en todo el corpus 
n1 <- length(idx); n2 <- sum(!r2$es_pub)
cmp <- tibble(sentimiento = SENT,
              v1 = sapply(SENT, function(s) 100 * mean(v1g[, s])),
              v2 = sapply(SENT, function(s) 100 * sum(cnt[, s] > 0) / n2)) %>%
  arrange(desc(v2))
write_csv(cmp, "outputs/tablas/comparacion_v1_v2.csv")
cat("\n=== % de mensajes por sentimiento: v1 vs v2 ===\n"); print(as.data.frame(cmp), digits = 3)

# Sankey v1 -> v2 (sentimiento dominante)
lab1 <- ifelse(is.na(d1$sent_dom[idx]), "sin señal", d1$sent_dom[idx])
lab2 <- ifelse(r2$es_pub, "publicidad (excluida)", ifelse(is.na(d2$sent_dom[idx]), "sin señal", d2$sent_dom[idx]))
lk <- tibble(from = paste0("v1: ", lab1), to = paste0("v2: ", lab2)) %>% count(from, to, name = "n")
write_csv(lk, "outputs/tablas/sankey_v1_v2.csv")

# 6. Graficos 
tema <- theme_minimal(base_size = 12)
g1 <- abl %>% pivot_longer(c(precision, recall, F1), names_to = "metrica", values_to = "valor") %>%
  ggplot(aes(etapa, valor, colour = metrica, group = metrica)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Qué aporta cada mejora al diccionario", x = NULL, y = NULL, colour = NULL) +
  tema + theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave("outputs/graficos/g_validacion_ablacion.png", g1, width = 9, height = 5, dpi = 150, bg = "white")

g2 <- por_sent %>% filter(!is.na(F1)) %>%
  ggplot(aes(reorder(sentimiento, F1), F1, fill = version)) +
  geom_col(position = "dodge") + coord_flip() +
  scale_fill_manual(values = c(v1 = "#9aa5b1", v2 = "#1f77b4")) +
  labs(title = "F1 por sentimiento: v1 vs v2 (100 mensajes etiquetados)", x = NULL, y = "F1", fill = NULL) + tema
ggsave("outputs/graficos/g_validacion_f1.png", g2, width = 8, height = 6, dpi = 150, bg = "white")

g3 <- cmp %>% pivot_longer(c(v1, v2), names_to = "version", values_to = "pct") %>%
  ggplot(aes(reorder(sentimiento, pct), pct, fill = version)) +
  geom_col(position = "dodge") + coord_flip() +
  scale_fill_manual(values = c(v1 = "#9aa5b1", v2 = "#1f77b4")) +
  labs(title = "Sentimientos en todo el corpus: v1 vs v2", x = NULL, y = "% de mensajes", fill = NULL) + tema
ggsave("outputs/graficos/g_comparacion_v1_v2.png", g3, width = 8, height = 6, dpi = 150, bg = "white")

# Interactivos (si algo falla, el script continúa)
guardar_w <- function(w, f) tryCatch(htmlwidgets::saveWidget(w, f, selfcontained = FALSE),
                                     error = function(e) message("No se pudo guardar ", f, ": ", conditionMessage(e)))
guardar_w(plotly::ggplotly(g1), "outputs/graficos/plotly_validacion_ablacion.html")
guardar_w(plotly::ggplotly(g2), "outputs/graficos/plotly_validacion_f1.html")
guardar_w(plotly::ggplotly(g3), "outputs/graficos/plotly_comparacion_v1_v2.html")

tryCatch({
  nodos <- data.frame(name = unique(c(lk$from, lk$to)), stringsAsFactors = FALSE)
  lkd <- as.data.frame(lk)
  lkd$source <- match(lkd$from, nodos$name) - 1; lkd$target <- match(lkd$to, nodos$name) - 1
  w <- networkD3::sankeyNetwork(Links = lkd, Nodes = nodos, Source = "source", Target = "target",
                                Value = "n", NodeID = "name", fontSize = 12, nodeWidth = 24)
  guardar_w(w, "outputs/graficos/sankey_v1_v2.html")
  g4 <- ggplot(lk, aes(axis1 = from, axis2 = to, y = n)) +
    ggalluvial::geom_alluvium(aes(fill = to), alpha = 0.7, show.legend = FALSE) +
    ggalluvial::geom_stratum(width = 0.28) +
    geom_text(stat = ggalluvial::StatStratum, aes(label = after_stat(stratum)), size = 2.6) +
    scale_x_discrete(limits = c("v1", "v2"), expand = c(0.2, 0.05)) +
    labs(title = "Sankey entre métodos: diccionario v1 → v2", y = "mensajes", x = NULL) + tema
  ggsave("outputs/graficos/g_sankey_v1_v2.png", g4, width = 11, height = 8, dpi = 150, bg = "white")
}, error = function(e) message("Sankey v1->v2: ", conditionMessage(e)))

tryCatch(registrar("R17_lexico_v2.R", "Léxico v2 con contexto + validación antes/después + Sankey v1→v2"),
         error = function(e) message("(bitácora: registrar() no aceptó estos argumentos: ", conditionMessage(e), ")"))
cat("\nListo. Ahora re-ejecuta R07, R08, R09, R12, R13, R14, R15 y R16 para refrescar todo con el v2.\n")