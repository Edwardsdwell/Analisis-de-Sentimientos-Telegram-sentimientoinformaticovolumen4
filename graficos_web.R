# =====================================================================
# graficos_web.R  -  Graficos interactivos (plotly) para el sitio
# Se carga con source("graficos_web.R") desde cada pagina .qmd
# Lee outputs/tablas/*.csv y data/processed/listo_sent.rds
# =====================================================================
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(plotly); library(readr); library(lubridate)})

# ---------- tema oscuro comun ----------------------------------------
TXT  <- "#e6edf3"; GRID <- "rgba(255,255,255,0.08)"
SENT_COL <- c(curiosidad = "#4cc9f0", gratitud = "#ffd166", alegría = "#06d6a0", confusión = "#c77dff",
              tristeza = "#5e7ce2", preocupación = "#f4a261", seguridad = "#2ec4b6", frustración = "#ff6b6b",
              esperanza = "#a8e063", sorpresa = "#ff9ff3", tranquilidad = "#74c0fc", decepción = "#adb5bd",
              enojo = "#e63946", admiración = "#f78fb3", cansancio = "#868e96")
VAL_COL  <- c(negativo = "#ff5d73", positivo = "#2ee6a6", `neutro (curiosidad/sorpresa)` = "#4cc9f0", `sin señal` = "#6b7280")
col_s <- function(x) { r <- unname(SENT_COL[x]); r[is.na(r)] <- "#9aa5b1"; r }

W <- function(p, ..., h = NULL) {
  p |> layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
              font = list(color = TXT, family = "Inter, Segoe UI, sans-serif", size = 13),
              hoverlabel = list(bgcolor = "#111a2b", bordercolor = "#4cc9f0", font = list(color = "#ffffff")),
              legend = list(font = list(color = TXT), bgcolor = "rgba(0,0,0,0)"),
              margin = list(l = 10, r = 10, t = 40, b = 10), ...) |>
    config(displaylogo = FALSE, responsive = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines")) -> w
  if (!is.null(h)) w$height <- h
  w
}
ax <- function(title = "", ...) list(title = title, gridcolor = GRID, zerolinecolor = GRID, color = TXT, ...)
ttl <- function(t) list(text = t, x = 0.02, font = list(size = 16, color = TXT))
tb  <- function(n) read_csv(file.path("outputs/tablas", paste0(n, ".csv")), show_col_types = FALSE)
pct <- function(x, d = 1) paste0(formatC(x, format = "f", digits = d, decimal.mark = ","), " %")

# barras horizontales genericas ordenadas
barh <- function(df, lab, val, color = "#4cc9f0", titulo = "", xt = "", text = NULL, h = NULL, hover = NULL) {
  df <- df[order(df[[val]]), ]
  df[[lab]] <- factor(df[[lab]], levels = df[[lab]])
  col <- if (length(color) == nrow(df)) color[order(match(seq_along(color), seq_along(color)))] else color
  plot_ly(df, y = df[[lab]], x = df[[val]], type = "bar", orientation = "h",
          marker = list(color = color, line = list(width = 0)),
          text = text, textposition = if (is.null(text)) "none" else "outside", cliponaxis = FALSE,
          hovertemplate = if (is.null(hover)) "%{y}: %{x:,.1f}<extra></extra>" else hover) |>
    W(title = ttl(titulo), xaxis = ax(xt), yaxis = list(title = "", color = TXT, automargin = TRUE), h = h)
}

# ---------- CALIDAD ---------------------------------------------------
gw_embudo <- function() {
  e <- tb("calidad_embudo"); e$paso <- factor(e$paso, levels = e$paso)
  plot_ly(e, x = ~paso, y = ~n, type = "bar", text = ~format(n, big.mark = " "), textposition = "outside",
          marker = list(color = c("#4cc9f0", "#4895ef", "#4361ee", "#7209b7", "#f72585")),
          customdata = ~pct_del_total, hovertemplate = "%{x}<br>%{y:,} filas (%{customdata} % del total)<extra></extra>") |>
    W(title = ttl("De 28 000 elementos a mensajes analizables"), xaxis = ax(""), yaxis = ax("Filas"), h = 420)
}
gw_dominios <- function(top = 15) {
  d <- tb("calidad_dominios") |> slice_head(n = top)
  barh(d, "dominio", "n", "#f58518", "Dominios más compartidos (solo el dominio, nunca el texto)", "Enlaces",
       text = format(d$n[order(d$n)], big.mark = " "), h = 460, hover = "%{y}: %{x:,} enlaces<extra></extra>")
}
gw_spam <- function() {
  s <- tb("calidad_spam_sensibilidad")
  plot_ly(s, x = ~umbral) |>
    add_trace(y = ~precision, name = "Precisión", type = "scatter", mode = "lines+markers", line = list(color = "#ff6b6b", width = 3)) |>
    add_trace(y = ~recall, name = "Recall", type = "scatter", mode = "lines+markers", line = list(color = "#4cc9f0", width = 3)) |>
    W(title = ttl("Umbral del puntaje de spam frente al filtro de publicidad"),
      xaxis = ax("Umbral", dtick = 1), yaxis = ax("", range = c(0, 1.05), tickformat = ".0%"),
      legend = list(orientation = "h", y = -0.2), h = 400)
}

# ---------- VALIDACION ------------------------------------------------
gw_ablacion <- function() {
  a <- tb("validacion_ablacion"); a$etapa <- factor(a$etapa, levels = a$etapa)
  plot_ly(a, x = ~etapa, type = "scatter", mode = "lines+markers") |>
    add_trace(y = ~precision, name = "Precisión", line = list(color = "#06d6a0", width = 3), marker = list(size = 9)) |>
    add_trace(y = ~recall, name = "Recall", line = list(color = "#4cc9f0", width = 3), marker = list(size = 9)) |>
    add_trace(y = ~F1, name = "F1", line = list(color = "#ffd166", width = 3), marker = list(size = 9)) |>
    W(title = ttl("Qué aporta cada mejora al diccionario"), xaxis = ax(""), yaxis = ax("", range = c(0, 1), tickformat = ".0%"),
      legend = list(orientation = "h", y = -0.3), h = 440)
}
gw_f1 <- function() {
  p <- tb("validacion_v1_v2_por_sentimiento") |> mutate(F1 = replace_na(F1, 0))
  ord <- p |> filter(version == "v2") |> arrange(F1) |> pull(sentimiento)
  p$sentimiento <- factor(p$sentimiento, levels = ord)
  plot_ly(p, y = ~sentimiento, x = ~F1, color = ~version, colors = c(v1 = "#8b97a7", v2 = "#4cc9f0"), type = "bar", orientation = "h",
          hovertemplate = "%{y} (%{fullData.name}): F1 %{x:.2f}<extra></extra>") |>
    W(title = ttl("F1 por sentimiento: versión 1 frente a versión 2"), barmode = "group", xaxis = ax("F1", range = c(0, 1)),
      yaxis = list(title = "", color = TXT), legend = list(orientation = "h", y = -0.12), h = 560)
}
gw_comparacion <- function() {
  c <- tb("comparacion_v1_v2") |> pivot_longer(c(v1, v2), names_to = "version", values_to = "pct")
  c$sentimiento <- factor(c$sentimiento, levels = rev(unique(tb("comparacion_v1_v2")$sentimiento[order(-tb("comparacion_v1_v2")$v2)])))
  plot_ly(c, y = ~sentimiento, x = ~pct, color = ~version, colors = c(v1 = "#8b97a7", v2 = "#4cc9f0"), type = "bar", orientation = "h",
          hovertemplate = "%{y} (%{fullData.name}): %{x:.2f} % de los mensajes<extra></extra>") |>
    W(title = ttl("Sentimientos en todo el corpus: versión 1 frente a versión 2"), barmode = "group",
      xaxis = ax("% de mensajes"), yaxis = list(title = "", color = TXT), legend = list(orientation = "h", y = -0.12), h = 600)
}
sankey_plotly <- function(from, to, n, titulo, h = 560, cols = NULL) {
  nodos <- unique(c(from, to))
  nc <- if (is.null(cols)) rep(c("#4cc9f0", "#06d6a0", "#ffd166", "#ff9ff3", "#f4a261", "#c77dff", "#ff6b6b", "#74c0fc"), length.out = length(nodos)) else cols
  si <- match(from, nodos) - 1; ti <- match(to, nodos) - 1
  plot_ly(type = "sankey", orientation = "h",
          node = list(label = nodos, color = nc, pad = 14, thickness = 18, line = list(color = "rgba(255,255,255,.3)", width = 0.5),
                      hovertemplate = "%{label}: %{value:,} mensajes<extra></extra>"),
          link = list(source = si, target = ti, value = n, color = paste0(substr(nc[si + 1], 1, 7), "55"),
                      hovertemplate = "%{source.label} → %{target.label}: %{value:,}<extra></extra>")) |>
    W(title = ttl(titulo), h = h)
}
gw_sankey_v1v2 <- function() {
  l <- tb("sankey_v1_v2")
  nodos <- unique(c(l$from, l$to))
  nm <- sub("^v[12]: ", "", nodos); nm <- tolower(nm)
  cols <- ifelse(nm == "sin señal", "#6b7280", ifelse(grepl("publicidad", nm), "#ff5d73", col_s(nm)))
  sankey_plotly(l$from, l$to, l$n, "Cómo cambió cada mensaje de la versión 1 a la versión 2", 620, cols)
}

# ---------- SENTIMIENTOS ----------------------------------------------
gw_nrc <- function() {
  n <- tb("comparacion_nrc_propio") |> mutate(pct = pct * 100)
  n <- n[order(n$pct), ]; n$sentimiento <- factor(n$sentimiento, levels = n$sentimiento)
  plot_ly(n, y = ~sentimiento, x = ~pct, color = ~fuente, colors = c("#8b97a7", "#f72585")[1:2], type = "bar", orientation = "h",
          hovertemplate = "%{y}: %{x:.1f} % de los mensajes<extra>%{fullData.name}</extra>") |>
    W(title = ttl("Librería NRC (8) frente a diccionario propio (15)"), xaxis = ax("% de mensajes analizados"),
      yaxis = list(title = "", color = TXT), legend = list(orientation = "h", y = -0.1), h = 680)
}
radar_p <- function(tab, grupo, ordenes = NULL, excluir = "curiosidad", titulo = "", cols = NULL, h = 560) {
  tt <- tab |> filter(!sentimiento %in% excluir)
  ejes <- tt |> group_by(sentimiento) |> summarise(t = sum(tasa)) |> arrange(desc(t)) |> pull(sentimiento)
  gs <- if (is.null(ordenes)) unique(as.character(tt[[grupo]])) else ordenes
  p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
  for (i in seq_along(gs)) {
    z <- tt |> filter(as.character(.data[[grupo]]) == gs[i]) |> arrange(match(sentimiento, ejes))
    cc <- if (is.null(cols)) c("#4cc9f0", "#ffd166", "#ff6b6b", "#06d6a0", "#c77dff", "#f4a261", "#ff9ff3", "#74c0fc", "#a8e063")[i] else cols[[gs[i]]]
    p <- add_trace(p, r = c(z$tasa, z$tasa[1]), theta = c(z$sentimiento, z$sentimiento[1]), name = gs[i], opacity = 0.85,
                   line = list(color = cc, width = 2.5), marker = list(color = cc), fillcolor = paste0(cc, "26"),
                   hovertemplate = "%{theta}: %{r:.2f} %<extra>%{fullData.name}</extra>")
  }
  W(p, title = ttl(titulo), h = h,
    polar = list(bgcolor = "rgba(255,255,255,0.03)", radialaxis = list(ticksuffix = "%", gridcolor = GRID, color = TXT, angle = 90),
                 angularaxis = list(categoryarray = ejes, categoryorder = "array", direction = "clockwise", color = TXT, gridcolor = GRID)),
    legend = list(orientation = "h", y = -0.1))
}
gw_radar_temp <- function() radar_p(tb("radar_temporadas"), "periodo_tipo", c("Invierno", "Verano", "Fuera de temporada"),
                                     titulo = "Perfil emocional por temporada (sin curiosidad)",
                                     cols = list(Invierno = "#4cc9f0", Verano = "#ffd166", `Fuera de temporada` = "#ff6b6b"))
gw_radar_per <- function() radar_p(tb("radar_periodos"), "periodo", titulo = "Perfil emocional por periodo (clic en la leyenda para aislar uno)", h = 640)
gw_inv_ver <- function() {
  t <- tb("radar_temporadas")
  ord <- t |> filter(periodo_tipo == "Invierno") |> arrange(tasa) |> pull(sentimiento)
  t$sentimiento <- factor(t$sentimiento, levels = ord)
  plot_ly(t, y = ~sentimiento, x = ~tasa, color = ~periodo_tipo, type = "bar", orientation = "h",
          colors = c(Invierno = "#4cc9f0", Verano = "#ffd166", `Fuera de temporada` = "#ff6b6b"),
          hovertemplate = "%{y} (%{fullData.name}): %{x:.2f} %<extra></extra>") |>
    W(title = ttl("Sentimientos en Invierno, Verano y fuera de temporada"), barmode = "group", xaxis = ax("% de mensajes"),
      yaxis = list(title = "", color = TXT), legend = list(orientation = "h", y = -0.1), h = 640)
}
gw_heat_periodos <- function() {
  s <- tb("sentimientos_por_periodo")
  ord_p <- c("Verano 2022-23", "Invierno 2023", "Verano 2023-24", "Invierno 2024", "Verano 2024-25", "Invierno 2025", "Verano 2025-26", "Invierno 2026", "Fuera de temporada")
  ord_p <- intersect(ord_p, unique(s$periodo))
  ord_s <- s |> group_by(sentimiento) |> summarise(t = sum(tasa100)) |> arrange(t) |> pull(sentimiento)
  m <- s |> mutate(periodo = factor(periodo, ord_p), sentimiento = factor(sentimiento, ord_s))
  z <- xtabs(tasa100 ~ sentimiento + periodo, data = m)
  tope <- 8   # el color llega al maximo en 8 %; curiosidad (mucho mayor) queda en el color mas claro
  plot_ly(x = colnames(z), y = rownames(z), z = unclass(z), type = "heatmap", zmin = 0, zmax = tope,
          colorscale = list(c(0, "#2a1458"), c(0.25, "#7b2cbf"), c(0.5, "#f72585"), c(0.75, "#ff9e00"), c(1, "#ffee32")),
          text = round(unclass(z), 1), texttemplate = "%{text}", textfont = list(color = "#ffffff", size = 11),
          colorbar = list(title = "% (tope 8)", tickfont = list(color = TXT), titlefont = list(color = TXT)),
          hovertemplate = "%{y} · %{x}: %{z:.2f} % de los mensajes<extra></extra>", xgap = 3, ygap = 3) |>
    W(title = ttl("Sentimientos por periodo (% de mensajes)"), xaxis = list(title = "", color = TXT, tickangle = -30), yaxis = list(title = "", color = TXT), h = 600)
}
gw_sankey_per <- function() {
  s <- tb("sentimientos_por_periodo") |> filter(mens_con > 0)
  per <- unique(s$periodo); se <- unique(s$sentimiento)
  nodos <- c(per, se)
  cols <- c(rep("#4361ee", length(per)), col_s(se))
  sankey_plotly(s$periodo, s$sentimiento, s$mens_con, "De cada periodo a los sentimientos", 640, cols)
}
gw_polar_hora <- function() {
  hh <- tb("polar_hora_valencia")
  p <- plot_ly(type = "barpolar")
  for (g in names(VAL_COL)) {
    z <- hh |> filter(grupo == g) |> arrange(hora)
    if (nrow(z)) p <- add_trace(p, r = z$n, theta = z$hora * 15 + 7.5, name = g, marker = list(color = VAL_COL[[g]]),
                                text = paste0(z$hora, " h"), hovertemplate = paste0("%{text} · ", g, ": %{r} mensajes<extra></extra>"))
  }
  W(p, title = ttl("Reloj de mensajes: actividad y tono por hora"), h = 600,
    polar = list(bgcolor = "rgba(255,255,255,0.03)", radialaxis = list(gridcolor = GRID, color = TXT),
                 angularaxis = list(direction = "clockwise", rotation = 90, tickmode = "array", tickvals = seq(0, 315, 45),
                                    ticktext = paste0(seq(0, 21, 3), " h"), color = TXT, gridcolor = GRID)),
    barmode = "stack", legend = list(orientation = "h", y = -0.08))
}
load_d <- function() {
  B <- readRDS("data/processed/listo_sent.rds"); d <- B$d
  d$.fecha <- as.POSIXct(d$fecha)
  list(d = d, sent = B$sent)
}
valencia <- function(d, sent) {
  k <- stringi::stri_trans_general(sent, "Latin-ASCII")
  POS <- sent[k %in% c("alegria", "gratitud", "esperanza", "seguridad", "admiracion", "tranquilidad")]
  NEU <- sent[k %in% c("sorpresa", "curiosidad")]; NEG <- setdiff(sent, c(POS, NEU))
  mk <- function(v) rowSums(as.matrix(d[, paste0("s_", v), drop = FALSE]) > 0) > 0
  case_when(mk(NEG) ~ "negativo", mk(POS) ~ "positivo", mk(NEU) ~ "neutro (curiosidad/sorpresa)", TRUE ~ "sin señal")
}
gw_polar_semana <- function(L) {
  d <- L$d |> filter(usable); d$grupo <- valencia(d, L$sent); d$dia <- wday(d$.fecha, week_start = 1)
  s <- d |> count(dia, grupo) |> complete(dia = 1:7, grupo, fill = list(n = 0))
  p <- plot_ly(type = "barpolar")
  for (g in names(VAL_COL)) {
    z <- s |> filter(grupo == g) |> arrange(dia)
    p <- add_trace(p, r = z$n, theta = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom"), name = g, marker = list(color = VAL_COL[[g]]),
                   hovertemplate = paste0("%{theta} · ", g, ": %{r} mensajes<extra></extra>"))
  }
  W(p, title = ttl("Mensajes por día de la semana y tono"), h = 560, barmode = "stack",
    polar = list(bgcolor = "rgba(255,255,255,0.03)", radialaxis = list(gridcolor = GRID, color = TXT),
                 angularaxis = list(direction = "clockwise", rotation = 90, color = TXT, gridcolor = GRID)),
    legend = list(orientation = "h", y = -0.08))
}
gw_reacciones <- function() {
  r <- tb("reacciones_por_sentimiento")
  barh(r, "sentimiento", "reacciones", col_s(r$sentimiento[order(r$reacciones)]), "Sentimiento de las reacciones (emojis puestos a los mensajes)", "Reacciones",
       text = format(r$reacciones[order(r$reacciones)], big.mark = " "), h = 440, hover = "%{y}: %{x:,} reacciones<extra></extra>")
}
gw_sexo <- function() {
  s <- tb("sentimientos_por_sexo") |> arrange(pct_M + pct_H)
  s$lab <- ifelse(s$signif, paste0(s$sentimiento, " *"), s$sentimiento); s$lab <- factor(s$lab, levels = s$lab)
  plot_ly(s, y = ~lab, type = "bar", orientation = "h") |>
    add_trace(x = ~pct_H * 100, name = "Hombres", marker = list(color = "#4cc9f0"), hovertemplate = "%{y}: %{x:.2f} % de los mensajes de hombres<extra></extra>") |>
    add_trace(x = ~pct_M * 100, name = "Mujeres", marker = list(color = "#ff6fae"), hovertemplate = "%{y}: %{x:.2f} % de los mensajes de mujeres<extra></extra>") |>
    W(title = ttl("Sentimientos según el sexo inferido por nombre (* = diferencia significativa, Holm p < 0,05)"), barmode = "group",
      xaxis = ax("% de los mensajes de cada grupo"), yaxis = list(title = "", color = TXT), legend = list(orientation = "h", y = -0.1), h = 620)
}

# ---------- TEXTO ------------------------------------------------------
# PRIVACIDAD: nombres de personas que NO deben verse en el sitio publico.
# Agrega aqui (en minusculas y sin tildes) cualquier nombre o apellido que aparezca en las graficas de texto.
NOMBRES_OCULTOS <- c("maria", "jose", "carlos", "mullisaca", "menfy", "elvis", "ruben", "alcon", "beltran", "cosme", "corini", "chapi", "patzi")
oculta <- function(df, col) {
  if (!length(NOMBRES_OCULTOS)) return(df)
  pat <- paste0("(^| )(", paste(NOMBRES_OCULTOS, collapse = "|"), ")( |$)")
  df[!grepl(pat, df[[col]]), ]
}
gw_top_palabras <- function(n = 25) {
  f <- oculta(tb("frecuencias_palabras"), "palabra") |> slice_head(n = n)
  barh(f, "palabra", "n", "#4895ef", paste0("Las ", n, " palabras más frecuentes"), "Frecuencia", h = 620, hover = "%{y}: %{x:,} veces<extra></extra>")
}
gw_bigramas <- function(n = 20) {
  f <- oculta(tb("bigramas"), "bigrama") |> slice_head(n = n)
  barh(f, "bigrama", "n", "#06d6a0", paste0("Los ", n, " bigramas más frecuentes"), "Frecuencia", h = 560, hover = "%{y}: %{x:,} veces<extra></extra>")
}
gw_red_bigramas <- function(n = 70) {
  bg <- oculta(tb("bigramas"), "bigrama") |> slice_head(n = n)
  pr <- strsplit(bg$bigrama, " ", fixed = TRUE)
  e <- tibble(a = sapply(pr, `[`, 1), b = sapply(pr, `[`, 2), w = bg$n)
  e <- e[!is.na(e$b), ]
  nodos <- unique(c(e$a, e$b)); N <- length(nodos)
  set.seed(42)
  pos <- matrix(rnorm(N * 2), N, 2); ia <- match(e$a, nodos); ib <- match(e$b, nodos)
  k <- 1; temp <- 0.6
  for (it in 1:300) {
    disp <- matrix(0, N, 2)
    for (i in 1:N) {
      dlt <- sweep(pos, 2, pos[i, ], "-") * -1; dist <- pmax(sqrt(rowSums(dlt^2)), 1e-3)
      disp[i, ] <- colSums(dlt / dist * (k^2 / dist) * (seq_len(N) != i))
    }
    for (j in seq_len(nrow(e))) {
      dlt <- pos[ia[j], ] - pos[ib[j], ]; dist <- max(sqrt(sum(dlt^2)), 1e-3); f <- dist^2 / k * (1 + log(e$w[j]) / 5)
      disp[ia[j], ] <- disp[ia[j], ] - dlt / dist * f; disp[ib[j], ] <- disp[ib[j], ] + dlt / dist * f
    }
    disp <- disp - pos * 0.35
    len <- pmax(sqrt(rowSums(disp^2)), 1e-3); pos <- pos + disp / len * pmin(len, temp); temp <- temp * 0.985
  }
  grado <- table(c(e$a, e$b))[nodos]
  p <- plot_ly()
  for (j in seq_len(nrow(e)))
    p <- add_trace(p, x = pos[c(ia[j], ib[j]), 1], y = pos[c(ia[j], ib[j]), 2], type = "scatter", mode = "lines", hoverinfo = "none",
                   line = list(color = "rgba(76,201,240,0.35)", width = 1 + 4 * e$w[j] / max(e$w)), showlegend = FALSE)
  p <- add_trace(p, x = pos[, 1], y = pos[, 2], type = "scatter", mode = "markers+text", text = nodos, textposition = "top center",
                 textfont = list(color = TXT, size = 12), showlegend = FALSE,
                 marker = list(size = 7 + 3 * as.numeric(grado), color = "#ffd166", line = list(color = "#ffffff", width = 0.5)),
                 hovertemplate = "%{text}<extra></extra>")
  W(p, title = ttl("Red de palabras: los 70 bigramas más frecuentes (arrastra para mover)"), h = 640,
    xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), dragmode = "pan")
}
facet_bars <- function(df, grupo, palabra, valor, ncol, titulo, color, h) {
  gs <- unique(df[[grupo]]); nr <- ceiling(length(gs) / ncol)
  ps <- lapply(gs, function(g) {
    z <- df[df[[grupo]] == g, ]; z <- z[order(z[[valor]]), ]; z[[palabra]] <- factor(z[[palabra]], levels = z[[palabra]])
    plot_ly(z, y = z[[palabra]], x = z[[valor]], type = "bar", orientation = "h", name = g, marker = list(color = color),
            hovertemplate = paste0("%{y}: %{x:.4f}<extra>", g, "</extra>")) |>
      layout(annotations = list(list(text = paste0("<b>", g, "</b>"), x = 0.5, y = 1.0, yanchor = "bottom", xref = "paper", yref = "paper", showarrow = FALSE, font = list(color = TXT, size = 12))),
             yaxis = list(color = TXT, automargin = TRUE), xaxis = list(color = TXT, gridcolor = GRID))
  })
  subplot(ps, nrows = nr, margin = c(0.05, 0.05, 0.03, 0.09), shareX = FALSE) |> W(title = ttl(titulo), showlegend = FALSE, h = h)
}
gw_tfidf_periodos <- function(n = 7) {
  t <- oculta(tb("tfidf_periodos"), "palabra") |> filter(is.na(suppressWarnings(as.numeric(palabra)))) |> group_by(periodo) |> slice_max(tf_idf, n = n, with_ties = FALSE) |> ungroup()
  ord <- c("Verano 2022-23", "Invierno 2023", "Verano 2023-24", "Invierno 2024", "Verano 2024-25", "Invierno 2025", "Verano 2025-26", "Invierno 2026", "Fuera de temporada")
  t$periodo <- factor(t$periodo, intersect(ord, unique(t$periodo))); t <- t[order(t$periodo), ]; t$periodo <- as.character(t$periodo)
  facet_bars(t, "periodo", "palabra", "tf_idf", 3, "Palabras distintivas (TF-IDF) por periodo", "#9d4edd", 760)
}
gw_tfidf_temp <- function(n = 10) {
  t <- oculta(tb("tfidf_periodos"), "palabra") |> mutate(tipo = ifelse(grepl("^Verano", periodo), "Verano", ifelse(grepl("^Invierno", periodo), "Invierno", "Fuera de temporada"))) |>
    filter(is.na(suppressWarnings(as.numeric(palabra)))) |> group_by(tipo, palabra) |> summarise(n = sum(n), .groups = "drop") |>
    group_by(tipo) |> mutate(tf = n / sum(n)) |> ungroup() |> group_by(palabra) |> mutate(idf = log(3 / n_distinct(tipo))) |> ungroup() |>
    mutate(tf_idf = tf * idf) |> group_by(tipo) |> slice_max(tf_idf, n = n, with_ties = FALSE) |> ungroup()
  facet_bars(t, "tipo", "palabra", "tf_idf", 3, "Palabras distintivas (TF-IDF) por temporada", "#4cc9f0", 480)
}
gw_zipf <- function() {
  f <- tb("frecuencias_palabras") |> mutate(rango = row_number())  # Zipf: solo rangos, sin nombres visibles
  f$palabra <- ifelse(f$palabra %in% NOMBRES_OCULTOS, "(nombre oculto)", f$palabra)
  plot_ly(f, x = ~rango, y = ~n, type = "scatter", mode = "markers", text = ~palabra,
          marker = list(color = "#4cc9f0", size = 5, opacity = 0.6),
          hovertemplate = "«%{text}»<br>rango %{x}, %{y} veces<extra></extra>") |>
    W(title = ttl("Ley de Zipf: pocas palabras muy usadas, muchas casi nunca"), xaxis = ax("Rango (log)", type = "log"),
      yaxis = ax("Frecuencia (log)", type = "log"), h = 480)
}

# ---------- SUGERENCIAS (extras) --------------------------------------
gw_picos_dia <- function(L, sent_nombre, color = "#ff6b6b", top = 4) {
  d <- L$d |> filter(usable); col <- paste0("s_", sent_nombre)
  g <- d |> mutate(dia = as.Date(.fecha)) |> group_by(dia) |> summarise(n = sum(.data[[col]] > 0), tot = n(), .groups = "drop") |> filter(n > 0)
  tp <- g |> slice_max(n, n = top, with_ties = FALSE)
  plot_ly(g, x = ~dia, y = ~n, type = "bar", marker = list(color = color), customdata = ~tot,
          hovertemplate = "%{x|%d/%m/%Y}: %{y} mensajes de %{customdata}<extra></extra>") |>
    add_annotations(x = tp$dia, y = tp$n, text = format(tp$dia, "%d/%m/%y"), showarrow = TRUE, arrowcolor = TXT, font = list(color = TXT, size = 11), ay = -28) |>
    W(title = ttl(paste0("Mensajes con ", sent_nombre, " por día")), xaxis = ax(""), yaxis = ax("Mensajes"), h = 400)
}
gw_heat_hora_dia <- function(L) {
  d <- L$d |> filter(usable); d$h <- hour(d$.fecha); d$dia <- wday(d$.fecha, week_start = 1)
  m <- d |> count(dia, h) |> complete(dia = 1:7, h = 0:23, fill = list(n = 0))
  z <- xtabs(n ~ dia + h, m)
  plot_ly(x = paste0(colnames(z), " h"), y = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom"), z = unclass(z), type = "heatmap",
          colorscale = list(c(0, "#0b1426"), c(0.35, "#3a0ca3"), c(0.7, "#f72585"), c(1, "#ffd166")), xgap = 2, ygap = 2,
          hovertemplate = "%{y} %{x}: %{z} mensajes<extra></extra>") |>
    W(title = ttl("Cuándo se escribe: hora por día de la semana (hora de La Paz)"), xaxis = list(title = "", color = TXT), yaxis = list(title = "", color = TXT, autorange = "reversed"), h = 440)
}
gw_sent_hora <- function(L, sel = c("curiosidad", "gratitud", "frustración", "alegría", "preocupación")) {
  d <- L$d |> filter(usable); d$h <- hour(d$.fecha)
  p <- plot_ly(type = "scatter", mode = "lines+markers")
  for (s in sel) {
    z <- d |> group_by(h) |> summarise(t = mean(.data[[paste0("s_", s)]] > 0) * 100, .groups = "drop")
    p <- add_trace(p, x = z$h, y = z$t, name = s, line = list(color = col_s(s), width = 2.5), marker = list(color = col_s(s)),
                   hovertemplate = paste0("%{x} h: %{y:.1f} %<extra>", s, "</extra>"))
  }
  W(p, title = ttl("Sentimientos según la hora del día (% de los mensajes de esa hora)"), xaxis = ax("Hora", dtick = 3), yaxis = ax("% de mensajes"),
    legend = list(orientation = "h", y = -0.2), h = 440)
}
gw_respuesta <- function() {
  r <- tb("respuesta_por_sentimiento"); r$col <- ifelse(r$grupo == "TODOS", "#8b97a7", "#4cc9f0")
  r <- r[order(r$pct_respondido), ]; r$grupo <- factor(r$grupo, levels = r$grupo)
  p1 <- plot_ly(r, y = ~grupo, x = ~pct_respondido, type = "bar", orientation = "h", marker = list(color = ~col), name = "% que recibe respuesta",
                hovertemplate = "%{y}: %{x:.1f} % recibe respuesta<extra></extra>")
  p2 <- plot_ly(r, y = ~grupo, x = ~mediana_min, type = "bar", orientation = "h", marker = list(color = ~col), name = "Mediana (min)",
                hovertemplate = "%{y}: %{x:.1f} min<extra></extra>")
  subplot(p1, p2, shareY = TRUE, titleX = TRUE, margin = 0.04) |>
    W(title = ttl("Quién recibe respuesta y qué tan rápido (izq.: % con respuesta · der.: minutos de mediana)"), showlegend = FALSE, h = 620,
      xaxis = ax(""), xaxis2 = ax(""), yaxis = list(title = "", color = TXT))
}
gw_indice_mensual <- function() {
  i <- tb("indice_emocional_mensual")
  plot_ly(i, x = ~mes, y = ~indice, type = "scatter", mode = "lines+markers", line = list(color = "#4cc9f0", width = 3),
          marker = list(color = ifelse(i$indice >= 0, "#2ee6a6", "#ff5d73"), size = 9), customdata = ~n,
          hovertemplate = "%{x|%b %Y}: índice %{y:.2f} (%{customdata} mensajes)<extra></extra>") |>
    add_trace(x = range(i$mes), y = c(0, 0), type = "scatter", mode = "lines", inherit = FALSE, line = list(color = "rgba(255,255,255,.4)", dash = "dot"), hoverinfo = "none", showlegend = FALSE) |>
    W(title = ttl("Índice emocional mensual (+1 todo positivo, −1 todo negativo)"), xaxis = ax(""), yaxis = ax("Índice", range = c(-1, 1)), showlegend = FALSE, h = 420)
}
gw_pos_neg <- function() {
  i <- tb("indice_emocional_mensual")
  plot_ly(i, x = ~mes, type = "scatter", mode = "lines") |>
    add_trace(y = ~pos100, name = "Positivo", line = list(color = "#2ee6a6", width = 3), hovertemplate = "%{x|%b %Y}: %{y:.1f} por 100 mensajes<extra>Positivo</extra>") |>
    add_trace(y = ~neg100, name = "Negativo", line = list(color = "#ff5d73", width = 3), hovertemplate = "%{x|%b %Y}: %{y:.1f} por 100 mensajes<extra>Negativo</extra>") |>
    W(title = ttl("Emociones positivas y negativas por mes (menciones por 100 mensajes)"), xaxis = ax(""), yaxis = ax(""), legend = list(orientation = "h", y = -0.15), h = 400)
}
gw_indice_periodos <- function() {
  i <- tb("indice_emocional_mensual"); P <- read_csv("data/processed/periodos.csv", show_col_types = FALSE)
  i$periodo <- "Fuera de temporada"
  for (r in seq_len(nrow(P))) i$periodo[i$mes >= as.Date(P$inicio[r]) & i$mes <= as.Date(P$fin[r])] <- P$etiqueta[r]
  g <- i |> group_by(periodo) |> summarise(indice = (sum(pos) - sum(neg)) / (sum(pos) + sum(neg)), n = sum(n), .groups = "drop")
  ord <- c("Verano 2022-23", "Invierno 2023", "Verano 2023-24", "Invierno 2024", "Verano 2024-25", "Invierno 2025", "Verano 2025-26", "Invierno 2026", "Fuera de temporada")
  g$periodo <- factor(g$periodo, intersect(ord, g$periodo)); g <- g[order(g$periodo), ]
  plot_ly(g, x = ~periodo, y = ~indice, type = "bar", marker = list(color = ifelse(g$indice >= 0, "#2ee6a6", "#ff5d73")),
          hovertemplate = "%{x}: índice %{y:.2f}<extra></extra>") |>
    W(title = ttl("Índice emocional por periodo"), xaxis = list(title = "", color = TXT, tickangle = -25), yaxis = ax("Índice"), h = 420)
}
gw_nucleo <- function() {
  n <- tb("nucleo_vs_resto"); ord <- n |> filter(grepl("Resto", grupo)) |> arrange(pct) |> pull(sentimiento); n$sentimiento <- factor(n$sentimiento, ord)
  plot_ly(n, y = ~sentimiento, x = ~pct, color = ~grupo, type = "bar", orientation = "h",
          colors = c("#4361ee", "#ffd166"), hovertemplate = "%{y} (%{fullData.name}): %{x:.2f} %<extra></extra>") |>
    W(title = ttl("Núcleo activo (100+ mensajes) frente al resto de usuarios"), barmode = "group", xaxis = ax("% de sus mensajes"),
      yaxis = list(title = "", color = TXT), legend = list(orientation = "h", y = -0.1), h = 620)
}
gw_top_usuarios <- function(L, n = 10) {
  d <- L$d |> filter(usable, !is.na(usuario))
  cols <- paste0("s_", L$sent)
  u <- d |> group_by(usuario) |> filter(n() >= 100) |>
    summarise(msgs = n(), pct = mean(n_senal > 0) * 100, across(all_of(cols), ~ sum(.x > 0), .names = "c_{.col}"), .groups = "drop")
  u$div <- rowSums(u[, grepl("^c_", names(u))] > 0)
  u <- u |> slice_max(pct, n = n, with_ties = FALSE) |> arrange(pct)
  u$alias <- sprintf("U%02d", rev(seq_len(nrow(u)))); u$alias <- factor(u$alias, levels = u$alias)
  plot_ly(u, x = ~pct, y = ~alias, type = "scatter", mode = "markers", customdata = cbind(u$msgs, u$div),
          marker = list(size = ~6 + 3 * div, color = "#ff6b6b", line = list(color = "#fff", width = 0.5)),
          hovertemplate = "%{y}: %{x:.1f} % de sus %{customdata[0]} mensajes con sentimiento<br>%{customdata[1]} sentimientos distintos<extra></extra>") |>
    W(title = ttl("Usuarios (anónimos) que más expresan sentimientos · tamaño = variedad de sentimientos"), xaxis = ax("% de sus mensajes con algún sentimiento"),
      yaxis = list(title = "", color = TXT), h = 440)
}
gw_series <- function(L) {
  d <- L$d |> filter(usable); d$mes <- floor_date(as.Date(d$.fecha), "month")
  base <- d |> count(mes, name = "nb") |> filter(nb >= 50)
  p <- plot_ly(type = "scatter", mode = "lines+markers")
  for (s in L$sent) {
    z <- d |> group_by(mes) |> summarise(m = sum(.data[[paste0("s_", s)]] > 0), .groups = "drop") |> inner_join(base, by = "mes") |> mutate(t = m / nb * 100)
    p <- add_trace(p, x = z$mes, y = z$t, name = s, line = list(color = col_s(s), width = 2), marker = list(color = col_s(s), size = 5),
                   visible = if (s %in% c("curiosidad", "gratitud", "frustración", "alegría")) TRUE else "legendonly",
                   hovertemplate = paste0("%{x|%b %Y}: %{y:.1f} %<extra>", s, "</extra>"))
  }
  W(p, title = ttl("Evolución mensual de cada sentimiento (clic en la leyenda para mostrar u ocultar)"), xaxis = ax(""), yaxis = ax("% de mensajes del mes"), h = 520,
    legend = list(orientation = "h", y = -0.12))
}
gw_animacion <- function() {
  s <- tb("sentimientos_por_periodo")
  ord <- intersect(c("Verano 2022-23", "Invierno 2023", "Verano 2023-24", "Invierno 2024", "Verano 2024-25", "Invierno 2025", "Verano 2025-26", "Invierno 2026", "Fuera de temporada"), unique(s$periodo))
  s <- s |> filter(sentimiento != "curiosidad") |> mutate(periodo = factor(periodo, ord))
  oo <- s |> group_by(sentimiento) |> summarise(t = sum(tasa100)) |> arrange(t) |> pull(sentimiento); s$sentimiento <- factor(s$sentimiento, oo)
  plot_ly(s, y = ~sentimiento, x = ~tasa100, frame = ~periodo, type = "bar", orientation = "h", marker = list(color = col_s(as.character(s$sentimiento))),
          hovertemplate = "%{y}: %{x:.2f} % de los mensajes<extra></extra>") |>
    W(title = ttl("Animación: cómo cambia cada emoción por periodo (sin curiosidad)"), xaxis = ax("% de mensajes", range = c(0, 7.5)),
      yaxis = list(title = "", color = TXT), h = 600) |>
    animation_opts(frame = 900, transition = 500, redraw = TRUE) |>
    animation_slider(currentvalue = list(prefix = "Periodo: ", font = list(color = TXT))) |>
    animation_button(label = "▶ Reproducir")
}

# ---------- extras de apoyo --------------------------------------------
safe <- function(expr) tryCatch({ w <- expr; if (inherits(w, "plotly")) w <- plotly::plotly_build(w); w }, error = function(e)
  htmltools::div(class = "aviso", paste0("Esta gráfica necesita datos que todavía no están: ", conditionMessage(e))))
gw_general <- function(L) {
  d <- L$d |> filter(usable)
  g <- tibble(sentimiento = L$sent, pct = sapply(L$sent, function(s) mean(d[[paste0("s_", s)]] > 0) * 100),
              n = sapply(L$sent, function(s) sum(d[[paste0("s_", s)]] > 0)))
  g <- g[order(g$pct), ]
  barh(g, "sentimiento", "pct", col_s(g$sentimiento), "Qué sentimientos aparecen (% de los mensajes analizados)", "% de mensajes",
       text = pct(g$pct), h = 600, hover = "%{y}: %{x:.2f} % de los mensajes<extra></extra>")
}
