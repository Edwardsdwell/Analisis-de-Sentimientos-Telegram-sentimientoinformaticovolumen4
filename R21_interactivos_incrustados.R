
escribir <- function(txt, f) writeLines(txt, f, useBytes = TRUE)

# interactivos.qmd 
escribir(r"---(---
title: "Visualizaciones interactivas"
---

```{r}
suppressMessages({library(dplyr); library(tidyr); library(plotly)})
oscuro <- function(p, ...) layout(p, paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117", font = list(color = "#e6edf3"), ...)
radar <- function(tab, grupo, excluir = character()) {
  tt   <- tab |> filter(!sentimiento %in% excluir)
  ejes <- tt |> group_by(sentimiento) |> summarise(t = sum(tasa)) |> arrange(desc(t)) |> pull(sentimiento)
  p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
  for (g in unique(as.character(tt[[grupo]]))) {
    z <- tt |> filter(as.character(.data[[grupo]]) == g) |> arrange(match(sentimiento, ejes))
    p <- add_trace(p, r = c(z$tasa, z$tasa[1]), theta = c(z$sentimiento, z$sentimiento[1]), name = g, opacity = 0.6,
                   hovertemplate = "%{theta}: %{r:.1f}%<extra>%{fullData.name}</extra>")
  }
  oscuro(p, height = 560,
         polar = list(bgcolor = "#161b22", radialaxis = list(ticksuffix = "%", angle = 90),
                      angularaxis = list(categoryarray = ejes, categoryorder = "array", direction = "clockwise")),
         legend = list(orientation = "h", y = -0.1))
}
```

## Radar de emociones por temporada

Pasa el mouse para ver el valor; clic en la leyenda para ocultar una temporada.

```{r}
radar(readr::read_csv("outputs/tablas/radar_temporadas.csv", show_col_types = FALSE), "periodo_tipo")
```

## Radar por periodo (sin curiosidad)

Haz clic en un periodo de la leyenda para aislarlo.

```{r}
tp <- readr::read_csv("outputs/tablas/radar_periodos.csv", show_col_types = FALSE)
radar(tp, "periodo", excluir = "curiosidad")
```

## Reloj de mensajes por hora del día

```{r}
hh <- readr::read_csv("outputs/tablas/polar_hora_valencia.csv", show_col_types = FALSE)
cg <- c("negativo" = "#d62728", "positivo" = "#2ca02c", "neutro (curiosidad/sorpresa)" = "#1f77b4", "sin señal" = "#c7c7c7")
p <- plot_ly(type = "barpolar")
for (g in names(cg)) {
  z <- hh |> filter(grupo == g) |> arrange(hora)
  if (nrow(z)) p <- add_trace(p, r = z$n, theta = z$hora * 15 + 7.5, name = g, marker = list(color = cg[[g]]),
                              hovertemplate = paste0(g, ": %{r} mensajes<extra></extra>"))
}
oscuro(p, height = 560,
       polar = list(bgcolor = "#161b22", angularaxis = list(direction = "clockwise", rotation = 90, tickmode = "array",
                    tickvals = seq(0, 315, 45), ticktext = paste0(seq(0, 21, 3), "h"))),
       legend = list(orientation = "h", y = -0.1))
```

## Qué aporta cada mejora del diccionario

```{r}
fa <- "outputs/tablas/validacion_ablacion.csv"
if (file.exists(fa)) {
  a <- readr::read_csv(fa, show_col_types = FALSE)
  a$etapa <- factor(a$etapa, levels = a$etapa)
  p <- plot_ly(a, x = ~etapa, type = "scatter", mode = "lines+markers")
  p <- add_trace(p, y = ~precision, name = "Precisión")
  p <- add_trace(p, y = ~recall, name = "Recall")
  p <- add_trace(p, y = ~F1, name = "F1")
  oscuro(p, yaxis = list(title = "", range = c(0, 1), tickformat = ".0%"), xaxis = list(title = ""), legend = list(orientation = "h", y = -0.3))
}
```

## Sankey entre métodos: v1 → v2

```{r}
suppressMessages(library(networkD3))
lk <- readr::read_csv("outputs/tablas/sankey_v1_v2.csv", show_col_types = FALSE)
nodos <- data.frame(name = unique(c(lk$from, lk$to)))
enl <- as.data.frame(lk)
enl$source <- match(enl$from, nodos$name) - 1; enl$target <- match(enl$to, nodos$name) - 1
sk <- sankeyNetwork(Links = enl, Nodes = nodos, Source = "source", Target = "target", Value = "n",
                    NodeID = "name", fontSize = 13, nodeWidth = 24, height = 650)
htmlwidgets::onRender(sk, "function(el){ d3.select(el).selectAll('.node text').style('fill','#ffffff'); }")
```

## Embudo de datos

```{r}
e <- readr::read_csv("outputs/tablas/calidad_embudo.csv", show_col_types = FALSE)
e$paso <- factor(e$paso, levels = e$paso)
oscuro(plot_ly(e, x = ~paso, y = ~n, type = "bar", text = ~format(n, big.mark = " "), textposition = "outside",
               marker = list(color = "#4cc9f0")), xaxis = list(title = ""), yaxis = list(title = "Filas"))
```

## Animación por periodo

![](outputs/graficos/anim_sentimientos_periodos.gif)

## Evolución mensual (clic en la leyenda para mostrar u ocultar)

```{r}
d <- readRDS("data/processed/listo_sent.rds")$d
cols <- grep("^s_", names(d), value = TRUE)
base <- d |> filter(usable)
men <- base |> select(mes, all_of(cols)) |>
  pivot_longer(-mes, names_to = "sentimiento", values_to = "n") |>
  mutate(sentimiento = sub("^s_", "", sentimiento)) |>
  group_by(mes, sentimiento) |> summarise(m = sum(n > 0), .groups = "drop") |>
  left_join(base |> count(mes, name = "nb"), by = "mes") |> filter(nb >= 50) |>
  mutate(tasa = round(m / nb * 100, 2))
plot_ly(men, x = ~mes, y = ~tasa, color = ~sentimiento, type = "scatter", mode = "lines+markers") |>
  oscuro(yaxis = list(title = "% de mensajes"), xaxis = list(title = ""))
```

## Sentimientos por periodo

```{r}
tab <- readr::read_csv("outputs/tablas/sentimientos_por_periodo.csv", show_col_types = FALSE)
orden <- c(readr::read_csv("data/processed/periodos.csv", show_col_types = FALSE)$etiqueta, "Fuera de temporada")
orden <- intersect(orden, unique(tab$periodo))
hm <- tab |> select(periodo, sentimiento, tasa100) |> mutate(tasa100 = round(tasa100, 2)) |>
  pivot_wider(names_from = periodo, values_from = tasa100)
plot_ly(x = orden, y = hm$sentimiento, z = as.matrix(hm[, orden]), type = "heatmap", colors = "Blues") |> oscuro()
```

## Sankey interactivo: periodo → sentimiento

```{r}
tab <- tab |> filter(mens_con > 0)
nodos <- data.frame(name = c(orden, unique(tab$sentimiento)))
enl <- tab |> mutate(source = match(periodo, nodos$name) - 1, target = match(sentimiento, nodos$name) - 1, value = mens_con) |>
  as.data.frame()
sk <- sankeyNetwork(Links = enl, Nodes = nodos, Source = "source", Target = "target", Value = "value",
                    NodeID = "name", fontSize = 13, nodeWidth = 24, sinksRight = TRUE)
htmlwidgets::onRender(sk, "function(el){ d3.select(el).selectAll('.node text').style('fill','#ffffff'); }")
```
)---", "interactivos.qmd")

# ---------------- validacion.qmd: reemplaza el iframe del Sankey ----------------
v <- paste(readLines("validacion.qmd", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
nuevo <- paste0("```{r}\n",
                "suppressMessages(library(networkD3))\n",
                "lk <- readr::read_csv(\"outputs/tablas/sankey_v1_v2.csv\", show_col_types = FALSE)\n",
                "nodos <- data.frame(name = unique(c(lk$from, lk$to)))\n",
                "enl <- as.data.frame(lk)\n",
                "enl$source <- match(enl$from, nodos$name) - 1; enl$target <- match(enl$to, nodos$name) - 1\n",
                "sk <- sankeyNetwork(Links = enl, Nodes = nodos, Source = \"source\", Target = \"target\", Value = \"n\",\n",
                "                    NodeID = \"name\", fontSize = 13, nodeWidth = 24, height = 650)\n",
                "htmlwidgets::onRender(sk, \"function(el){ d3.select(el).selectAll('.node text').style('fill','#ffffff'); }\")\n",
                "```\n")
v2 <- gsub("```\\{r\\}\\n#\\| output: asis\\nif \\(file.exists\\(\"outputs/graficos/sankey_v1_v2.html\"\\)\\)[\\s\\S]*?```\\n",
           gsub("\\", "\\\\", nuevo, fixed = TRUE), v, perl = TRUE)
if (identical(v, v2)) message("AVISO: no se encontro el bloque del Sankey en validacion.qmd") 
escribir(v2, "validacion.qmd")

cat("Archivos escritos. Renderizando el sitio...\n")
ok <- tryCatch({ quarto::quarto_render(); TRUE },
               error = function(e) { message("ERROR al renderizar: ", conditionMessage(e)); FALSE })
if (ok) cat("\nListo. Abre docs/index.html y revisa Interactivos y Validacion.\n")