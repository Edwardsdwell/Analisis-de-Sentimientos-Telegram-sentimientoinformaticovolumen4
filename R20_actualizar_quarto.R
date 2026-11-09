
escribir <- function(txt, f) writeLines(txt, f, useBytes = TRUE)

# ---------------- _quarto.yml ----------------
escribir(r"---(project:
  type: website
  output-dir: docs
  render:
    - index.qmd
    - metodologia.qmd
    - calidad.qmd
    - validacion.qmd
    - sentimientos.qmd
    - texto.qmd
    - extras.qmd
    - interactivos.qmd
    - bitacora.qmd
    - conclusiones.qmd
  resources:
    - outputs/graficos/*.html
    - outputs/graficos/lib_widgets
    - outputs/graficos/*_files
website:
  title: "Sentimientos en Telegram"
  page-footer: "Proyecto UMSA · Minería de Datos · Análisis de texto y sentimientos · Todo replicable en R"
  navbar:
    left:
      - {href: index.qmd, text: Inicio}
      - {href: metodologia.qmd, text: Metodología}
      - {href: calidad.qmd, text: Calidad}
      - {href: validacion.qmd, text: Validación}
      - {href: sentimientos.qmd, text: Sentimientos}
      - {href: texto.qmd, text: Texto y temas}
      - {href: extras.qmd, text: Extras}
      - {href: interactivos.qmd, text: Interactivos}
      - {href: bitacora.qmd, text: Bitácora}
      - {href: conclusiones.qmd, text: Conclusiones}
format:
  html:
    theme: darkly
    css: styles.css
    toc: true
    toc-depth: 2
    smooth-scroll: true
execute:
  echo: false
  warning: false
  message: false
)---", "_quarto.yml")

# ---------------- styles.css (agrega estilo para los graficos interactivos) ----------------
css <- readLines("styles.css", warn = FALSE, encoding = "UTF-8")
if (!any(grepl("iframe.w", css, fixed = TRUE)))
  css <- c(css, "iframe.w { width: 100%; border: 1px solid rgba(255,255,255,.14); border-radius: 10px; background: #fff; margin: 8px 0 18px; }")
escribir(css, "styles.css")

# ---------------- index.qmd ----------------
escribir(r"---(---
title: "Análisis de sentimientos en un grupo de Telegram"
subtitle: "Curso de Temporada · 2022-2026 · 15 sentimientos con diccionario propio"
---

```{r}
#| output: asis
suppressMessages({library(dplyr); library(lubridate)})
d <- readRDS("data/processed/listo_sent.rds")$d
f <- function(x) format(x, big.mark = " ", scientific = FALSE)
kpi <- function(v, t) sprintf('<div class="kpi"><div class="v">%s</div><div class="t">%s</div></div>', v, t)
cat('<div class="kpis">',
    kpi(f(nrow(d)), "elementos exportados"),
    kpi(f(sum(d$es_msg)), "mensajes"),
    kpi(f(n_distinct(d$usuario[d$es_msg], na.rm = TRUE)), "usuarios que escribieron"),
    kpi(f(sum(d$usable)), "mensajes analizados"),
    kpi(paste0(round(mean(d$n_senal[d$usable] > 0) * 100, 1), " %"), "con algún sentimiento"),
    kpi(paste(format(as_date(min(d$fecha)), "%m/%Y"), "–", format(as_date(max(d$fecha)), "%m/%Y")), "periodo cubierto"),
    '</div>', sep = "\n")
```

## ¿De qué trata?

Analizamos el chat exportado de un grupo de Telegram del curso de temporada (desde 2022) para medir **qué emociones expresan los estudiantes, cuándo y sobre qué temas**. Usamos un **diccionario propio de 15 sentimientos** (extensible), lo **validamos con 100 mensajes etiquetados a mano** y lo comparamos con la librería NRC (8 emociones).

## Hallazgos principales

```{r}
#| output: asis
curi <- round(mean(d$s_curiosidad[d$usable] > 0) * 100, 1)
npub <- sum(d$es_publicidad, na.rm = TRUE)
fa <- "outputs/tablas/validacion_ablacion.csv"
txt_val <- ""
if (file.exists(fa)) {
  a <- readr::read_csv(fa, show_col_types = FALSE)
  a1 <- a[1, ]; a2 <- a[nrow(a), ]
  txt_val <- sprintf("Al mejorar el diccionario (contexto, negaciones, preguntas y publicidad), la **precisión subió de %d %% a %d %%** y el **recall de %d %% a %d %%** (F1: %.2f → %.2f) sobre los 100 mensajes etiquetados.",
                     round(a1$precision * 100), round(a2$precision * 100), round(a1$recall * 100), round(a2$recall * 100), a1$F1, a2$F1)
}
h <- function(x) cat('::: {.hallazgo}\n', x, '\n:::\n\n', sep = "")
h(sprintf("**Uno de cada tres mensajes es una pregunta**: la curiosidad aparece en el %s %% de los mensajes analizados. El grupo se usa sobre todo para resolver dudas.", format(curi, decimal.mark = ",")))
if (nzchar(txt_val)) h(txt_val)
h(sprintf("**Se excluyó la publicidad**: %s mensajes (invitaciones a otros grupos, formularios, convocatorias) salen del análisis; casi dos de cada tres enlaces del chat son invitaciones de WhatsApp.", f(npub)))
h("**La frustración y la preocupación se concentran en inscripción, cupos y pagos**, y suben en los periodos de inscripción (ver Extras y Texto y temas).")
h("**El perfil emocional de Invierno, Verano y fuera de temporada es parecido**; lo que cambia es el volumen y los temas (ver el radar en Sentimientos).")
```

## Cómo recorrer el sitio

Metodología → Calidad → Validación → Sentimientos → Texto y temas → Extras → Interactivos → Bitácora → Conclusiones.
)---", "index.qmd")

# ---------------- metodologia.qmd ----------------
escribir(r"---(---
title: "Metodología"
---

## Flujo de trabajo (todo en R)

1. **Ingesta**: lectura de `result.json` desde `media.zip` sin modificar el original (huella SHA-256 registrada).
2. **Limpieza**: se *marcan* (no se borran) mensajes de servicio, sin texto, solo multimedia, solo enlace, muy cortos, repetidos y ráfagas.
3. **Anonimización**: cada usuario se reemplaza por un hash con sal; los nombres no salen del equipo.
4. **Periodos**: tabla configurable (Invierno may–ago, Verano dic–feb, fuera de temporada).
5. **Sentimientos**: diccionario propio de 15 categorías (palabras y emojis), **versión 2 con contexto** (negaciones, preguntas, cortesía, publicidad) + comparación con NRC/syuzhet.
6. **Validación**: 100 mensajes etiquetados a mano; precisión, recall y F1 por sentimiento y por mejora (ver [Validación](validacion.qmd)).
7. **Texto**: tokenización, stopwords (conservando negaciones), TF-IDF, bigramas, ley de Zipf, temas con LDA.
8. **Extras**: picos, hora × día, velocidad de respuesta, índice emocional, núcleo de usuarios, radar y relojes polares.
9. **Calidad**: embudo de datos, dominios, puntaje de spam y 10 chequeos de integridad.

## Replicabilidad

- Parámetros en un solo archivo (`R01_config.R`), semilla fija (42), rutas relativas, sin ventanas de selección.
- Cada paso queda en la [bitácora](bitacora.qmd).
- Para añadir un sentimiento nuevo basta agregar filas al archivo del diccionario (`lexicon_sentimientos_v2.csv`).

## Los 15 sentimientos del diccionario propio (v2)

```{r}
fl <- if (file.exists("lexicon_sentimientos_v2.csv")) "lexicon_sentimientos_v2.csv" else "lexicon_sentimientos.csv"
L <- readr::read_csv(fl, show_col_types = FALSE)
tb <- L |> dplyr::count(sentimiento, tipo) |> tidyr::pivot_wider(names_from = tipo, values_from = n, values_fill = 0)
knitr::kable(tb)
```

## Periodos

```{r}
P <- readr::read_csv("data/processed/periodos.csv", show_col_types = FALSE)
knitr::kable(P[, c("etiqueta", "inicio", "fin")], col.names = c("Periodo", "Inicio", "Fin"))
```

## Ética y privacidad

Los usuarios se anonimizan; el sexo se **infiere por nombre** (diccionario propio), solo se usa de forma agregada y puede ser incorrecto; no se publica ningún texto de mensajes (de los enlaces solo se muestra el dominio).
)---", "metodologia.qmd")

# ---------------- calidad.qmd ----------------
escribir(r"---(---
title: "Calidad de datos y limpieza"
---

**Principio:** no se elimina ninguna fila; el ruido se *marca* con columnas verdadero/falso y se puede filtrar o revertir.

## Embudo de datos

```{r}
ff <- "outputs/tablas/calidad_embudo.csv"
if (file.exists(ff)) {
  e <- readr::read_csv(ff, show_col_types = FALSE)
  knitr::kable(e, col.names = c("Paso", "Filas", "% del total", "Perdidas en el paso"), format.args = list(big.mark = " "))
} else cat("Ejecuta R19_calidad.R")
```

![](outputs/graficos/g_calidad_embudo.png)

## Marcas de limpieza

```{r}
d <- readRDS("data/processed/listo_sent.rds")$d
msg <- sum(d$es_msg)
tb <- tibble::tibble(
  Marca = c("Eventos de servicio (no son mensajes)", "Sin texto", "Solo multimedia", "Solo enlace", "Solo emoji",
            "Muy corto (1-2 palabras)", "Repetido (3+ palabras)", "En ráfaga (5+ seguidos)",
            "Publicidad (invitaciones, formularios)", "Mensajes con emojis"),
  Filas = c(sum(!d$es_msg), sum(d$sin_texto), sum(d$solo_multimedia), sum(d$solo_link), sum(d$solo_emoji),
            sum(d$muy_corto), sum(d$es_repetido), sum(d$rafaga), sum(d$es_publicidad), sum(d$apto_emoji)))
tb$`% de los mensajes` <- c(NA, round(tb$Filas[-1] / msg * 100, 1))
knitr::kable(tb, format.args = list(big.mark = " "))
```

Un mismo mensaje puede tener varias marcas. Los anuncios largos y la publicidad se marcan y no se analizan para sentimientos.

## Enlaces: qué se comparte (solo dominios)

![](outputs/graficos/g_calidad_dominios.png)

## Puntaje de spam y sensibilidad del umbral

Cada mensaje recibe un puntaje (enlaces, invitaciones, muchos emojis, MAYÚSCULAS, repetido, muy largo). El filtro final solo saca invitaciones y convocatorias; la gráfica muestra cuánto coincide el puntaje con ese filtro según el umbral elegido.

![](outputs/graficos/g_calidad_spam_umbral.png)

```{r}
fs <- "outputs/tablas/calidad_spam_sensibilidad.csv"
if (file.exists(fs)) knitr::kable(readr::read_csv(fs, show_col_types = FALSE))
```

## Chequeos de integridad

```{r}
fc <- "outputs/tablas/calidad_chequeos.csv"
if (file.exists(fc)) {
  ch <- readr::read_csv(fc, show_col_types = FALSE)
  cat("**", sum(ch$resultado == "OK"), " de ", nrow(ch), " chequeos en OK**\n\n", sep = "")
  print(knitr::kable(ch, col.names = c("Chequeo", "Qué verifica", "Resultado", "Detalle")))
}
```

## Qué se corrigió respecto al preprocesamiento inicial

- Texto en formato lista (enlaces, negritas): se aplanó correctamente, sin residuos como "listtype" o "bold".
- Usuario identificado por `from_id` (no por nombre, ni "rellenando hacia abajo").
- Emojis y reacciones conservados como fuente emocional.
- Negaciones ("no", "sin", "pero") conservadas en el análisis.
)---", "calidad.qmd")

# ---------------- validacion.qmd (NUEVA) ----------------
escribir(r"---(---
title: "Validación del diccionario"
---

Etiquetamos **a mano 100 mensajes** (60 con señal y 40 sin señal, muestra estratificada) y comparamos lo que detecta el diccionario con lo que realmente expresan. Así sabemos **qué tan confiable es**.

## Cómo se mide

- **Precisión**: de lo que el diccionario marca, cuánto es correcto.
- **Recall**: de lo que realmente hay, cuánto encuentra.
- **F1**: resumen de las dos.

## Qué aporta cada mejora

```{r}
fa <- "outputs/tablas/validacion_ablacion.csv"
if (file.exists(fa)) {
  a <- readr::read_csv(fa, show_col_types = FALSE)
  a[, -1] <- lapply(a[, -1], function(x) round(x, 3))
  knitr::kable(a, col.names = c("Etapa", "Precisión", "Recall", "F1", "Acierto binario", "Coincidencia exacta"))
}
```

![](outputs/graficos/g_validacion_ablacion.png)

## F1 por sentimiento: versión 1 frente a versión 2

![](outputs/graficos/g_validacion_f1.png)

```{r}
fp <- "outputs/tablas/validacion_v1_v2_por_sentimiento.csv"
if (file.exists(fp)) {
  p <- readr::read_csv(fp, show_col_types = FALSE)
  p[, c("precision", "recall", "F1")] <- lapply(p[, c("precision", "recall", "F1")], function(x) round(x, 2))
  knitr::kable(p[p$version == "v2", c("sentimiento", "TP", "FP", "FN", "precision", "recall", "F1")])
}
```

## Cambios hechos al diccionario (v1 → v2)

```{r}
fc <- "outputs/tablas/lexicon_v2_cambios.csv"
if (file.exists(fc)) knitr::kable(readr::read_csv(fc, show_col_types = FALSE))
```

## Efecto en todo el corpus

![](outputs/graficos/g_comparacion_v1_v2.png)

## Sankey entre métodos: v1 → v2

Cada flujo muestra a qué sentimiento dominante pasó un mensaje al cambiar de versión (incluye "publicidad (excluida)" y "sin señal").

```{r}
#| output: asis
if (file.exists("outputs/graficos/sankey_v1_v2.html"))
  cat('<iframe class="w" src="outputs/graficos/sankey_v1_v2.html" height="650"></iframe>\n') else
  cat('![](outputs/graficos/g_sankey_v1_v2.png)\n')
```

## Límites de esta validación

- La evaluación es **sobre la misma muestra** con la que se ajustó el diccionario, por lo que los números son algo optimistas.
- Etiquetó **una sola persona**; con dos se podría medir el acuerdo (κ de Cohen).
- Son 100 mensajes: las cifras por sentimiento con pocos casos son orientativas.
)---", "validacion.qmd")

# ---------------- sentimientos.qmd ----------------
escribir(r"---(---
title: "Sentimientos"
---

## Visión general (diccionario propio v2, 15 categorías)

![](outputs/graficos/g_sentimientos_general.png)

La **curiosidad** (preguntas) domina; después vienen la gratitud, la frustración y la alegría.

## Librería NRC (8) frente a diccionario propio (15)

![](outputs/graficos/g_nrc_vs_propio.png)

NRC es un diccionario inglés traducido: detecta muchísimo más (confianza, anticipación) pero con palabras ambiguas. El diccionario propio es más conservador y cubre 15 categorías pensadas para este contexto.

## Radar de emociones: Invierno, Verano y fuera de temporada

![](outputs/graficos/g_radar_temporadas.png)

El perfil es muy parecido entre temporadas. La versión interactiva está en [Interactivos](interactivos.qmd).

## Por periodo

![](outputs/graficos/g_heatmap_periodos.png)

![](outputs/graficos/g_invierno_verano.png)

## Flujo periodo → sentimiento (Sankey)

![](outputs/graficos/g_sankey_periodos.png)

## Reloj de actividad: hora del día y tono

![](outputs/graficos/g_polar_hora.png)

![](outputs/graficos/g_polar_semana.png)

## Reacciones

![](outputs/graficos/g_reacciones_sentimiento.png)

La reacción más usada es el pulgar arriba (aprobación): reaccionar en este grupo es sobre todo *aprobar*.

## Hombres y mujeres (sexo inferido por nombre)

![](outputs/graficos/g_sentimientos_sexo.png)

Limitación: gran parte de los mensajes quedó como *indeterminado* porque no se pudo inferir el sexo; los resultados se leen con cautela.
)---", "sentimientos.qmd")

# ---------------- extras.qmd ----------------
escribir(r"---(---
title: "Análisis extra"
---

## Días pico (preocupación, frustración y confusión)

![](outputs/graficos/g_picos_dia_preocupacion.png)

![](outputs/graficos/g_picos_dia_frustracion.png)

![](outputs/graficos/g_picos_dia_confusion.png)

Los picos de frustración y preocupación coinciden con momentos de inscripción, cupos y pagos.

## Cuándo se escribe y cómo cambian los sentimientos

![](outputs/graficos/g_heatmap_hora_dia.png)

![](outputs/graficos/g_sentimiento_hora.png)

## ¿Quién recibe respuesta y qué tan rápido?

![](outputs/graficos/g_respuesta_sentimiento.png)

## Índice emocional

![](outputs/graficos/g_indice_emocional.png)

![](outputs/graficos/g_pos_vs_neg.png)

![](outputs/graficos/g_indice_periodos.png)

Con el diccionario v2 el índice emocional general queda cerca de cero: hay un equilibrio entre emociones positivas y negativas, y el verano 2025-26 es el periodo más negativo.

## Núcleo activo frente al resto

![](outputs/graficos/g_nucleo_vs_resto.png)

![](outputs/graficos/g_top_usuarios.png)
)---", "extras.qmd")

# ---------------- interactivos.qmd ----------------
escribir(r"---(---
title: "Visualizaciones interactivas"
---

```{r}
#| output: asis
ifr <- function(f, h = 600) if (file.exists(file.path("outputs/graficos", f)))
  cat(sprintf('<iframe class="w" src="outputs/graficos/%s" height="%d"></iframe>\n\n', f, h)) else cat("*(falta ", f, ")*\n\n", sep = "")
cat("## Radar de emociones por temporada\n\nPasa el mouse para ver el valor; clic en la leyenda para ocultar una temporada.\n\n")
ifr("plotly_radar_temporadas.html", 620)
cat("## Radar por periodo (sin curiosidad)\n\nHaz clic en un periodo de la leyenda para aislarlo.\n\n")
ifr("plotly_radar_periodos.html", 650)
cat("## Reloj de mensajes por hora del día\n\n")
ifr("plotly_polar_hora.html", 620)
cat("## Qué aporta cada mejora del diccionario\n\n")
ifr("plotly_validacion_ablacion.html", 480)
cat("## Sankey entre métodos: v1 → v2\n\n")
ifr("sankey_v1_v2.html", 650)
cat("## Embudo de datos\n\n")
ifr("plotly_calidad_embudo.html", 480)
```

## Animación por periodo

![](outputs/graficos/anim_sentimientos_periodos.gif)

## Evolución mensual (clic en la leyenda para mostrar u ocultar)

```{r}
suppressMessages({library(dplyr); library(tidyr); library(plotly)})
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
  layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117", font = list(color = "#e6edf3"),
         yaxis = list(title = "% de mensajes"), xaxis = list(title = ""))
```

## Sentimientos por periodo

```{r}
tab <- readr::read_csv("outputs/tablas/sentimientos_por_periodo.csv", show_col_types = FALSE)
orden <- c(readr::read_csv("data/processed/periodos.csv", show_col_types = FALSE)$etiqueta, "Fuera de temporada")
orden <- intersect(orden, unique(tab$periodo))
hm <- tab |> select(periodo, sentimiento, tasa100) |> mutate(tasa100 = round(tasa100, 2)) |>
  pivot_wider(names_from = periodo, values_from = tasa100)
plot_ly(x = orden, y = hm$sentimiento, z = as.matrix(hm[, orden]), type = "heatmap", colors = "Blues") |>
  layout(paper_bgcolor = "#0d1117", plot_bgcolor = "#0d1117", font = list(color = "#e6edf3"))
```

## Sankey interactivo: periodo → sentimiento

```{r}
suppressMessages({library(networkD3); library(dplyr)})
tab <- readr::read_csv("outputs/tablas/sentimientos_por_periodo.csv", show_col_types = FALSE) |> filter(mens_con > 0)
orden <- c(readr::read_csv("data/processed/periodos.csv", show_col_types = FALSE)$etiqueta, "Fuera de temporada")
orden <- intersect(orden, unique(tab$periodo))
nodos <- data.frame(name = c(orden, unique(tab$sentimiento)))
enl <- tab |> mutate(source = match(periodo, nodos$name) - 1, target = match(sentimiento, nodos$name) - 1, value = mens_con) |>
  as.data.frame()
sk <- sankeyNetwork(Links = enl, Nodes = nodos, Source = "source", Target = "target", Value = "value",
                    NodeID = "name", fontSize = 13, nodeWidth = 24, sinksRight = TRUE)
htmlwidgets::onRender(sk, "function(el){ d3.select(el).selectAll('.node text').style('fill','#ffffff'); }")
```
)---", "interactivos.qmd")

# ---------------- conclusiones.qmd ----------------
escribir(r"---(---
title: "Limitaciones y conclusiones"
---

## Conclusiones

- El chat gira en torno a **materias, paralelos, horarios, inscripción y cupos**, con picos estacionales en Invierno y Verano.
- **Uno de cada tres mensajes es una pregunta** (curiosidad): el grupo funciona sobre todo como espacio para resolver dudas. Le sigue la **gratitud**.
- La **frustración** y la **preocupación** se asocian a inscripción, cupos y pagos.
- La **publicidad** (invitaciones a otros grupos y formularios) es una parte importante de los enlaces y se excluyó del análisis.
- El **perfil emocional es similar** entre Invierno, Verano y fuera de temporada.
- Mejorar el diccionario con **contexto** (negaciones, preguntas, cortesía, publicidad) subió de forma clara la precisión y el recall frente a la versión inicial (ver [Validación](validacion.qmd)).

## Limitaciones

- El diccionario detecta palabras y emojis: no entiende ironía ni contexto profundo, y solo una parte de los mensajes (alrededor de un tercio) tiene señal emocional.
- La validación usa **100 mensajes**, **un solo etiquetador** y la misma muestra con la que se ajustó el diccionario: las cifras son algo optimistas.
- El sexo se **infiere por nombre**; una parte importante de los mensajes quedó como indeterminada y la inferencia puede fallar.
- Se analiza un solo grupo de Telegram: los resultados no se generalizan a otros cursos.
- Los picos diarios se basan en pocos mensajes y se interpretan con cautela.

## Trabajo futuro

Segundo etiquetador y κ de Cohen, modelos supervisados con las etiquetas manuales, *embeddings* en español, detección de ironía y comparación entre cursos.
)---", "conclusiones.qmd")

cat("Archivos escritos. Renderizando el sitio...\n")
ok <- tryCatch({ quarto::quarto_render(); TRUE },
               error = function(e) { message("ERROR al renderizar: ", conditionMessage(e)); FALSE })
if (ok) {
  registrar("10. Sitio Quarto", "Sitio actualizado con v2, validacion, radar/polar y calidad",
            0, 0, "docs/")
  cat("\nListo. Abre docs/index.html para verlo.\n")
}