escribir <- function(txt, f) writeLines(txt, f, useBytes = TRUE)

# ---------------- _quarto.yml ----------------
escribir(r"---(project:
  type: website
  output-dir: docs
  render:
    - index.qmd
    - metodologia.qmd
    - calidad.qmd
    - sentimientos.qmd
    - texto.qmd
    - extras.qmd
    - interactivos.qmd
    - bitacora.qmd
    - conclusiones.qmd
website:
  title: "Sentimientos en Telegram"
  page-footer: "Proyecto UMSA · Análisis de texto y sentimientos · Todo replicable en R"
  navbar:
    left:
      - {href: index.qmd, text: Inicio}
      - {href: metodologia.qmd, text: Metodología}
      - {href: calidad.qmd, text: Calidad y limpieza}
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

# ---------------- styles.css ----------------
escribir(r"---(body { background: radial-gradient(1100px 600px at 10% -10%, #1b2a41 0%, #0d1117 60%) fixed !important; }
.kpis { display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 14px; margin: 22px 0; }
.kpi { background: rgba(255,255,255,.06); border: 1px solid rgba(255,255,255,.14); border-radius: 14px; padding: 16px; text-align: center; }
.kpi .v { font-size: 1.9rem; font-weight: 700; color: #4cc9f0; }
.kpi .t { font-size: .85rem; opacity: .8; }
img { max-width: 100%; border-radius: 10px; background: #fff; padding: 6px; }
table { font-size: .9rem; }
.hallazgo { border-left: 4px solid #4cc9f0; padding: 6px 14px; margin: 10px 0; background: rgba(76,201,240,.08); border-radius: 0 8px 8px 0; }
)---", "styles.css")

# ---------------- .gitignore (protege datos privados) ----------------
escribir(c("data/", "validacion/", ".Rproj.user/", ".Rhistory", ".RData", "*.rds", "/.quarto/", "_nombres_LOCAL.rds"), ".gitignore")

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

Analizamos el chat exportado de un grupo de Telegram del curso de temporada (desde 2022) para medir **qué emociones expresan los estudiantes, cuándo y sobre qué temas**. Usamos un **diccionario propio de 15 sentimientos** (extensible) y lo comparamos con la librería NRC (8 emociones).

## Hallazgos principales

::: {.hallazgo}
**La frustración se concentra en "inscripción y pagos"**: aparece 3,3 veces más que el promedio en ese tema (caídas del sistema y problemas de pago).
:::

::: {.hallazgo}
**Las dudas sí se atienden**: los mensajes con confusión se responden en una mediana de 5,4 min (promedio general: 6,2) y los de preocupación en 3,8 min.
:::

::: {.hallazgo}
**Hombres y mujeres expresan casi igual**: de 15 sentimientos, solo la tristeza difiere de forma significativa (más en mujeres, p ajustado = 0,019).
:::

::: {.hallazgo}
**El chat es estacional**: los picos de actividad coinciden con las temporadas de Invierno (may–ago) y Verano (dic–feb), y los inviernos son más positivos que los veranos.
:::

## Cómo recorrer el sitio

Metodología → Calidad y limpieza → Sentimientos → Texto y temas → Extras → Interactivos → Bitácora → Conclusiones.
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
5. **Sentimientos**: diccionario propio de 15 categorías (palabras y emojis) + comparación con NRC/syuzhet.
6. **Texto**: tokenización, stopwords (conservando negaciones), TF-IDF, bigramas, ley de Zipf, temas con LDA.
7. **Extras**: picos, hora × día, velocidad de respuesta, índice emocional, núcleo de usuarios, validación manual.

## Replicabilidad

- Parámetros en un solo archivo (`R01_config.R`), semilla fija (42), rutas relativas, sin ventanas de selección.
- Cada paso queda en la [bitácora](bitacora.qmd).
- Para añadir un sentimiento nuevo basta agregar filas al archivo `lexicon_sentimientos.csv`.

## Los 15 sentimientos del diccionario propio

```{r}
L <- readr::read_csv("lexicon_sentimientos.csv", show_col_types = FALSE)
tb <- L |> dplyr::count(sentimiento, tipo) |> tidyr::pivot_wider(names_from = tipo, values_from = n, values_fill = 0)
knitr::kable(tb)
```

## Periodos

```{r}
P <- readr::read_csv("data/processed/periodos.csv", show_col_types = FALSE)
knitr::kable(P[, c("etiqueta", "inicio", "fin")], col.names = c("Periodo", "Inicio", "Fin"))
```

## Ética y privacidad

Los usuarios se anonimizan; el sexo se **infiere por nombre** (diccionario propio), solo se usa de forma agregada y puede ser incorrecto; no se publica ningún texto de mensajes.
)---", "metodologia.qmd")

# ---------------- calidad.qmd ----------------
escribir(r"---(---
title: "Calidad de datos y limpieza"
---

**Principio:** no se elimina ninguna fila; el ruido se *marca* con columnas verdadero/falso y se puede filtrar o revertir.

```{r}
#| output: asis
d <- readRDS("data/processed/listo_sent.rds")$d
msg <- sum(d$es_msg)
tb <- tibble::tibble(
  Marca = c("Eventos de servicio (no son mensajes)", "Sin texto", "Solo multimedia", "Solo enlace", "Solo emoji",
            "Muy corto (1-2 palabras)", "Repetido (3+ palabras)", "En ráfaga (5+ seguidos)",
            "Aptos para análisis de texto", "Mensajes con emojis"),
  Filas = c(sum(!d$es_msg), sum(d$sin_texto), sum(d$solo_multimedia), sum(d$solo_link), sum(d$solo_emoji),
            sum(d$muy_corto), sum(d$es_repetido), sum(d$rafaga), sum(d$apto_texto), sum(d$apto_emoji)))
tb$`% de los mensajes` <- c(NA, round(tb$Filas[-1] / msg * 100, 1))
print(knitr::kable(tb))
cat("\n\n**Filas totales:**", nrow(d), "· **mensajes:**", msg, "· **mensajes analizados para sentimientos:**", sum(d$usable), "\n")
```

Un mismo mensaje puede tener varias marcas. Los anuncios largos (más de 50 palabras) se marcan y no se analizan para sentimientos.

## Qué se corrigió respecto al preprocesamiento inicial

- Texto en formato lista (enlaces, negritas): se aplanó correctamente, sin residuos como "listtype" o "bold".
- Usuario identificado por `from_id` (no por nombre, ni "rellenando hacia abajo").
- Emojis y reacciones conservados como fuente emocional.
- Negaciones ("no", "sin", "pero") conservadas en el análisis.
)---", "calidad.qmd")

# ---------------- sentimientos.qmd ----------------
escribir(r"---(---
title: "Sentimientos"
---

## Visión general (diccionario propio, 15 categorías)

![](outputs/graficos/g_sentimientos_general.png)

## Librería NRC (8) frente a diccionario propio (15)

![](outputs/graficos/g_nrc_vs_propio.png)

NRC es un diccionario inglés traducido: detecta muchísimo más (confianza, anticipación) pero con palabras ambiguas. El diccionario propio es más conservador y cubre 15 categorías pensadas para este contexto.

## Por periodo

![](outputs/graficos/g_heatmap_periodos.png)

![](outputs/graficos/g_invierno_verano.png)

## Flujo periodo → sentimiento (Sankey)

![](outputs/graficos/g_sankey_periodos.png)

La versión interactiva está en la página [Interactivos](interactivos.qmd).

## Reacciones

![](outputs/graficos/g_reacciones_sentimiento.png)

La reacción más usada es el pulgar arriba (aprobación): reaccionar en este grupo es sobre todo *aprobar*.

## Hombres y mujeres (sexo inferido por nombre)

![](outputs/graficos/g_sentimientos_sexo.png)

Solo la **tristeza** muestra diferencia significativa (más en mujeres). Limitación: gran parte de los mensajes quedó como *indeterminado* porque no se pudo inferir el sexo.
)---", "sentimientos.qmd")

# ---------------- texto.qmd ----------------
escribir(r"---(---
title: "Texto y temas"
---

## Palabras y bigramas

![](outputs/graficos/g_top_palabras.png)

![](outputs/graficos/g_bigramas.png)

![](outputs/graficos/g_red_bigramas.png)

## Palabras distintivas (TF-IDF)

![](outputs/graficos/g_tfidf_temporada.png)

![](outputs/graficos/g_tfidf_periodos.png)

## Nubes de palabras por sentimiento

![](outputs/graficos/g_nubes_sentimientos.png)

## Lingüística superficial

![](outputs/graficos/g_zipf.png)

El corpus sigue la ley de Zipf. La riqueza léxica (TTR) es mayor en "fuera de temporada" solo porque hay menos texto: el TTR baja al crecer el corpus.

## Temas (LDA, K = 6)

![](outputs/graficos/g_temas_lda.png)

![](outputs/graficos/g_tema_periodo.png)

## Tema × sentimiento

![](outputs/graficos/g_tema_sentimiento.png)

::: {.hallazgo}
**Inscripción y pagos** concentra la frustración (3,3 veces el promedio); **Docentes y contenidos** concentra la curiosidad (1,6 veces).
:::
)---", "texto.qmd")

# ---------------- extras.qmd ----------------
escribir(r"---(---
title: "Análisis extra"
---

## Días pico (preocupación, frustración y confusión)

![](outputs/graficos/g_picos_dia_preocupacion.png)

![](outputs/graficos/g_picos_dia_frustracion.png)

![](outputs/graficos/g_picos_dia_confusion.png)

El pico de frustración de julio de 2025 se asocia a palabras como *cayó, sistema, banco*: una caída del sistema de pagos.

## Cuándo se escribe y cómo cambian los sentimientos

![](outputs/graficos/g_heatmap_hora_dia.png)

![](outputs/graficos/g_sentimiento_hora.png)

## ¿Quién recibe respuesta y qué tan rápido?

![](outputs/graficos/g_respuesta_sentimiento.png)

::: {.hallazgo}
Hipótesis descartada: los mensajes con **confusión no esperan más** (mediana 5,4 min frente a 6,2 del promedio). Los de preocupación se responden aún más rápido (3,8 min).
:::

## Índice emocional

![](outputs/graficos/g_indice_emocional.png)

![](outputs/graficos/g_pos_vs_neg.png)

![](outputs/graficos/g_indice_periodos.png)

El índice general es de +0,21: hay más emociones positivas que negativas, y los inviernos son más positivos que los veranos.

## Núcleo activo frente al resto

![](outputs/graficos/g_nucleo_vs_resto.png)

![](outputs/graficos/g_top_usuarios.png)

Los 26 usuarios más activos (100+ mensajes) expresan menos sentimiento que el resto (20,2 % frente a 25 %): su aporte es más informativo.

## Validación del diccionario (100 mensajes etiquetados a mano)

```{r}
f <- "outputs/tablas/validacion_diccionario.csv"
if (file.exists(f)) knitr::kable(readr::read_csv(f, show_col_types = FALSE)) else cat("Validación manual pendiente de completar.")
```
)---", "extras.qmd")

# ---------------- interactivos.qmd ----------------
escribir(r"---(---
title: "Visualizaciones interactivas"
---

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

# ---------------- bitacora.qmd ----------------
escribir(r"---(---
title: "Bitácora de ejecución"
---

Cada script del proyecto registra automáticamente su paso (fecha, etapa, registros antes y después, salida y estado). Las filas marcadas `reconstruida` provienen de las fechas reales de los archivos generados antes de activar el registro automático.

```{r}
b <- readr::read_csv("bitacora/bitacora.csv", show_col_types = FALSE,
                     col_types = readr::cols(fecha = "c", etapa = "c", accion = "c", salida = "c", estado = "c", origen = "c", .default = "d"))
DT::datatable(b, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
```

## Tres niveles de actualización

1. **Automática**: cada script llama a `registrar()` y añade su fila al ejecutarse.
2. **Re-render**: al ejecutar `quarto::quarto_render()` esta página relee el CSV y se actualiza sola.
3. **Manual**: las notas y la tabla de errores se editan en este archivo (`bitacora.qmd`).

## Errores encontrados y cómo se resolvieron

| Problema | Causa | Solución |
|---|---|---|
| No se encontraba `R/01_config.R` | Windows no permite `/` en el nombre del archivo | Nombres `R01_config.R` y `source("R01_config.R")` |
| Faltaba el paquete `reshape2` en LDA | Dependencia que `tidytext` necesita | `install.packages("reshape2")` |
| `Invalid plot index` en RStudio | Panel Plots saturado por muchos gráficos | Limpiar el panel y usar `graphics.off()`; los gráficos ya estaban guardados |
| Descarga de `sf`, `s2`, `BH` con timeout | Conexión lenta con CRAN | No eran necesarios para el análisis |
| Bitácora con una sola fila | `R01_config.R` reiniciaba el registro | Registro acumulativo + reconstrucción desde archivos |
)---", "bitacora.qmd")

# ---------------- conclusiones.qmd ----------------
escribir(r"---(---
title: "Limitaciones y conclusiones"
---

## Conclusiones

- El chat gira en torno a **materias, paralelos, horarios, retiro y adición**, con picos estacionales en Invierno y Verano.
- La **curiosidad** y la **gratitud** dominan entre los sentimientos detectados; el chat es sobre todo un espacio de preguntas.
- La **frustración** se asocia a inscripción y pagos, incluyendo una caída del sistema en julio de 2025.
- Las dudas y preocupaciones **se atienden rápido**: el grupo responde más a quien expresa emociones negativas.
- **Hombres y mujeres expresan casi lo mismo**; solo la tristeza difiere (más en mujeres).
- El índice emocional es positivo (+0,21) y los **inviernos son más positivos que los veranos**.

## Limitaciones

- El diccionario detecta palabras y emojis: no entiende ironía ni contexto, y solo una parte de los mensajes (alrededor de un cuarto) tiene señal emocional.
- El sexo se **infiere por nombre**; una parte importante de los mensajes quedó como indeterminada y la inferencia puede fallar.
- Se analiza un solo grupo de Telegram: los resultados no se generalizan a otros cursos.
- Los picos diarios se basan en pocos mensajes y se interpretan con cautela.

## Trabajo futuro

Modelos supervisados con etiquetas manuales, *embeddings* en español, detección de ironía y comparación entre cursos.
)---", "conclusiones.qmd")

cat("Archivos creados: _quarto.yml, styles.css, .gitignore y 9 paginas .qmd\n")