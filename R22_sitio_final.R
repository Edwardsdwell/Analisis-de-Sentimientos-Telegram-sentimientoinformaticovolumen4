
escribir <- function(txt, f) writeLines(txt, f, useBytes = TRUE)
if (!file.exists("graficos_web.R")) stop("Falta graficos_web.R en la carpeta del proyecto.")
if (file.exists("extras.qmd")) file.remove("extras.qmd")

# quarto.yml 
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
    - sugerencias.qmd
    - interactivos.qmd
    - bitacora.qmd
    - conclusiones.qmd
website:
  title: "Sentimientos en Telegram"
  page-footer: "Proyecto UMSA · Minería de Datos · Análisis de texto y sentimientos · Todo replicable en R"
  sidebar:
    style: docked
    search: false
    collapse-level: 1
    contents:
      - {href: index.qmd, text: "Inicio"}
      - {href: metodologia.qmd, text: "Metodología"}
      - {href: calidad.qmd, text: "Calidad"}
      - {href: validacion.qmd, text: "Validación"}
      - {href: sentimientos.qmd, text: "Sentimientos"}
      - {href: texto.qmd, text: "Texto y temas"}
      - {href: sugerencias.qmd, text: "Sugerencias"}
      - {href: interactivos.qmd, text: "Interactivos"}
      - {href: bitacora.qmd, text: "Bitácora"}
      - {href: conclusiones.qmd, text: "Conclusiones"}
format:
  html:
    theme: darkly
    css: styles.css
    toc: false
    smooth-scroll: true
    page-layout: article
    grid:
      sidebar-width: 270px
      body-width: 1080px
      margin-width: 20px
execute:
  echo: false
  warning: false
  message: false
)---", "_quarto.yml")

# ---------------- styles.css (con estrellas generadas al azar) ----------------
set.seed(7)
estrellas <- function(n, cols) paste(sprintf("%.1fvw %.1fvh 0 %s", runif(n, 0, 100), runif(n, 0, 100),
                                             sample(cols, n, TRUE)), collapse = ",\n    ")
cols1 <- c("rgba(255,255,255,.9)", "rgba(255,255,255,.6)", "rgba(190,215,255,.8)", "rgba(255,235,200,.7)")
cols2 <- c("rgba(255,255,255,1)", "rgba(180,200,255,.9)", "rgba(255,220,240,.8)")
css <- paste0(r"---(@import url('https://fonts.googleapis.com/css2?family=Bricolage+Grotesque:opsz,wght@12..96,600;12..96,800&family=Inter:wght@400;500;600&display=swap');

:root { --bs-body-bg: #000; --bs-body-color: #e6edf3; --acento: #4cc9f0; }
html, body { background: #000 !important; color: #e6edf3; }
body { font-family: 'Inter', 'Segoe UI', sans-serif; font-size: 1.02rem; }

/* ---------- fondo estrellado ---------- */
body::before, body::after { content: ""; position: fixed; top: 0; left: 0; width: 2px; height: 2px; border-radius: 50%;
  background: transparent; z-index: 0; pointer-events: none; }
body::before { box-shadow:
    )---", estrellas(150, cols1), r"---(;
  animation: titila 5s ease-in-out infinite alternate; }
body::after { width: 3px; height: 3px; box-shadow:
    )---", estrellas(40, cols2), r"---(;
  animation: titila 8s ease-in-out infinite alternate-reverse; }
@keyframes titila { from { opacity: .35; } to { opacity: 1; } }
#quarto-content, #quarto-document-content, .page-columns, main.content, #quarto-sidebar, .nav-footer, footer { position: relative; z-index: 1; }
#quarto-content, .content, main, .page-columns, body, .nav-footer { background: transparent !important; }

/* ---------- sin lineas separadoras en ningun lado ---------- */
hr, .quarto-title-block hr { display: none !important; }
h1, h2, h3, h4, .h2, .h3, .title { border: none !important; border-bottom: none !important; padding-bottom: 0 !important; }
#title-block-header, header, .quarto-title, .quarto-title-meta, .nav-footer, .sidebar, #quarto-sidebar, .sidebar-navigation,
.sidebar-item-container, .sidebar-item, .sidebar-header, .sidebar-tools-main, footer, .page-footer, table, th, td, tr, thead, tbody,
.table > :not(caption) > * > *, details, summary, blockquote, figure, img { border-color: transparent !important; }
.nav-footer { border-top: none !important; }
#quarto-sidebar { border-right: none !important; }
.sidebar-navigation { border-right: none !important; }

/* ---------- menu lateral ---------- */
#quarto-sidebar { background: linear-gradient(180deg, rgba(10,14,32,.92), rgba(4,6,16,.92)) !important; backdrop-filter: blur(6px); }
.sidebar-title, .sidebar-header .sidebar-title a { text-transform: uppercase; letter-spacing: .05em; font-family: 'Bricolage Grotesque', sans-serif; font-weight: 800; font-size: 1.25rem;
  background: linear-gradient(90deg, #4cc9f0, #c77dff); -webkit-background-clip: text; background-clip: text; color: transparent !important; }
.sidebar-item a, .sidebar-link { text-transform: uppercase; letter-spacing: .05em; font-size: .86rem; border-radius: 14px; padding: 9px 14px !important; margin: 3px 6px; color: #cfd8e3 !important;
  font-weight: 500; transition: background .2s, transform .2s; display: block; }
.sidebar-item a:hover { background: rgba(76,201,240,.14); transform: translateX(4px); color: #fff !important; }
.sidebar-item a.active, .sidebar-link.active { background: linear-gradient(90deg, rgba(76,201,240,.35), rgba(199,125,255,.28));
  color: #fff !important; font-weight: 600; box-shadow: 0 0 18px rgba(76,201,240,.25); }

/* ---------- titulos ---------- */
.quarto-title h1.title, h1.title { font-family: 'Bricolage Grotesque', sans-serif; font-weight: 800; font-size: 2.3rem; letter-spacing: .02em; text-transform: uppercase;
  background: linear-gradient(90deg, #4cc9f0 0%, #c77dff 55%, #ff9ff3 100%); -webkit-background-clip: text; background-clip: text; color: transparent; }
.quarto-title .subtitle, .subtitle { color: #aab6c4; font-size: 1.05rem; }
h2, .h2 { font-family: 'Bricolage Grotesque', sans-serif; font-weight: 700; font-size: 1.4rem; text-transform: uppercase; letter-spacing: .04em; color: #fff; margin-top: 2.6rem; margin-bottom: .9rem; }
h2::before { content: ""; display: inline-block; width: 12px; height: 12px; border-radius: 50%; margin-right: 12px;
  background: radial-gradient(circle at 30% 30%, #fff, #4cc9f0 60%, #7209b7); box-shadow: 0 0 12px #4cc9f0; vertical-align: middle; }
h3 { font-family: 'Bricolage Grotesque', sans-serif; color: #dfe7f1; }
a { color: #6fd6f6; }

/* ---------- tarjetas de cristal para graficas ---------- */
.html-widget, .plotly.html-widget { border-radius: 20px; background: rgba(10,14,28,.55); padding: 6px;
  box-shadow: 0 8px 30px rgba(0,0,0,.5), inset 0 0 0 1px rgba(255,255,255,.04); margin: .6rem 0 1.2rem; }
.js-plotly-plot .plotly .modebar { opacity: .35; }
img { background: #fff; border-radius: 18px; padding: 8px; max-width: 100%; box-shadow: 0 8px 28px rgba(0,0,0,.5); }
.aviso { padding: 14px 18px; border-radius: 16px; background: rgba(255,157,0,.12); color: #ffc477; margin: .6rem 0; }

/* ---------- burbujas KPI ---------- */
.kpis { display: flex; flex-wrap: wrap; gap: 20px; margin: 1.2rem 0 1.8rem; }
.kpi { --c: #4cc9f0; width: 148px; height: 148px; border-radius: 50%; display: flex; flex-direction: column; align-items: center;
  justify-content: center; text-align: center; padding: 10px; position: relative;
  background: radial-gradient(circle at 30% 24%, rgba(255,255,255,.42), color-mix(in srgb, var(--c) 30%, transparent) 38%, rgba(8,12,28,.7) 78%);
  box-shadow: inset -10px -12px 26px rgba(0,0,0,.45), inset 6px 6px 16px rgba(255,255,255,.12), 0 0 30px color-mix(in srgb, var(--c) 40%, transparent);
  animation: flotar 6s ease-in-out infinite; }
.kpi:nth-child(2n) { animation-duration: 7.5s; animation-delay: -2s; }
.kpi:nth-child(3n) { animation-duration: 5.5s; animation-delay: -3.5s; }
.kpi .v { font-family: 'Bricolage Grotesque', sans-serif; font-weight: 800; font-size: 1.55rem; color: #fff; line-height: 1.1; }
.kpi .t { font-size: .74rem; color: #dbe7f5; margin-top: 4px; line-height: 1.2; }
@keyframes flotar { 0%, 100% { transform: translateY(0); } 50% { transform: translateY(-9px); } }

/* ---------- portada: KPIs + nota lateral ---------- */
.portada { display: grid; grid-template-columns: 1fr 270px; gap: 26px; align-items: start; }
@media (max-width: 900px) { .portada { grid-template-columns: 1fr; } }

/* ---------- burbujas estilo Telegram ---------- */
.tg { display: flex; flex-direction: column; gap: 14px; margin: 1rem 0 1.8rem; }
.msg { --bg: #182533; --who: #6ab3f3; position: relative; max-width: min(760px, 94%); padding: 11px 16px 24px; border-radius: 20px;
  line-height: 1.5; color: #f1f5f9; background: var(--bg); box-shadow: 0 5px 20px rgba(0,0,0,.5); align-self: flex-start;
  border-bottom-left-radius: 5px; animation: aparece .55s both; }
.msg::before { content: ""; position: absolute; bottom: 0; left: -9px; width: 14px; height: 16px; background: var(--bg);
  clip-path: polygon(100% 0, 100% 100%, 0 100%); }
.msg::after { content: attr(data-t); position: absolute; right: 14px; bottom: 5px; font-size: .7rem; opacity: .6; }
.msg .who { display: block; font-weight: 700; font-size: .82rem; color: var(--who); margin-bottom: 3px; }
.msg p { margin: 0 0 .45rem; } .msg p:last-child { margin-bottom: 0; } .msg ul, .msg ol { margin: .3rem 0 0; padding-left: 1.2rem; }
.msg.out { align-self: flex-end; border-bottom-left-radius: 20px; border-bottom-right-radius: 5px; }
.msg.out::before { left: auto; right: -9px; clip-path: polygon(0 0, 0 100%, 100% 100%); }
.msg.azul   { --bg: #2b5278; --who: #8fd0ff; }
.msg.verde  { --bg: #2d6a4f; --who: #95e6b5; }
.msg.morada { --bg: #54367f; --who: #d3b1ff; }
.msg.naranja{ --bg: #8a4b1f; --who: #ffc58a; }
.msg.rosa   { --bg: #8a3563; --who: #ffb3d9; }
.msg.rojo   { --bg: #7d2b3a; --who: #ffadb9; }
.msg.cian   { --bg: #1f5f6b; --who: #8ff0ff; }
.tg .msg:nth-child(2) { animation-delay: .12s; } .tg .msg:nth-child(3) { animation-delay: .24s; }
.tg .msg:nth-child(4) { animation-delay: .36s; } .tg .msg:nth-child(5) { animation-delay: .48s; } .tg .msg:nth-child(6) { animation-delay: .6s; }
@keyframes aparece { from { opacity: 0; transform: translateY(14px) scale(.96); } to { opacity: 1; transform: none; } }
.nota .msg { font-size: .86rem; padding: 9px 13px 22px; max-width: 100%; }

/* ---------- dos columnas ---------- */
.dos { display: grid; grid-template-columns: 1.35fr 1fr; gap: 30px; align-items: start; margin: 1rem 0 1.6rem; }
@media (max-width: 900px) { .dos { grid-template-columns: 1fr; } }
.dos h2 { margin-top: 0; }
.dos ol { list-style: none; counter-reset: paso; padding: 0; margin: 0; }
.dos ol li { counter-increment: paso; position: relative; padding: 11px 16px 11px 58px; margin-bottom: 10px; border-radius: 18px;
  background: rgba(255,255,255,.05); }
.dos ol li::before { content: counter(paso); position: absolute; left: 12px; top: 50%; transform: translateY(-50%); width: 34px; height: 34px;
  border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: 800; color: #04101c;
  background: radial-gradient(circle at 30% 30%, #fff, #4cc9f0 70%); box-shadow: 0 0 14px rgba(76,201,240,.5); }
.dos ul { padding-left: 1.1rem; }

/* ---------- fichas, burbujas pequenas, chips ---------- */
.fichas { display: grid; grid-template-columns: repeat(auto-fill, minmax(165px, 1fr)); gap: 14px; margin: 1rem 0 1.6rem; }
.ficha { --c: #4cc9f0; padding: 14px 16px; border-radius: 22px; background: linear-gradient(145deg, color-mix(in srgb, var(--c) 22%, rgba(10,14,28,.8)), rgba(10,14,28,.7));
  box-shadow: 0 6px 20px rgba(0,0,0,.45); transition: transform .2s; }
.ficha:hover { transform: translateY(-4px); }
.ficha .n { font-family: 'Bricolage Grotesque', sans-serif; font-size: 1.3rem; font-weight: 800; color: #fff; }
.ficha .l { font-size: .82rem; color: #c9d4e2; line-height: 1.25; }
.ficha .s { font-size: .74rem; color: var(--c); margin-top: 3px; }
.chips { display: flex; flex-wrap: wrap; gap: 10px; margin: .8rem 0 1.4rem; }
.chip { --c: #4cc9f0; padding: 8px 16px; border-radius: 999px; background: color-mix(in srgb, var(--c) 22%, rgba(8,12,24,.8));
  color: #fff; font-size: .9rem; box-shadow: 0 0 14px color-mix(in srgb, var(--c) 30%, transparent); }
.chip b { color: var(--c); margin-left: 6px; }
a.pill { display: inline-block; padding: 9px 18px; margin: 5px 6px 5px 0; border-radius: 999px; text-decoration: none; color: #fff;
  background: linear-gradient(90deg, rgba(76,201,240,.28), rgba(199,125,255,.28)); transition: transform .2s, box-shadow .2s; }
a.pill:hover { transform: translateY(-3px); box-shadow: 0 0 18px rgba(76,201,240,.45); color: #fff; }
.chk { display: grid; grid-template-columns: repeat(auto-fill, minmax(260px, 1fr)); gap: 12px; margin: 1rem 0 1.6rem; }
.chk div { padding: 12px 16px; border-radius: 18px; background: rgba(255,255,255,.05); font-size: .88rem; color: #cfd8e3; }
.chk b { color: #fff; } .chk .ok { color: #2ee6a6; font-weight: 700; } .chk .no { color: #ff6b6b; font-weight: 700; }
.chk small { display: block; color: #8f9db0; margin-top: 2px; }

/* ---------- emojis animados ---------- */
.emo { display: flex; gap: 14px; font-size: 2.2rem; margin: .4rem 0 1rem; }
.emo span { display: inline-block; animation: salta 2.2s ease-in-out infinite; filter: drop-shadow(0 0 8px rgba(255,255,255,.25)); }
.emo span:nth-child(2) { animation-delay: .25s; } .emo span:nth-child(3) { animation-delay: .5s; } .emo span:nth-child(4) { animation-delay: .75s; }
.emo span:nth-child(5) { animation-delay: 1s; } .emo span:nth-child(6) { animation-delay: 1.25s; } .emo span:nth-child(7) { animation-delay: 1.5s; }
@keyframes salta { 0%, 100% { transform: translateY(0) scale(1); } 40% { transform: translateY(-14px) scale(1.18) rotate(-6deg); } 70% { transform: translateY(0) scale(.95); } }
.reac { display: flex; flex-wrap: wrap; gap: 12px; margin: .6rem 0 1rem; }
.reac span { padding: 6px 14px; border-radius: 999px; background: rgba(255,255,255,.08); font-size: 1.3rem; animation: pulso 2.6s ease-in-out infinite; }
.reac span:nth-child(2n) { animation-delay: .6s; } .reac span:nth-child(3n) { animation-delay: 1.2s; }
@keyframes pulso { 0%, 100% { transform: scale(1); } 50% { transform: scale(1.14); box-shadow: 0 0 16px rgba(255,255,255,.3); } }

/* ---------- tablas ---------- */
table { width: 100%; border-collapse: separate !important; border-spacing: 0 6px; font-size: .92rem; }
.table > :not(caption) > * > *, table td, table th { background: transparent !important; box-shadow: none !important; color: #dbe4ef; }
table thead th { color: #7fdcf8 !important; font-family: 'Bricolage Grotesque', sans-serif; font-weight: 700; padding: 8px 14px; }
table tbody tr { background: rgba(255,255,255,.05); transition: background .2s; }
table tbody tr:hover { background: rgba(76,201,240,.14); }
table tbody td { padding: 9px 14px; }
table tbody td:first-child { border-radius: 14px 0 0 14px; } table tbody td:last-child { border-radius: 0 14px 14px 0; }
details.det { margin: .8rem 0 1.4rem; } details.det summary { cursor: pointer; padding: 10px 18px; border-radius: 999px; display: inline-block;
  background: rgba(76,201,240,.16); color: #fff; list-style: none; } details.det summary:hover { background: rgba(76,201,240,.3); }
.nav-footer { color: #7d8aa0; font-size: .85rem; }
.interp { font-size: .88rem; color: #a9b7c8; margin: -.5rem 0 1.5rem; max-width: 900px; line-height: 1.45; }
code { background: rgba(255,255,255,.1) !important; color: #ffd166 !important; border-radius: 8px; padding: 1px 6px; }
)---")
escribir(css, "styles.css")

# ---------------- helpers comunes para las paginas ----------------
ayuda <- r"---(```{r}
#| include: false
suppressMessages({library(dplyr); library(tidyr); library(plotly); library(lubridate)})
source("graficos_web.R", encoding = "UTF-8")
L <- tryCatch(load_d(), error = function(e) NULL)
f <- function(x) format(x, big.mark = " ", scientific = FALSE)
ficha <- function(n, l, s = "", c = "#4cc9f0") sprintf('<div class="ficha" style="--c:%s"><div class="n">%s</div><div class="l">%s</div><div class="s">%s</div></div>', c, n, l, s)
emo <- function(...) cat('<div class="emo">', paste0("<span>", c(...), "</span>", collapse = ""), '</div>', sep = "")
```
)---"

# ---------------- index ----------------
escribir(paste0(r"---(---
title: "Análisis de sentimientos en un grupo de Telegram"
subtitle: "Curso de Temporada · 2022-2026 · 15 sentimientos con diccionario propio"
---

)---", ayuda, r"---(
```{r}
#| output: asis
d <- L$d
kpi <- function(v, t, c) sprintf('<div class="kpi" style="--c:%s"><div class="v">%s</div><div class="t">%s</div></div>', c, v, t)
cat('::: {.portada}\n\n<div>\n<div class="kpis">',
    kpi(f(nrow(d)), "elementos exportados", "#4cc9f0"),
    kpi(f(sum(d$es_msg)), "mensajes", "#c77dff"),
    kpi(f(n_distinct(d$usuario[d$es_msg], na.rm = TRUE)), "usuarios que escribieron", "#ff9ff3"),
    kpi(f(sum(d$usable)), "mensajes analizados", "#06d6a0"),
    kpi(paste0(round(mean(d$n_senal[d$usable] > 0) * 100, 1), " %"), "con algún sentimiento", "#ffd166"),
    kpi(paste(format(as_date(min(d$fecha)), "%m/%Y"), "–", format(as_date(max(d$fecha)), "%m/%Y")), "periodo cubierto", "#f4a261"),
    '</div>\n</div>\n\n', sep = "\n")
cat('::: {.nota}\n::: {.msg .morada data-t="ℹ️"}\n<span class="who">¿De qué trata?</span>\nAnalizamos el chat de un grupo de Telegram del curso de temporada para medir **qué emociones expresan los estudiantes, cuándo y sobre qué temas**, con un **diccionario propio de 15 sentimientos** validado con 100 mensajes etiquetados a mano.\n:::\n:::\n\n:::\n')
```

## Hallazgos principales

```{r}
#| output: asis
curi <- round(mean(d$s_curiosidad[d$usable] > 0) * 100, 1)
npub <- sum(d$es_publicidad, na.rm = TRUE)
fa <- "outputs/tablas/validacion_ablacion.csv"
txt_val <- ""
if (file.exists(fa)) {
  a <- readr::read_csv(fa, show_col_types = FALSE); a1 <- a[1, ]; a2 <- a[nrow(a), ]
  txt_val <- sprintf("Al mejorar el diccionario (contexto, negaciones, preguntas y publicidad), la **precisión subió de %d %% a %d %%** y el **recall de %d %% a %d %%** (F1: %.2f → %.2f) sobre los 100 mensajes etiquetados.",
                     round(a1$precision * 100), round(a2$precision * 100), round(a1$recall * 100), round(a2$recall * 100), a1$F1, a2$F1)
}
h <- function(who, x, col, t) cat('::: {.msg .', col, '  data-t="', t, '"}\n<span class="who">', who, '</span>\n', x, '\n:::\n\n', sep = "")
cat('::: {.tg}\n\n')
h("🤔 Curiosidad", sprintf("**Uno de cada tres mensajes es una pregunta**: la curiosidad aparece en el %s %% de los mensajes analizados. El grupo se usa sobre todo para resolver dudas.", format(curi, decimal.mark = ",")), "azul", "10:41")
if (nzchar(txt_val)) h("✅ Validación", txt_val, "verde", "10:42 ✓✓")
h("🚫 Publicidad", sprintf("**Se excluyó la publicidad**: %s mensajes (invitaciones a otros grupos, formularios, convocatorias) salen del análisis; casi dos de cada tres enlaces del chat son invitaciones de WhatsApp.", f(npub)), "naranja", "10:43")
h("😤 Frustración y preocupación", "**Se concentran en inscripción, cupos y pagos**, y suben en los periodos de inscripción (ver Sugerencias y Texto y temas).", "rojo", "10:44")
h("📡 Perfil emocional", "**El perfil emocional de Invierno, Verano y fuera de temporada es parecido**; lo que cambia es el volumen y los temas (ver el radar en Sentimientos).", "morada", "10:45 ✓✓")
cat(':::\n')
```

## Cómo recorrer el sitio

<a class="pill" href="metodologia.html">Metodología</a><a class="pill" href="calidad.html">Calidad</a><a class="pill" href="validacion.html">Validación</a><a class="pill" href="sentimientos.html">Sentimientos</a><a class="pill" href="texto.html">Texto y temas</a><a class="pill" href="sugerencias.html">Sugerencias</a><a class="pill" href="interactivos.html">Interactivos</a><a class="pill" href="bitacora.html">Bitácora</a><a class="pill" href="conclusiones.html">Conclusiones</a>
)---"), "index.qmd")

# ---------------- metodologia ----------------
escribir(paste0(r"---(---
title: "Metodología"
---

)---", ayuda, r"---(
::: {.dos}
::: {.izq}
## Flujo de trabajo (todo en R)

1. **Ingesta**: lectura de `result.json` desde `media.zip` sin modificar el original (huella SHA-256 registrada).
2. **Limpieza**: se *marcan* (no se borran) mensajes de servicio, sin texto, solo multimedia, solo enlace, muy cortos, repetidos y ráfagas.
3. **Anonimización**: cada usuario se reemplaza por un hash con sal; los nombres no salen del equipo.
4. **Periodos**: tabla configurable (Invierno may–ago, Verano dic–feb, fuera de temporada).
5. **Sentimientos**: diccionario propio de 15 categorías (palabras y emojis), **versión 2 con contexto** (negaciones, preguntas, cortesía, publicidad) + comparación con NRC/syuzhet.
6. **Validación**: 100 mensajes etiquetados a mano; precisión, recall y F1 por sentimiento y por mejora (ver [Validación](validacion.qmd)).
7. **Texto**: tokenización, stopwords (conservando negaciones), TF-IDF, bigramas, ley de Zipf, temas con LDA.
8. **Sugerencias**: picos, hora × día, velocidad de respuesta, índice emocional, núcleo de usuarios, radar y relojes polares.
9. **Calidad**: embudo de datos, dominios, puntaje de spam y 10 chequeos de integridad.
:::

::: {.der}
## Replicabilidad

::: {.tg}
::: {.msg .cian data-t="🔁"}
<span class="who">Todo en un solo lugar</span>
Parámetros en un solo archivo (`R01_config.R`), semilla fija (42), rutas relativas, sin ventanas de selección.
:::
::: {.msg .cian data-t="📒"}
<span class="who">Todo queda anotado</span>
Cada paso queda en la [bitácora](bitacora.qmd).
:::
::: {.msg .cian data-t="➕"}
<span class="who">Fácil de ampliar</span>
Para añadir un sentimiento nuevo basta agregar filas al archivo del diccionario (`lexicon_sentimientos_v2.csv`).
:::
:::
:::
:::

## Los 15 sentimientos del diccionario propio (v2)

```{r}
#| output: asis
fl <- if (file.exists("lexicon_sentimientos_v2.csv")) "lexicon_sentimientos_v2.csv" else "lexicon_sentimientos.csv"
Lx <- readr::read_csv(fl, show_col_types = FALSE)
tbl <- Lx |> count(sentimiento, tipo) |> pivot_wider(names_from = tipo, values_from = n, values_fill = 0)
for (cc in c("palabra", "emoji")) if (!cc %in% names(tbl)) tbl[[cc]] <- 0
cat('<div class="fichas">')
for (i in seq_len(nrow(tbl)))
  cat(ficha(tbl$sentimiento[i], paste0(tbl$palabra[i], " palabras"), paste0(tbl$emoji[i], " emojis"), col_s(tbl$sentimiento[i])))
cat('</div>\n')
```

::: {.dos}
::: {.izq}
## Periodos

```{r}
P <- readr::read_csv("data/processed/periodos.csv", show_col_types = FALSE)
knitr::kable(P[, c("etiqueta", "inicio", "fin")], col.names = c("Periodo", "Inicio", "Fin"))
```
:::

::: {.der}
## Ética y privacidad

::: {.tg}
::: {.msg .rosa data-t="😉"}
<span class="who">Ética y privacidad</span>
Los usuarios se anonimizan; el sexo se **infiere por nombre** (diccionario propio), solo se usa de forma agregada y puede ser incorrecto.

No se publica ningún texto de mensajes: de los enlaces solo se muestra el dominio. 😉
:::
:::
:::
:::
)---"), "metodologia.qmd")

# ---------------- calidad ----------------
escribir(paste0(r"---(---
title: "Calidad de datos y limpieza"
---

)---", ayuda, r"---(
::: {.tg}
::: {.msg .verde data-t="🧹 ✓✓"}
<span class="who">Principio</span>
**No se elimina ninguna fila**: el ruido se *marca* con columnas verdadero/falso y se puede filtrar o revertir.
:::
:::

## Embudo de datos

```{r}
#| output: asis
e <- tb("calidad_embudo")
cat('<div class="fichas">')
cols <- c("#4cc9f0", "#4895ef", "#7b6cf6", "#c77dff", "#f72585")
for (i in seq_len(nrow(e))) cat(ficha(f(e$n[i]), e$paso[i], paste0(e$pct_del_total[i], " % del total"), cols[(i - 1) %% 5 + 1]))
cat('</div>\n')
```

```{r}
safe(gw_embudo())
```

<p class="interp">De 28 000 elementos exportados quedan 19 317 mensajes analizables; casi la mitad de ellos (9 093) expresa al menos un sentimiento.</p>

## Marcas de limpieza

```{r}
#| output: asis
dd <- L$d; msg <- sum(dd$es_msg)
m <- tibble::tibble(
  Marca = c("Eventos de servicio", "Sin texto", "Solo multimedia", "Solo enlace", "Solo emoji", "Muy corto (1-2 palabras)",
            "Repetido (3+ palabras)", "En ráfaga (5+ seguidos)", "Publicidad", "Mensajes con emojis"),
  Filas = c(sum(!dd$es_msg), sum(dd$sin_texto), sum(dd$solo_multimedia), sum(dd$solo_link), sum(dd$solo_emoji),
            sum(dd$muy_corto), sum(dd$es_repetido), sum(dd$rafaga), sum(dd$es_publicidad), sum(dd$apto_emoji)))
pal <- c("#4cc9f0", "#c77dff", "#ff9ff3", "#06d6a0", "#ffd166", "#f4a261", "#ff6b6b", "#74c0fc", "#ff5d73", "#a8e063")
cat('<div class="fichas">')
for (i in seq_len(nrow(m))) cat(ficha(f(m$Filas[i]), m$Marca[i], if (i > 1) paste0(round(m$Filas[i] / msg * 100, 1), " % de los mensajes") else "no son mensajes", pal[i]))
cat('</div>\n')
```

Un mismo mensaje puede tener varias marcas. Los anuncios largos y la publicidad se marcan y no se analizan para sentimientos.

## Enlaces: qué se comparte (solo dominios)

```{r}
safe(gw_dominios())
```

<p class="interp">Casi dos de cada tres enlaces son invitaciones de WhatsApp, seguidas de formularios; por eso la publicidad se excluyó del análisis.</p>

## Puntaje de spam y sensibilidad del umbral

::: {.tg}
::: {.msg .azul data-t="ℹ️"}
<span class="who">¿Cómo se lee?</span>
Cada mensaje recibe un puntaje (enlaces, invitaciones, muchos emojis, MAYÚSCULAS, repetido, muy largo). El filtro final solo saca invitaciones y convocatorias; la gráfica muestra cuánto coincide el puntaje con ese filtro según el umbral elegido.
:::
:::

```{r}
safe(gw_spam())
```

<p class="interp">Con umbrales bajos el puntaje marca demasiados mensajes (baja precisión); al subirlo coincide mejor con el filtro de publicidad sin perder casi nada.</p>

## Chequeos de integridad

```{r}
#| output: asis
ch <- tb("calidad_chequeos")
okn <- sum(ch$resultado == "OK")
cat('<div class="chips"><span class="chip" style="--c:#2ee6a6">', okn, ' de ', nrow(ch), ' chequeos en OK<b>✔</b></span></div>\n', sep = "")
cat('<div class="chk">')
for (i in seq_len(nrow(ch)))
  cat(sprintf('<div><span class="%s">%s</span> <b>%s</b> %s<small>%s</small></div>', ifelse(ch$resultado[i] == "OK", "ok", "no"),
              ifelse(ch$resultado[i] == "OK", "✔", "✘"), ch$chequeo[i], ch$descripcion[i], ch$detalle[i]))
cat('</div>\n')
```
)---"), "calidad.qmd")

# ---------------- validacion ----------------
escribir(paste0(r"---(---
title: "Validación del diccionario"
---

)---", ayuda, r"---(
::: {.tg}
::: {.msg .azul data-t="📝"}
<span class="who">¿Qué hicimos?</span>
Etiquetamos **a mano 100 mensajes** (60 con señal y 40 sin señal, muestra estratificada) y comparamos lo que detecta el diccionario con lo que realmente expresan. Así sabemos **qué tan confiable es**.
:::
:::

## Cómo se mide

::: {.tg}
::: {.msg .verde data-t="🎯"}
<span class="who">Precisión</span>
De lo que el diccionario marca, cuánto es correcto.
:::
::: {.msg .morada data-t="🔎"}
<span class="who">Recall</span>
De lo que realmente hay, cuánto encuentra.
:::
::: {.msg .naranja data-t="⚖️"}
<span class="who">F1</span>
Resumen de las dos.
:::
:::

## Qué aporta cada mejora

```{r}
safe(gw_ablacion())
```

<p class="interp">Cada mejora al diccionario aumenta la precisión; la curva final muestra el efecto acumulado de todas.</p>

```{r}
a <- tb("validacion_ablacion"); a[, -1] <- lapply(a[, -1], function(x) round(x, 3))
knitr::kable(a, col.names = c("Etapa", "Precisión", "Recall", "F1", "Acierto binario", "Coincidencia exacta"))
```

## F1 por sentimiento: versión 1 frente a versión 2

```{r}
safe(gw_f1())
```

<p class="interp">La versión 2 mejora el F1 en la mayoría de los sentimientos; los que tienen pocos casos son los menos estables.</p>

```{r}
p <- tb("validacion_v1_v2_por_sentimiento"); p[, c("precision", "recall", "F1")] <- lapply(p[, c("precision", "recall", "F1")], function(x) round(x, 2))
knitr::kable(p[p$version == "v2", c("sentimiento", "TP", "FP", "FN", "precision", "recall", "F1")], col.names = c("Sentimiento", "TP", "FP", "FN", "Precisión", "Recall", "F1"))
```

## Cambios hechos al diccionario (v1 → v2)

```{r}
#| output: asis
cat("<details class='det'><summary>Ver la lista de cambios</summary>\n\n")
print(knitr::kable(tb("lexicon_v2_cambios")))
cat("\n\n</details>\n")
```

## Efecto en todo el corpus

```{r}
safe(gw_comparacion())
```

<p class="interp">El mayor cambio entre versiones está en la curiosidad, que sube mucho al reconocer las preguntas como tales.</p>

## Sankey entre métodos: v1 → v2

Cada flujo muestra a qué sentimiento dominante pasó un mensaje al cambiar de versión (incluye "publicidad (excluida)" y "sin señal").

```{r}
safe(gw_sankey_v1v2())
```

<p class="interp">Muestra a dónde fue cada mensaje al pasar de la versión 1 a la 2; buena parte de los cambios son mensajes que dejan de contarse por ser publicidad o no tener señal real.</p>

## Límites de esta validación

::: {.tg}
::: {.msg .naranja data-t="⚠️"}
<span class="who">Misma muestra</span>
La evaluación es **sobre la misma muestra** con la que se ajustó el diccionario, por lo que los números son algo optimistas.
:::
::: {.msg .naranja data-t="⚠️"}
<span class="who">Un solo etiquetador</span>
Etiquetó **una sola persona**; con dos se podría medir el acuerdo (κ de Cohen).
:::
::: {.msg .naranja data-t="⚠️"}
<span class="who">Pocos casos</span>
Son 100 mensajes: las cifras por sentimiento con pocos casos son orientativas.
:::
:::
)---"), "validacion.qmd")

# ---------------- sentimientos ----------------
escribir(paste0(r"---(---
title: "Sentimientos"
---

)---", ayuda, r"---(
```{r}
#| output: asis
emo("😊", "🙏", "🤔", "😢", "😤", "😮", "😌")
```

## Visión general (diccionario propio v2, 15 categorías)

```{r}
safe(gw_general(L))
```

<p class="interp">La curiosidad es el sentimiento dominante; le siguen la gratitud, la frustración y la alegría.</p>

::: {.tg}
::: {.msg .azul data-t="🤔"}
<span class="who">Lo que más se ve</span>
La **curiosidad** (preguntas) domina; después vienen la gratitud, la frustración y la alegría.
:::
:::

## Librería NRC (8) frente a diccionario propio (15)

```{r}
safe(gw_nrc())
```

<p class="interp">La librería NRC detecta más mensajes pero con palabras ambiguas; el diccionario propio es más conservador.</p>

::: {.tg}
::: {.msg .morada data-t="📚"}
<span class="who">¿Por qué dos diccionarios?</span>
NRC es un diccionario inglés traducido: detecta muchísimo más (confianza, anticipación) pero con palabras ambiguas. El diccionario propio es más conservador y cubre 15 categorías pensadas para este contexto.
:::
:::

## Radar de emociones: Invierno, Verano y fuera de temporada

```{r}
safe(gw_radar_temp())
```

<p class="interp">El perfil emocional es muy parecido entre Invierno, Verano y fuera de temporada; cambia el volumen, no la forma.</p>

El perfil es muy parecido entre temporadas.

## Por periodo

```{r}
safe(gw_heat_periodos())
```

<p class="interp">La curiosidad domina todos los periodos (el color llega al máximo en 8 %); el resto de sentimientos se mantiene en valores bajos y estables.</p>

```{r}
safe(gw_inv_ver())
```

<p class="interp">Las tres temporadas tienen valores similares en casi todos los sentimientos.</p>

## Flujo periodo → sentimiento (Sankey)

```{r}
safe(gw_sankey_per())
```

<p class="interp">Cada periodo reparte sus mensajes con sentimiento entre las mismas categorías; el grosor del flujo indica cuántos mensajes son.</p>

## Reloj de actividad: hora del día y tono

```{r}
safe(gw_polar_hora())
```

<p class="interp">La actividad se concentra en la mañana, con un máximo alrededor de las 10 h, y es muy baja de madrugada.</p>

```{r}
safe(gw_polar_semana(L))
```

<p class="interp">La actividad es mayor entre semana y baja los fines de semana.</p>

## Reacciones

```{r}
#| output: asis
cat('<div class="reac"><span>👍</span><span>❤️</span><span>🔥</span><span>😢</span><span>🎉</span><span>🙏</span><span>👏</span></div>\n')
```

```{r}
safe(gw_reacciones())
```

<p class="interp">El pulgar arriba concentra casi todas las reacciones, por eso la categoría "seguridad" domina.</p>

::: {.tg}
::: {.msg .verde data-t="👍 ✓✓"}
<span class="who">Reaccionar es aprobar</span>
La reacción más usada es el pulgar arriba (aprobación): reaccionar en este grupo es sobre todo *aprobar*.
:::
:::

## Hombres y mujeres (sexo inferido por nombre)

```{r}
safe(gw_sexo())
```

<p class="interp">La mayoría de las diferencias entre hombres y mujeres son pequeñas; solo las marcadas con asterisco son significativas.</p>

::: {.tg}
::: {.msg .naranja data-t="⚠️"}
<span class="who">Con cautela</span>
Gran parte de los mensajes quedó como *indeterminado* porque no se pudo inferir el sexo; los resultados se leen con cautela.
:::
:::
)---"), "sentimientos.qmd")

# ---------------- texto y temas ----------------
escribir(paste0(r"---(---
title: "Texto y temas"
---

)---", ayuda, r"---(
## Palabras más frecuentes

```{r}
safe(gw_top_palabras())
```

<p class="interp">Las palabras más frecuentes giran en torno a materias, grupos y trámites de inscripción.</p>

## Bigramas

```{r}
safe(gw_bigramas())
```

<p class="interp">Los pares de palabras más repetidos confirman los temas del chat: materias, cupos, clases y agradecimientos.</p>

```{r}
safe(gw_red_bigramas())
```

<p class="interp">Las palabras más conectadas forman grupos temáticos alrededor de materias y trámites.</p>

## Palabras distintivas (TF-IDF)

```{r}
safe(gw_tfidf_periodos())
```

<p class="interp">Las palabras distintivas de cada periodo reflejan lo que se discutía en esa época (inscripciones, sorteos, cupos).</p>

```{r}
safe(gw_tfidf_temp())
```

<p class="interp">Cada temporada tiene su propio vocabulario característico.</p>

## Ley de Zipf

```{r}
safe(gw_zipf())
```

<p class="interp">Pocas palabras se usan muchísimo y la gran mayoría casi nunca, como en cualquier texto natural.</p>

## Lingüística superficial

```{r}
li <- tb("linguistica_superficial")
knitr::kable(li, digits = 3)
```

## Temas del chat (LDA)

::: {.tg}
::: {.msg .azul data-t="🖼️"}
<span class="who">Gráficas de temas</span>
Estas gráficas vienen del modelo de temas (LDA) y se muestran como imagen.
:::
:::

```{r}
#| output: asis
for (im in c("g_temas_lda", "g_tema_periodo", "g_tema_sentimiento", "g_nubes_sentimientos"))
  if (file.exists(sprintf("outputs/graficos/%s.png", im))) cat(sprintf("![](outputs/graficos/%s.png)\n\n", im))
```
)---"), "texto.qmd")

# ---------------- sugerencias ----------------
escribir(paste0(r"---(---
title: "Sugerencias"
subtitle: "Análisis adicionales que se pueden usar para tomar decisiones"
---

)---", ayuda, r"---(
## Días pico (preocupación, frustración y confusión)

```{r}
safe(gw_picos_dia(L, "preocupación", "#f4a261"))
```

<p class="interp">Los días con más preocupación coinciden con fechas de inscripción y cupos.</p>

```{r}
safe(gw_picos_dia(L, "frustración", "#ff6b6b"))
```

<p class="interp">La frustración se dispara en días puntuales, casi siempre por cupos agotados.</p>

```{r}
safe(gw_picos_dia(L, "confusión", "#c77dff"))
```

<p class="interp">La confusión aparece en pocos mensajes y días sueltos; se lee con cautela.</p>

::: {.tg}
::: {.msg .rojo data-t="📅"}
<span class="who">Picos</span>
Los picos de frustración y preocupación coinciden con momentos de inscripción, cupos y pagos.
:::
:::

## Cuándo se escribe y cómo cambian los sentimientos

```{r}
safe(gw_heat_hora_dia(L))
```

<p class="interp">La actividad es mayor en las mañanas de los días de semana.</p>

```{r}
safe(gw_sent_hora(L))
```

<p class="interp">Los sentimientos mantienen un perfil parecido a lo largo del día; la curiosidad es la más alta en casi todas las horas.</p>

## ¿Quién recibe respuesta y qué tan rápido?

```{r}
safe(gw_respuesta())
```

<p class="interp">Solo una cuarta parte de los mensajes recibe respuesta, y la mediana de espera es de unos 6 minutos.</p>

## Índice emocional

```{r}
safe(gw_indice_mensual())
```

<p class="interp">El índice se mueve cerca de cero: hay un equilibrio entre emociones positivas y negativas a lo largo del tiempo.</p>

```{r}
safe(gw_pos_neg())
```

<p class="interp">Las emociones positivas y negativas suben y bajan juntas, según el volumen de mensajes de cada mes.</p>

```{r}
safe(gw_indice_periodos())
```

<p class="interp">Verano 2025-26 es el periodo más negativo; fuera de temporada e Invierno 2024 son los más positivos.</p>

::: {.tg}
::: {.msg .verde data-t="⚖️"}
<span class="who">Equilibrio</span>
Con el diccionario v2 el índice emocional general queda cerca de cero: hay un equilibrio entre emociones positivas y negativas, y el verano 2025-26 es el periodo más negativo.
:::
:::

## Núcleo activo frente al resto

```{r}
safe(gw_nucleo())
```

<p class="interp">Los usuarios más activos expresan sentimientos con una frecuencia parecida a la del resto.</p>

```{r}
safe(gw_top_usuarios(L))
```

<p class="interp">Pocos usuarios concentran la expresión de sentimientos; los puntos grandes expresan más variedad.</p>
)---"), "sugerencias.qmd")

# ---------------- interactivos ----------------
escribir(paste0(r"---(---
title: "Visualizaciones interactivas"
subtitle: "Pasa el mouse, haz clic en la leyenda, acércate con la rueda del mouse"
---

)---", ayuda, r"---(
## Radar de emociones por temporada

```{r}
safe(gw_radar_temp())
```

<p class="interp">El perfil emocional es muy parecido entre Invierno, Verano y fuera de temporada; cambia el volumen, no la forma.</p>

## Radar por periodo

```{r}
safe(gw_radar_per())
```

<p class="interp">Al aislar un periodo en la leyenda se ve que todos comparten una forma similar.</p>

## Animación: las emociones periodo a periodo

Pulsa **▶ Reproducir** o mueve la barra.

```{r}
safe(gw_animacion())
```

<p class="interp">Al avanzar los periodos las barras cambian poco: la estructura emocional del grupo es estable.</p>

## Reloj de actividad

```{r}
safe(gw_polar_hora())
```

<p class="interp">La actividad se concentra en la mañana, con un máximo alrededor de las 10 h, y es muy baja de madrugada.</p>

## Evolución mensual de cada sentimiento

```{r}
safe(gw_series(L))
```

<p class="interp">Los sentimientos se mantienen estables mes a mes, con picos en los meses de inscripción.</p>

## Mapa de calor por periodo

```{r}
safe(gw_heat_periodos())
```

<p class="interp">La curiosidad domina todos los periodos (el color llega al máximo en 8 %); el resto de sentimientos se mantiene en valores bajos y estables.</p>

## Flujo periodo → sentimiento

```{r}
safe(gw_sankey_per())
```

<p class="interp">Cada periodo reparte sus mensajes con sentimiento entre las mismas categorías; el grosor del flujo indica cuántos mensajes son.</p>

## Cambio v1 → v2

```{r}
safe(gw_sankey_v1v2())
```

<p class="interp">Muestra a dónde fue cada mensaje al pasar de la versión 1 a la 2; buena parte de los cambios son mensajes que dejan de contarse por ser publicidad o no tener señal real.</p>
)---"), "interactivos.qmd")

# ---------------- bitacora ----------------
escribir(r"---(---
title: "Bitácora"
---

```{r}
#| output: asis
cand <- list.files(".", pattern = "bitacora.*\\.csv$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
cand <- cand[!grepl("docs/", cand)]
if (!length(cand)) {
  cat("No se encontró el archivo de la bitácora (bitacora.csv).")
} else {
  B <- readr::read_csv(cand[1], show_col_types = FALSE)
  cat('<div style="overflow-x:auto">\n\n')
  print(knitr::kable(B))
  cat('\n\n</div>\n')
}
```
)---", "bitacora.qmd")

# ---------------- conclusiones ----------------
escribir(r"---(---
title: "Conclusiones y limitaciones"
---

## Conclusiones

::: {.tg}
::: {.msg .verde data-t="10:51 ✓✓"}
<span class="who">Temas</span>
El chat gira en torno a **materias, paralelos, horarios, inscripción y cupos**, con picos estacionales en Invierno y Verano.
:::
::: {.msg .verde data-t="10:52 ✓✓"}
<span class="who">Dudas y gratitud</span>
**Uno de cada tres mensajes es una pregunta** (curiosidad): el grupo funciona sobre todo como espacio para resolver dudas. Le sigue la **gratitud**.
:::
::: {.msg .verde data-t="10:53 ✓✓"}
<span class="who">Frustración y preocupación</span>
Se asocian a inscripción, cupos y pagos.
:::
::: {.msg .verde data-t="10:54 ✓✓"}
<span class="who">Publicidad</span>
Las invitaciones a otros grupos y formularios son una parte importante de los enlaces y se excluyeron del análisis.
:::
::: {.msg .verde data-t="10:55 ✓✓"}
<span class="who">Perfil emocional</span>
Es **similar** entre Invierno, Verano y fuera de temporada.
:::
::: {.msg .verde data-t="10:56 ✓✓"}
<span class="who">Diccionario con contexto</span>
Mejorarlo con **negaciones, preguntas, cortesía y publicidad** subió de forma clara la precisión y el recall frente a la versión inicial (ver [Validación](validacion.qmd)).
:::
:::

## Limitaciones

::: {.tg}
::: {.msg .naranja .out data-t="⚠️"}
<span class="who">Palabras y emojis</span>
El diccionario detecta palabras y emojis: no entiende ironía ni contexto profundo, y solo una parte de los mensajes (alrededor de un tercio) tiene señal emocional.
:::
::: {.msg .naranja .out data-t="⚠️"}
<span class="who">Validación pequeña</span>
Usa **100 mensajes**, **un solo etiquetador** y la misma muestra con la que se ajustó el diccionario: las cifras son algo optimistas.
:::
::: {.msg .naranja .out data-t="⚠️"}
<span class="who">Sexo inferido</span>
Se **infiere por nombre**; una parte importante de los mensajes quedó como indeterminada y la inferencia puede fallar.
:::
::: {.msg .naranja .out data-t="⚠️"}
<span class="who">Un solo grupo</span>
Se analiza un solo grupo de Telegram: los resultados no se generalizan a otros cursos.
:::
::: {.msg .naranja .out data-t="⚠️"}
<span class="who">Picos diarios</span>
Se basan en pocos mensajes y se interpretan con cautela.
:::
:::
)---", "conclusiones.qmd")

# ---------------- render ----------------
cat("Archivos escritos. Renderizando el sitio...\n")
ok <- tryCatch({ quarto::quarto_render(); TRUE },
               error = function(e) { message("ERROR al renderizar: ", conditionMessage(e)); FALSE })
if (ok) {
  file.create("docs/.nojekyll")
  if (!file.exists(".gitignore"))
    escribir(c("# datos privados: NO subir", "data/", "*.zip", "result.json", "*.rds", ".Rproj.user/", ".Rhistory", ".quarto/", ".RData"), ".gitignore")
  registrar("10. Sitio Quarto", "Rediseno final del sitio: menu lateral, burbujas, graficas interactivas",
            0, 0, "docs/")
  cat("\nListo. Abre docs/index.html para verlo.\n")
}
