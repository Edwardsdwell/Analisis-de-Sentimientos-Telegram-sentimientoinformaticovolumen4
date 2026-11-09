library(tidyverse)
source("R01_config.R")

items <- tribble(
  ~etapa,                    ~accion,                                                                   ~ruta,                                        ~tipo,
  "1. Ingesta",              "Lectura de result.json desde media.zip (28 000 elementos); SHA-256 del zip", "data/processed/base.rds",                  "rds",
  "2. Limpieza",             "Marcas de ruido, repetidos, rafagas y hash de usuario; sin borrar filas",    "data/processed/limpio.rds",                "rds",
  "3. Periodos",             "Invierno (may-ago), Verano (dic-feb) y Fuera de temporada",                  "data/processed/listo.rds",                 "rds",
  "4. Lexico",               "Diccionario propio de 15 sentimientos (palabras y emojis)",                  "lexicon_sentimientos.csv",                 "csv",
  "5. Sentimientos",         "Conteo de sentimientos por mensaje con el diccionario propio",               "data/processed/listo_sent.rds",            "rds",
  "6. Periodos x sentimiento","Tabla de sentimientos por periodo",                                         "outputs/tablas/sentimientos_por_periodo.csv","csv",
  "6. Sankey",               "Sankey periodo a sentimiento (estatico e interactivo)",                      "outputs/graficos/g_sankey_periodos.png",   "png",
  "7. NRC vs propio",        "Comparacion syuzhet/NRC (8) contra diccionario propio (15)",                 "outputs/tablas/comparacion_nrc_propio.csv","csv",
  "7. Reacciones",           "Sentimiento de las reacciones (emojis)",                                     "outputs/tablas/reacciones_por_sentimiento.csv","csv",
  "8. Genero",               "Sexo inferido por nombre (solo hash y sexo) y sentimientos por sexo",        "data/processed/usuarios_sexo.csv",         "csv",
  "9. Texto",                "Tokenizacion, stopwords, frecuencias, TF-IDF, bigramas, Zipf",               "data/processed/tokens.rds",                "rds",
  "10. Temas",               "LDA (K=6) y tema dominante por mensaje",                                     "data/processed/temas.rds",                 "rds",
  "11. Picos y respuesta",   "Dias pico de preocupacion y velocidad de respuesta por sentimiento",         "outputs/tablas/respuesta_por_sentimiento.csv","csv",
  "12. Indice y nucleo",     "Indice emocional mensual y nucleo de usuarios vs resto",                     "outputs/tablas/indice_emocional_mensual.csv","csv",
  "13. Validacion",          "Muestra de 100 mensajes para validacion manual del diccionario",             "validacion/muestra_100.csv",               "csv2"
)

rec <- pmap_dfr(items, function(etapa, accion, ruta, tipo) {
  if (!file.exists(ruta)) return(NULL)
  n <- tryCatch(switch(tipo,
                       rds  = { x <- readRDS(ruta); if (is.data.frame(x)) nrow(x) else if (is.list(x) && !is.null(x$d)) nrow(x$d) else NA_real_ },
                       csv  = nrow(read_csv(ruta, show_col_types = FALSE)),
                       csv2 = nrow(read_csv2(ruta, show_col_types = FALSE)),
                       NA_real_), error = function(e) NA_real_)
  tibble(paso = NA_real_, fecha = format(file.info(ruta)$mtime, "%Y-%m-%d %H:%M"), etapa = etapa,
         accion = accion, filas_antes = as.numeric(n), filas_despues = as.numeric(n),
         salida = basename(ruta), estado = "OK", origen = "reconstruida")
})

for (cc in c("estado", "origen")) if (!cc %in% names(bitacora)) bitacora[[cc]] <- rep(NA_character_, nrow(bitacora))
bitacora$estado[is.na(bitacora$estado)] <- "OK"
act   <- bitacora %>% filter(is.na(origen) | origen != "reconstruida")
nueva <- bind_rows(act, rec) %>% arrange(fecha) %>% mutate(paso = row_number())
write_csv(nueva, RUTA_BIT)
print(nueva %>% select(paso, fecha, etapa, filas_despues, origen), n = 40)
cat("Bitacora guardada con", nrow(nueva), "filas.\n")