RUTA_ZIP         <- "data/raw/media.zip"
RUTA_JSON_DENTRO <- "media/result.json"
DIR_PROC <- "data/processed"; DIR_GRAF <- "outputs/graficos"; DIR_TAB <- "outputs/tablas"
SEMILLA  <- 42
SAL      <- "cambia_esta_frase_y_guardala"   # la misma que ya usaste; no la cambies ahora
set.seed(SEMILLA)
for (d in c(DIR_PROC, DIR_GRAF, DIR_TAB, "bitacora")) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Bitacora automatica: una fila por paso real ejecutado (se ACUMULA entre ejecuciones)
RUTA_BIT <- "bitacora/bitacora.csv"
bitacora <- if (file.exists(RUTA_BIT)) {
  readr::read_csv(RUTA_BIT, show_col_types = FALSE,
                  col_types = readr::cols(fecha = "c", etapa = "c", accion = "c", salida = "c",
                                          estado = "c", origen = "c", .default = "d"))
} else tibble::tibble()

registrar <- function(etapa, accion, filas_antes, filas_despues, salida = "", estado = "OK", origen = "ejecucion") {
  bitacora <<- dplyr::bind_rows(bitacora, tibble::tibble(
    paso = nrow(bitacora) + 1, fecha = format(Sys.time(), "%Y-%m-%d %H:%M"),
    etapa = etapa, accion = accion, filas_antes = as.numeric(filas_antes),
    filas_despues = as.numeric(filas_despues), salida = salida, estado = estado, origen = origen))
  readr::write_csv(bitacora, RUTA_BIT)
}
cat("Config cargada. Semilla:", SEMILLA, "\n")