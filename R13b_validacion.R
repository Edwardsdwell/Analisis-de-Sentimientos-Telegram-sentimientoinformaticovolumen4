library(tidyverse); library(stringi)
source("R01_config.R")
B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
cols <- paste0("s_", sent)
RUTA <- "validacion/muestra_100.csv"
norm <- function(x) str_squish(stri_trans_general(str_to_lower(coalesce(x, "")), "Latin-ASCII"))
base <- d %>% filter(usable, n_palabras >= 3)

if (!file.exists(RUTA)) {
  dir.create("validacion", showWarnings = FALSE)
  set.seed(SEMILLA)
  con <- base %>% filter(n_senal > 0) %>% slice_sample(n = 60)
  sin <- base %>% filter(n_senal == 0) %>% slice_sample(n = 40)
  m <- bind_rows(con %>% mutate(grupo = "con_senal"), sin %>% mutate(grupo = "sin_senal")) %>% slice_sample(prop = 1)
  m$detectados <- apply(m[, cols], 1, function(r) paste(sent[r > 0], collapse = "; "))
  out <- m %>% transmute(n = row_number(), id, texto = substr(texto_final, 1, 300), grupo, detectados, etiqueta_manual = "")
  write_excel_csv2(out %>% select(-grupo, -detectados) %>% mutate(oculto = ""), "validacion/_borrar.csv")  # no usar
  file.remove("validacion/_borrar.csv")
  write_excel_csv2(out, RUTA)
  cat("Creado", RUTA, "\nAbrelo en Excel y llena la columna 'etiqueta_manual' con UNO de los 15 sentimientos,",
      "o 'ninguno' si el mensaje no expresa ninguno.\nLuego guarda (como CSV) y vuelve a pulsar Source en este script.\n")
} else {
  v <- read_csv2(RUTA, show_col_types = FALSE) %>%
    mutate(man = norm(etiqueta_manual))
  if (all(v$man == "")) stop("La columna etiqueta_manual esta vacia. Llenala y vuelve a correr.")
  v$ninguno <- v$man %in% c("", "ninguno", "ninguna", "neutro")
  v$acierto <- map2_lgl(v$man, v$detectados, ~ norm(.y) != "" && .x %in% norm(str_split(.y, ";")[[1]]))
  c1 <- v %>% filter(grupo == "con_senal"); c0 <- v %>% filter(grupo == "sin_senal")
  prec <- mean(c1$acierto); a <- mean(!c1$ninguno); b <- mean(!c0$ninguno)
  Ncon <- sum(base$n_senal > 0); Nsin <- sum(base$n_senal == 0)
  rec <- (Ncon * prec) / (Ncon * a + Nsin * b); f1 <- 2 * prec * rec / (prec + rec)
  cat("Precision (cuando detecta, acierta):", round(prec * 100, 1), "%\n")
  cat("Falsos negativos entre mensajes sin senal:", round(b * 100, 1), "%\n")
  cat("Cobertura estimada (recall):", round(rec * 100, 1), "% | F1 estimado:", round(f1 * 100, 1), "%\n")
  err <- c1 %>% filter(!acierto) %>% select(texto, detectados, etiqueta_manual)
  cat("\nErrores para corregir el diccionario:\n"); print(err, n = 30, width = 200)
  write_csv(tibble(precision = prec, falsos_neg_sin_senal = b, recall_est = rec, f1_est = f1, n = nrow(v)),
            file.path(DIR_TAB, "validacion_diccionario.csv"))
  registrar("13. Validacion", "Validacion manual de 100 mensajes del diccionario", nrow(v), nrow(v), "validacion_diccionario.csv")
}