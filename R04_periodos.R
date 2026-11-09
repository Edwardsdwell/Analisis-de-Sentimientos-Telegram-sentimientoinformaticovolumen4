library(tidyverse); library(lubridate)
source("R01_config.R")

ANIO_INI <- 2022; ANIO_FIN <- 2026      # cambia aqui para otros anios

P <- bind_rows(
  tibble(anio = ANIO_INI:ANIO_FIN, tipo = "Invierno",
         inicio = ymd(paste0(anio, "-05-01")), fin = ymd(paste0(anio, "-08-31")),
         etiqueta = paste("Invierno", anio)),
  tibble(anio = ANIO_INI:ANIO_FIN, tipo = "Verano",
         inicio = ymd(paste0(anio, "-12-01")), fin = ymd(paste0(anio + 1, "-03-01")) - days(1),
         etiqueta = paste0("Verano ", anio, "-", substr(anio + 1, 3, 4)))
) %>% arrange(inicio)
write_csv(P, file.path(DIR_PROC, "periodos.csv"))
print(P %>% select(etiqueta, inicio, fin))

B <- readRDS(file.path(DIR_PROC, "limpio.rds")); d <- B$d

fd  <- as_date(d$fecha)
idx <- vapply(fd, function(x) { i <- which(x >= P$inicio & x <= P$fin)
if (length(i)) i[1] else NA_integer_ }, integer(1))
d$periodo      <- coalesce(P$etiqueta[idx], "Fuera de temporada")
d$periodo_tipo <- coalesce(P$tipo[idx],     "Fuera de temporada")
d$anio         <- year(fd)
d$mes          <- floor_date(fd, "month")

cat("\n--- Mensajes por tipo de periodo ---\n")
print(d %>% filter(es_msg) %>% count(periodo_tipo))
cat("\n--- Mensajes por periodo ---\n")
print(d %>% filter(es_msg) %>% count(periodo) %>% arrange(desc(n)), n = 20)

cat("\nUsuarios distintos (todos):", n_distinct(d$usuario, na.rm = TRUE), "\n")
cat("Usuarios que escribieron:", n_distinct(d$usuario[d$es_msg], na.rm = TRUE), "\n")

stopifnot(!any(is.na(d$periodo)), nrow(d) == 28000)
registrar("3. Periodos", "Asignacion de Invierno (may-ago), Verano (dic-feb) y Fuera de temporada",
          nrow(d), nrow(d), "listo.rds, periodos.csv")
saveRDS(list(d = d, reac = B$reac, hash = B$hash), file.path(DIR_PROC, "listo.rds"))
write_csv(d %>% select(-texto, -texto_final), file.path(DIR_PROC, "chat_procesado.csv"))
cat("OK: listo.rds y chat_procesado.csv guardados.\n")