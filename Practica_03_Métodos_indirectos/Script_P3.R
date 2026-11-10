# ============================================================
# PRÁCTICA 3: OBTENCIÓN DE EDAD POR MÉTODOS INDIRECTOS
# Petersen + Bhattacharya  |  Script único para Posit Cloud
# ============================================================
# INSTRUCCIONES:
# 1) Sube el archivo "P2 TALLAS TILAPIAS Petersen & Bhattacharya.xls"
#    a la carpeta del proyecto (panel Files -> Upload).
# 2) Verifica el nombre del archivo en el bloque 1.
# 3) Ejecuta TODO el script con Ctrl + Shift + Enter.
# 4) Revisa el panel Plots (gráficos) y Files (resultados .csv).
# ============================================================

# ------------------------------------------------------------
# 0. PAQUETES
# ------------------------------------------------------------
paquetes <- c("readxl", "tidyverse")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# ------------------------------------------------------------
# 1. PARÁMETROS EDITABLES POR EL ALUMNO
# ------------------------------------------------------------
archivo     <- "P2 TALLAS TILAPIAS Petersen & Bhattacharya.xls"
hoja        <- "Hoja1"
col_fecha   <- 1     # columna de Fecha
col_colecta <- 2     # columna de Colecta
col_talla   <- 3     # columna de Talla (mm)
h           <- 10    # intervalo de clase en mm

# Deja NULL para que el script use la fecha con MÁS datos (recomendado para
# empezar). Luego podrás fijar la que elijas, p. ej. "1999-04-23".
fecha_elegida <- NULL

# Rangos (en mm) donde se observan rectas con pendiente negativa.
# Ajusta estos intervalos mirando el gráfico "Bhattacharya por fecha".
rangos <- list(
  c(40, 55),
  c(58, 78),
  c(80, 105),
  c(105, 130),
  c(130, 160)
)

# ------------------------------------------------------------
# 2. LEER DATOS
# ------------------------------------------------------------
leer_datos <- function(archivo, hoja, col_fecha, col_colecta, col_talla) {
  if (grepl("\\.xls$|\\.xlsx$", archivo, ignore.case = TRUE)) {
    df <- readxl::read_excel(archivo, sheet = hoja)
  } else if (grepl("\\.csv$", archivo, ignore.case = TRUE)) {
    df <- read.csv(archivo, stringsAsFactors = FALSE)
  } else stop("Formato no soportado: usa .xls, .xlsx o .csv")
  
  df <- df[, c(col_fecha, col_colecta, col_talla)]
  colnames(df) <- c("Fecha", "Colecta", "Talla")
  df %>%
    mutate(
      Fecha   = as.character(Fecha),
      Colecta = as.numeric(Colecta),
      Talla   = as.numeric(Talla)
    ) %>%
    filter(!is.na(Talla)) %>%
    tidyr::fill(Fecha, Colecta, .direction = "down")
}

datos <- leer_datos(archivo, hoja, col_fecha, col_colecta, col_talla)
cat("\n>>> Registros cargados:", nrow(datos), "\n")

# ------------------------------------------------------------
# 3. NORMALIZAR FECHAS (seriales Excel -> YYYY-MM-DD)
# ------------------------------------------------------------
normalizar_fecha <- function(x) {
  x_chr <- trimws(as.character(x))
  num   <- suppressWarnings(as.numeric(x_chr))
  es_serial <- !is.na(num) & num > 20000 & num < 60000
  x_chr[es_serial] <- format(as.Date(num[es_serial], origin = "1899-12-30"),
                             "%Y-%m-%d")
  x_chr[x_chr == "Sep.-98"] <- "1998-09-15"   # etiqueta textual
  x_chr
}

datos <- datos %>% mutate(Fecha = normalizar_fecha(Fecha))

cat("\n>>> Registros por fecha:\n")
print(sort(table(datos$Fecha), decreasing = TRUE))

# ------------------------------------------------------------
# 4. ELEGIR FECHA PARA BHATTACHARYA
# ------------------------------------------------------------
if (is.null(fecha_elegida) || !(fecha_elegida %in% datos$Fecha)) {
  fecha_elegida <- names(sort(table(datos$Fecha), decreasing = TRUE))[1]
  cat("\n>>> Fecha elegida automáticamente:", fecha_elegida, "\n")
} else {
  cat("\n>>> Fecha elegida por el usuario:", fecha_elegida, "\n")
}

# ------------------------------------------------------------
# 5. FUNCIÓN: TABLA DE FRECUENCIAS
# ------------------------------------------------------------
crear_tabla_frec <- function(df, h = 10) {
  if (nrow(df) < 2) return(NULL)
  min_talla <- floor(min(df$Talla, na.rm = TRUE) / h) * h
  max_talla <- ceiling(max(df$Talla, na.rm = TRUE) / h) * h
  if (!is.finite(min_talla) || !is.finite(max_talla)) return(NULL)
  breaks <- seq(min_talla, max_talla, by = h)
  if (length(breaks) < 2) return(NULL)
  clases <- cut(df$Talla, breaks = breaks, right = FALSE, include.lowest = TRUE)
  data.frame(
    Clase      = levels(clases),
    Frecuencia = as.numeric(table(clases))
  ) %>%
    mutate(
      Pto_medio = breaks[-length(breaks)] + h/2,
      Log10_f   = ifelse(Frecuencia > 0, log10(Frecuencia), NA),
      Dif_log_f = c(diff(Log10_f), NA)
    )
}

# ------------------------------------------------------------
# 6. MÉTODO DE PETERSEN: HISTOGRAMAS POR FECHA
# ------------------------------------------------------------
petersen_plot <- datos %>%
  ggplot(aes(x = Talla)) +
  geom_histogram(binwidth = h, fill = "steelblue", color = "black") +
  facet_wrap(~ Fecha, scales = "free_y") +
  labs(x = "Talla (mm)", y = "Frecuencia",
       title = "Petersen: distribución de frecuencias de tallas por fecha") +
  theme_minimal(base_size = 11)
print(petersen_plot)
ggsave("01_petersen.png", petersen_plot, width = 12, height = 8, dpi = 150)

# ------------------------------------------------------------
# 7. BHATTACHARYA POR TODAS LAS FECHAS (para elegir el mes)
# ------------------------------------------------------------
bhatta_todas <- datos %>%
  group_by(Fecha) %>%
  group_modify(function(.x, .y) {
    t <- crear_tabla_frec(.x, h = h)
    if (is.null(t)) return(data.frame())
    t
  }) %>%
  ungroup() %>%
  filter(is.finite(Dif_log_f))

if (nrow(bhatta_todas) > 0) {
  g_todas <- ggplot(bhatta_todas, aes(x = Pto_medio, y = Dif_log_f)) +
    geom_point(size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~ Fecha, scales = "free") +
    labs(x = "Punto medio (mm)", y = expression(Delta ~ log[10] ~ f),
         title = "Bhattacharya por fecha: busca rectas con pendiente negativa") +
    theme_minimal(base_size = 10)
  print(g_todas)
  ggsave("02_bhattacharya_por_fecha.png", g_todas,
         width = 14, height = 9, dpi = 150)
}

# ------------------------------------------------------------
# 8. BHATTACHARYA PARA LA FECHA ELEGIDA
# ------------------------------------------------------------
datos_mes <- datos %>% filter(Fecha == fecha_elegida)
cat("\n>>> Registros en", fecha_elegida, ":", nrow(datos_mes), "\n")

tabla <- crear_tabla_frec(datos_mes, h = h)
if (is.null(tabla)) stop("No hay datos suficientes en la fecha elegida.")

cat("\n>>> Tabla de frecuencias (fecha elegida):\n")
print(tabla)

# Gráfico base de Bhattacharya
bhatta_base <- tabla %>%
  filter(is.finite(Dif_log_f)) %>%
  ggplot(aes(x = Pto_medio, y = Dif_log_f)) +
  geom_point(size = 2.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Punto medio (mm)",
       y = expression(Delta ~ log[10] ~ f),
       title = paste("Bhattacharya -", fecha_elegida)) +
  theme_minimal(base_size = 12)
print(bhatta_base)

# ------------------------------------------------------------
# 9. AJUSTE DE RECTAS (componentes gaussianos)
# ------------------------------------------------------------
ajustar_bhattacharya <- function(tabla, rangos, h) {
  resultados <- data.frame()
  for (i in seq_along(rangos)) {
    r   <- rangos[[i]]
    sub <- tabla %>%
      filter(Pto_medio >= r[1], Pto_medio <= r[2], is.finite(Dif_log_f))
    if (nrow(sub) >= 2) {
      modelo <- tryCatch(lm(Dif_log_f ~ Pto_medio, data = sub),
                         error = function(e) NULL)
      if (is.null(modelo)) next
      intercepto <- coef(modelo)[1]
      pendiente  <- coef(modelo)[2]
      if (is.na(pendiente) || pendiente >= 0) next   # solo pendientes negativas
      lambda <- -intercepto / pendiente
      media  <- lambda + h/2
      resultados <- rbind(resultados, data.frame(
        Componente     = i,
        Rango_mm       = paste(r, collapse = "-"),
        Intercepto     = round(intercepto, 4),
        Pendiente      = round(pendiente, 4),
        Lambda         = round(lambda, 2),
        Media_estimada = round(media, 2),
        R2             = round(summary(modelo)$r.squared, 4),
        n_puntos       = nrow(sub)
      ))
    }
  }
  return(resultados)
}

resultados_bhatta <- ajustar_bhattacharya(tabla, rangos, h)
cat("\n>>> Tallas medias por componente gaussiano:\n")
print(resultados_bhatta)

# ------------------------------------------------------------
# 10. GRÁFICO FINAL CON RECTAS AJUSTADAS
# ------------------------------------------------------------
bhatta_final <- bhatta_base
for (i in seq_along(rangos)) {
  r   <- rangos[[i]]
  sub <- tabla %>%
    filter(Pto_medio >= r[1], Pto_medio <= r[2], is.finite(Dif_log_f))
  if (nrow(sub) >= 2) {
    modelo <- tryCatch(lm(Dif_log_f ~ Pto_medio, data = sub),
                       error = function(e) NULL)
    if (is.null(modelo)) next
    x_seq  <- seq(r[1], r[2], length.out = 100)
    y_pred <- predict(modelo, newdata = data.frame(Pto_medio = x_seq))
    bhatta_final <- bhatta_final +
      geom_line(data = data.frame(x = x_seq, y = y_pred),
                aes(x = x, y = y), color = "blue", linewidth = 1)
  }
}
print(bhatta_final)
ggsave("03_bhattacharya_ajustado.png", bhatta_final,
       width = 11, height = 7, dpi = 150)

# ------------------------------------------------------------
# 11. EXPORTAR RESULTADOS
# ------------------------------------------------------------
write.csv(tabla,             "04_tabla_frecuencias.csv",      row.names = FALSE)
write.csv(resultados_bhatta, "05_tallas_por_grupo_edad.csv",  row.names = FALSE)
write.csv(bhatta_todas,      "06_bhattacharya_todas_fechas.csv", row.names = FALSE)

cat("\n============================================================\n")
cat("ARCHIVOS GENERADOS:\n")
cat(" - 01_petersen.png                    (histogramas por fecha)\n")
cat(" - 02_bhattacharya_por_fecha.png      (para elegir el mes)\n")
cat(" - 03_bhattacharya_ajustado.png       (rectas y componentes)\n")
cat(" - 04_tabla_frecuencias.csv\n")
cat(" - 05_tallas_por_grupo_edad.csv       <-- RESULTADO PRINCIPAL\n")
cat(" - 06_bhattacharya_todas_fechas.csv\n")
cat("============================================================\n")
