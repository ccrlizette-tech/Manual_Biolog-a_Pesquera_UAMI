# ==============================================================================
# PRÁCTICA 9: SELECTIVIDAD DE ARTES DE PESCA (ATARRAYAS)
# Aberturas: 2", 2.5", 2¾", 3", 3.5", 4"
# Entorno: Posit Cloud
# ==============================================================================

# 1. PAQUETES
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl))   install.packages("readxl")
if(!require(scales))   install.packages("scales")
library(tidyverse); library(readxl); library(scales)

# 2. CARPETA DE RESULTADOS
if(!dir.exists("resultados_practica9")) dir.create("resultados_practica9")

# ==============================================================================
# 3. CARGAR DATOS
# ==============================================================================
# Se saltan las 2 primeras filas (título "Atarraya" + encabezados) y se
# toman las 6 columnas con datos de tallas (A-F)
datos_raw <- read_excel("Selectividad Atarraya 2 a 4 DATOS (1).xls",
                        sheet = "Datos", skip = 2, col_names = FALSE)
datos_raw <- datos_raw[, 1:6]
names(datos_raw) <- c("Malla_2","Malla_2.5","Malla_2.75",
                      "Malla_3","Malla_3.5","Malla_4")

# ==============================================================================
# 4. LIMPIEZA Y FORMATO LARGO
# ==============================================================================
datos_long <- datos_raw %>%
  mutate(across(everything(), ~suppressWarnings(as.numeric(.x)))) %>%
  pivot_longer(everything(), names_to = "Malla", values_to = "Longitud",
               values_drop_na = TRUE) %>%
  filter(!is.na(Longitud)) %>%
  mutate(Malla = factor(Malla,
                        levels = c("Malla_2","Malla_2.5","Malla_2.75",
                                   "Malla_3","Malla_3.5","Malla_4"),
                        labels = c('2"','2.5"','2.75"','3"','3.5"','4"')))

cat("\n=== NÚMERO DE INDIVIDUOS POR MALLA ===\n")
print(table(datos_long$Malla))

# ==============================================================================
# 5. RESUMEN POR MALLA
# ==============================================================================
resumen <- datos_long %>%
  group_by(Malla) %>%
  summarise(
    n         = n(),
    Long_min  = min(Longitud),
    Long_max  = max(Longitud),
    Long_media= round(mean(Longitud), 2),
    Long_mediana = round(median(Longitud), 2),
    .groups = "drop"
  )
write.csv(resumen, "resultados_practica9/01_resumen_por_malla.csv",
          row.names = FALSE)
print(resumen)

# ==============================================================================
# 6. FUNCIÓN PARA CALCULAR LA CDF EMPÍRICA (por clases)
# ==============================================================================
calcular_cdf <- function(data_mesh, bin_size = 0.5) {
  if(nrow(data_mesh) == 0) return(tibble())
  
  min_len <- floor(min(data_mesh$Longitud))
  max_len <- ceiling(max(data_mesh$Longitud))
  breaks  <- seq(min_len, max_len + bin_size, by = bin_size)
  
  data_mesh %>%
    mutate(Clase = cut(Longitud, breaks = breaks,
                       include.lowest = TRUE, right = FALSE)) %>%
    group_by(Clase) %>%
    summarise(Frecuencia = n(), .groups = "drop") %>%
    mutate(
      Punto_Medio = as.numeric(sub(".*,([0-9.]+).*",
                                   "\\1", as.character(Clase))) - bin_size/2,
      Proporcion  = Frecuencia / sum(Frecuencia),
      Acumulada   = cumsum(Proporcion)
    )
}

# Aplicar a cada malla con nest()
cdf_all <- datos_long %>%
  group_by(Malla) %>%
  nest() %>%
  mutate(cdf = map(data, calcular_cdf)) %>%
  select(-data) %>%
  unnest(cdf)

write.csv(cdf_all, "resultados_practica9/02_tabla_CDF.csv", row.names = FALSE)

# ==============================================================================
# 7. EXTRAER L25, L50 y L75 POR INTERPOLACIÓN
# ==============================================================================
extraer_percentiles <- function(df_mesh) {
  if(nrow(df_mesh) < 2) return(tibble())
  tibble(
    L25 = round(approx(df_meshAcumulada,dfmeshPunto_Medio,
                       xout = 0.25, rule = 2)$y, 2),
    L50 = round(approx(df_meshAcumulada,dfmeshPunto_Medio,
                       xout = 0.50, rule = 2)$y, 2),
    L75 = round(approx(df_meshAcumulada,dfmeshPunto_Medio,
                       xout = 0.75, rule = 2)$y, 2)
  )
}

percentiles <- cdf_all %>%
  group_by(Malla) %>%
  group_modify(~extraer_percentiles(.x)) %>%
  ungroup() %>%
  left_join(resumen %>% select(Malla, n), by = "Malla")

write.csv(percentiles, "resultados_practica9/03_percentiles_L25_L50_L75.csv",
          row.names = FALSE)
cat("\n=== TALLAS DE SELECTIVIDAD (interpolación) ===\n")
print(percentiles)

# Verificación con quantiles directos
percentiles_q <- datos_long %>%
  group_by(Malla) %>%
  summarise(
    n     = n(),
    L25_q = round(quantile(Longitud, 0.25, na.rm = TRUE), 2),
    L50_q = round(quantile(Longitud, 0.50, na.rm = TRUE), 2),
    L75_q = round(quantile(Longitud, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  )
write.csv(percentiles_q, "resultados_practica9/03b_quantiles_directos.csv",
          row.names = FALSE)
cat("\n=== VERIFICACIÓN CON QUANTILES DIRECTOS ===\n")
print(percentiles_q)

# ==============================================================================
# 8. GRÁFICA 1: TODAS LAS CURVAS JUNTAS
# ==============================================================================
p_todas <- ggplot(cdf_all, aes(x = Punto_Medio, y = Acumulada, color = Malla)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = c(0.25, 0.50, 0.75),
             linetype = "dashed", color = "gray40", alpha = 0.6) +
  scale_y_continuous(labels = percent_format(), breaks = seq(0, 1, 0.25)) +
  labs(title    = "Curvas de selectividad por abertura de malla",
       subtitle = "Atarrayas - Práctica 9",
       x = "Longitud total (cm)",
       y = "Probabilidad acumulada de captura",
       color = "Abertura") +
  theme_minimal()
print(p_todas)
ggsave("resultados_practica9/04_curvas_todas.png", p_todas,
       width = 10, height = 6, dpi = 300)

# ==============================================================================
# 9. GRÁFICA 2: CURVAS INDIVIDUALES (FACET)
# ==============================================================================
p_facet <- ggplot(cdf_all, aes(x = Punto_Medio, y = Acumulada)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 1.5) +
  geom_hline(yintercept = 0.25, linetype = "dotted", color = "red") +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.75, linetype = "dotted", color = "red") +
  facet_wrap(~Malla, scales = "free_x", ncol = 3) +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Curvas de selectividad individuales",
       subtitle = "Líneas rojas: 25%, 50% y 75% de probabilidad acumulada",
       x = "Longitud total (cm)", y = "Probabilidad acumulada") +
  theme_minimal()
print(p_facet)
ggsave("resultados_practica9/05_curvas_facet.png", p_facet,
       width = 12, height = 8, dpi = 300)

# ==============================================================================
# 10. GRÁFICA 3: HISTOGRAMAS DE FRECUENCIA POR MALLA
# ==============================================================================
p_hist <- ggplot(datos_long, aes(x = Longitud, fill = Malla)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.8) +
  facet_wrap(~Malla, scales = "free", ncol = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución de tallas por abertura de malla",
       x = "Longitud total (cm)", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_hist)
ggsave("resultados_practica9/06_histogramas.png", p_hist,
       width = 12, height = 8, dpi = 300)

# ==============================================================================
# 11. GRÁFICA 4: COMPARACIÓN L25-L50-L75
# ==============================================================================
p_comp <- percentiles %>%
  select(Malla, L25, L50, L75) %>%
  pivot_longer(-Malla, names_to = "Percentil", values_to = "Longitud") %>%
  mutate(Percentil = factor(Percentil, levels = c("L25","L50","L75"))) %>%
  ggplot(aes(x = Malla, y = Longitud, fill = Percentil)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = Longitud), position = position_dodge(0.9),
            vjust = -0.3, size = 3) +
  labs(title = "Tallas de selectividad (L25, L50, L75) por malla",
       x = "Abertura de malla", y = "Longitud total (cm)",
       fill = "Percentil") +
  theme_minimal()
print(p_comp)
ggsave("resultados_practica9/07_comparacion_percentiles.png", p_comp,
       width = 10, height = 6, dpi = 300)

# ==============================================================================
# 12. GRÁFICA 5: CURVA CDF SUAVIZADA (estilo Figura 2 del manual)
# ==============================================================================
# Para cada malla se ajusta una curva logística a los puntos de la CDF
# y se marca el L50 estimado
p_suave <- ggplot(cdf_all, aes(x = Punto_Medio, y = Acumulada)) +
  geom_point(color = "orange", size = 2, alpha = 0.7) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ x,
              se = FALSE, color = "darkblue", linewidth = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  facet_wrap(~Malla, scales = "free_x", ncol = 3) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Curvas de selectividad con ajuste logístico",
       x = "Longitud total (cm)", y = "Probabilidad de captura") +
  theme_minimal()
print(p_suave)
ggsave("resultados_practica9/08_curvas_suavizadas.png", p_suave,
       width = 12, height = 8, dpi = 300)

# ==============================================================================
# 13. VISUALIZAR TABLAS
# ==============================================================================
View(resumen)
View(percentiles)
View(percentiles_q)
View(cdf_all)

# ==============================================================================
# 14. MENSAJE FINAL
# ==============================================================================
cat("\n=======================================================\n")
cat("✔ Práctica 9 completada.\n")
cat("✔ Archivos guardados en 'resultados_practica9'.\n")
cat("✔ Tablas abiertas en el visor.\n")
cat("=======================================================\n")
