# ==============================================================================
# PRÁCTICA 11: RMS CON EL MODELO HOLÍSTICO DE SCHAEFER (1954)
# Entorno: Posit Cloud
# ==============================================================================

# 1. PAQUETES
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(scales))    install.packages("scales")
library(tidyverse); library(scales)

# 2. CARPETA DE RESULTADOS
if(!dir.exists("resultados_practica11")) dir.create("resultados_practica11")

# ==============================================================================
# 3. DATOS DE CAPTURA Y ESFUERZO (Tabla 1 del manual)
# ==============================================================================
# C = captura en miles de toneladas (toneladas *10^3)
# f = número de buques estándar (esfuerzo)
datos <- tibble(
  Anio = 1999:2007,
  f    = c(1869, 1884, 1560, 1539, 1983, 2757, 3474, 5910, 3951),
  C    = c(150,  147,  142.5, 135, 153,  168,  198,  174,  156)
) %>%
  mutate(
    CPUE = (C * 1000) / f      # Captura por unidad de esfuerzo (toneladas/buque)
  )

cat("\n=== DATOS CON CPUE CALCULADO ===\n")
print(as.data.frame(datos))

write.csv(datos, "resultados_practica11/01_tabla_datos_CPUE.csv", row.names = FALSE)

# ==============================================================================
# 4. REGRESIÓN LINEAL CPUE = a - b*f
# ==============================================================================
modelo <- lm(CPUE ~ f, data = datos)

a <- coef(modelo)[1]         # Intercepto (CPUE máxima teórica, t/buque)
b <- -coef(modelo)[2]        # Pendiente positiva (reducción de CPUE por buque)
r2 <- summary(modelo)$r.squared

cat("\n=== REGRESIÓN LINEAL CPUE = a - b·f ===\n")
cat("a (intercepto) =", round(a, 4), "t/buque\n")
cat("b (pendiente)  =", round(b, 8), "t/buque²\n")
cat("R²             =", round(r2, 4), "\n")
cat("Ecuación: CPUE =", round(a, 4), "-", round(b, 8), "* f\n")

# ==============================================================================
# 5. ESTIMACIÓN DEL RMS Y ESFUERZO ÓPTIMO
# ==============================================================================
f_opt_ton  <- a / (2 * b)            # Esfuerzo óptimo (buques)
RMS_ton    <- a^2 / (4 * b)          # RMS en toneladas
RMS_miles  <- RMS_ton / 1000         # RMS en miles de toneladas
CPUE_opt   <- a / 2                  # CPUE en el óptimo

resumen_RMS <- tibble(
  Parametro       = c("a (intercepto)", "b (pendiente)", "R²",
                      "f_opt (buques)", "CPUE_opt (t/buque)",
                      "RMS (toneladas)", "RMS (miles de toneladas)"),
  Valor           = c(round(a, 4), round(b, 8), round(r2, 4),
                      round(f_opt_ton, 0), round(CPUE_opt, 2),
                      round(RMS_ton, 0), round(RMS_miles, 2))
)

cat("\n=== RESULTADOS DEL MODELO DE SCHAEFER ===\n")
print(as.data.frame(resumen_RMS))

write.csv(resumen_RMS, "resultados_practica11/02_resumen_RMS.csv", row.names = FALSE)

# ==============================================================================
# 6. CURVA DE RENDIMIENTO R(f) = a·f - b·f²
# ==============================================================================
f_seq <- seq(0, max(datos$f) * 1.2, length.out = 300)
curva_R <- tibble(
  f       = f_seq,
  R_ton   = a * f_seq - b * f_seq^2,
  R_miles = R_ton / 1000
)

write.csv(curva_R, "resultados_practica11/03_curva_rendimiento.csv", row.names = FALSE)

# ==============================================================================
# 7. GRÁFICA 1: CPUE vs ESFUERZO (relación lineal)
# ==============================================================================
p1 <- ggplot(datos, aes(x = f, y = CPUE)) +
  geom_point(size = 3, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  geom_hline(yintercept = a, linetype = "dashed", color = "gray50") +
  annotate("text", x = max(datos$f) * 0.7, y = a * 0.97,
           label = paste0("a = ", round(a, 2)),
           color = "gray30", size = 3.5, hjust = 0) +
  labs(
    title    = "Relación CPUE vs esfuerzo pesquero",
    subtitle = paste0("CPUE = ", round(a, 4), " - ", round(b, 8), "·f   |   R² = ", round(r2, 3)),
    x = "Esfuerzo (f) — número de buques estándar",
    y = "CPUE (toneladas por buque)"
  ) +
  theme_minimal()
print(p1)
ggsave("resultados_practica11/04_CPUE_vs_f.png", p1, width = 9, height = 6, dpi = 300)

# ==============================================================================
# 8. GRÁFICA 2: CURVA DE PRODUCCIÓN DE SCHAEFER (parábola)
# ==============================================================================
p2 <- ggplot() +
  # Curva de rendimiento
  geom_line(data = curva_R, aes(x = f, y = R_miles),
            color = "darkblue", linewidth = 1.3) +
  # Capturas observadas
  geom_point(data = datos, aes(x = f, y = C),
             size = 3, color = "black", shape = 16) +
  # Punto del RMS
  geom_point(aes(x = f_opt_ton, y = RMS_miles),
             size = 5, color = "red", shape = 18) +
  geom_vline(xintercept = f_opt_ton, linetype = "dashed",
             color = "red", alpha = 0.7) +
  geom_hline(yintercept = RMS_miles, linetype = "dashed",
             color = "red", alpha = 0.7) +
  # Anotaciones
  annotate("text", x = f_opt_ton, y = RMS_miles,
           label = paste0("RMS = ", round(RMS_miles, 2), " miles t\n",
                          "f_opt = ", round(f_opt_ton, 0), " buques"),
           hjust = -0.15, vjust = 1.3, color = "red", size = 3.5) +
  # Zona de sobrepesca
  annotate("text", x = max(datos$f) * 0.95, y = RMS_miles * 0.2,
           label = "Sobrepesca", color = "gray40",
           size = 3.5, fontface = "italic") +
  labs(
    title    = "Curva de producción excedente de Schaefer",
    subtitle = paste0("R(f) = a·f - b·f²   |   RMS = a²/(4b) = ",
                      round(RMS_miles, 2), " miles de t"),
    x = "Esfuerzo (f) — número de buques estándar",
    y = "Captura / Rendimiento (miles de toneladas)"
  ) +
  theme_minimal()
print(p2)
ggsave("resultados_practica11/05_curva_Schaefer.png", p2, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 9. GRÁFICA 3: CAPTURA OBSERVADA vs CAPTURA PREDICHA POR EL MODELO
# ==============================================================================
datos <- datos %>%
  mutate(C_pred = (a * f - b * f^2) / 1000)  # miles de toneladas

p3 <- ggplot(datos, aes(x = Anio)) +
  geom_line(aes(y = C, color = "Observada"), linewidth = 1) +
  geom_point(aes(y = C, color = "Observada"), size = 3) +
  geom_line(aes(y = C_pred, color = "Predicha (Schaefer)"),
            linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = C_pred, color = "Predicha (Schaefer)"), size = 2.5) +
  scale_color_manual(values = c("Observada" = "black",
                                "Predicha (Schaefer)" = "red")) +
  labs(
    title = "Captura observada vs captura predicha por el modelo",
    x = "Año", y = "Captura (miles de toneladas)", color = ""
  ) +
  theme_minimal() + theme(legend.position = "bottom")
print(p3)
ggsave("resultados_practica11/06_obs_vs_predicha.png", p3, width = 10, height = 5, dpi = 300)

# ==============================================================================
# 10. TABLA COMPARATIVA FINAL
# ==============================================================================
tabla_final <- datos %>%
  mutate(
    CPUE           = round(CPUE, 2),
    C_pred_miles   = round(C_pred, 2),
    Diferencia     = round(C - C_pred_miles, 2)
  ) %>%
  select(Anio, f, C, CPUE, C_pred_miles, Diferencia)

write.csv(tabla_final, "resultados_practica11/07_tabla_comparativa.csv", row.names = FALSE)
print(tabla_final)

# ==============================================================================
# 11. VISUALIZAR
# ==============================================================================
View(datos)
View(resumen_RMS)
View(curva_R)
View(tabla_final)

cat("\n=======================================================\n")
cat("✔ Práctica 11 completada.\n")
cat("✔ Archivos en 'resultados_practica11'.\n")
cat("=======================================================\n")

