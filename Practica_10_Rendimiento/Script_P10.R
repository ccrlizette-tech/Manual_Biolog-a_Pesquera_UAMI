# ==============================================================================
# PRÁCTICA 10: RENDIMIENTO POR RECLUTA (Y/R) - MODELO DE BEVERTON Y HOLT
# Entorno: Posit Cloud
# ==============================================================================

# 1. PAQUETES
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# 2. CARPETA DE RESULTADOS
if(!dir.exists("resultados_practica10")) dir.create("resultados_practica10")

# ==============================================================================
# 3. PARÁMETROS DEL MANUAL
# ==============================================================================
K    <- 0.19      # Coef. de crecimiento von Bertalanffy (año^-1)
t0   <- -0.2      # Edad inicial teórica (años)
Tr   <- 0.4       # Edad de reclutamiento (años)
Winf <- 286       # Peso asintótico (g)
M    <- 1.1       # Mortalidad natural (año^-1)

# Valores de F según el manual
F_vals <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
            1.2, 1.4, 1.6, 1.8, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5,
            5.0, 5.5, 6.0)

# Valores de Tc a evaluar (según el manual)
Tc_vals <- c(0.8, 1.0, 1.2)

# ==============================================================================
# 4. FUNCIÓN Y/R DE BEVERTON Y HOLT
# ==============================================================================
YR_BH <- function(F, Tc, K, t0, Tr, Winf, M) {
  S   <- exp(-K * (Tc - t0))            # Sobrevivencia del recluta a la edad Tc
  Z   <- M + F                          # Mortalidad total
  num <- F * exp(-M * (Tc - Tr)) * Winf * (1 - S)
  den <- Z + K * S
  num / den
}

# ==============================================================================
# 5. CALCULAR Y/R PARA TODAS LAS COMBINACIONES
# ==============================================================================
resultados <- expand_grid(F = F_vals, Tc = Tc_vals) %>%
  mutate(
    YR = YR_BH(F, Tc, K, t0, Tr, Winf, M),
    Z  = M + F
  )

# Tabla pivoteada (una columna por Tc)
tabla_pivot <- resultados %>%
  mutate(Col = paste0("YR_Tc_", Tc)) %>%
  select(F, YR, Col) %>%
  pivot_wider(names_from = Col, values_from = YR) %>%
  mutate(across(starts_with("YR_"), ~round(.x, 2)))

write.csv(resultados,   "resultados_practica10/01_tabla_YR_completa.csv", row.names = FALSE)
write.csv(tabla_pivot,  "resultados_practica10/02_tabla_YR_pivoteada.csv", row.names = FALSE)

cat("\n=== TABLA Y/R PARA CADA Tc (g/recluta) ===\n")
print(as.data.frame(tabla_pivot))

# ==============================================================================
# 6. IDENTIFICAR F_RMS (F al máximo Y/R) POR CADA Tc
# ==============================================================================
RMS <- resultados %>%
  group_by(Tc) %>%
  slice_max(YR, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(F_RMS = F, YR_max = YR) %>%
  mutate(across(c(F_RMS, YR_max), ~round(.x, 3)))

write.csv(RMS, "resultados_practica10/03_F_RMS.csv", row.names = FALSE)
cat("\n=== F óptimo (RMS) por Tc ===\n")
print(as.data.frame(RMS))

# ==============================================================================
# 7. GRÁFICA 1: TODAS LAS Tc EN UNA MISMA CURVA
# ==============================================================================
p1 <- ggplot(resultados, aes(x = F, y = YR, color = factor(Tc))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(data = RMS, aes(xintercept = F_RMS, color = factor(Tc)),
             linetype = "dashed", alpha = 0.7) +
  labs(
    title    = "Rendimiento por recluta (Y/R) vs mortalidad por pesca (F)",
    subtitle = paste0("Modelo de Beverton y Holt — K = ", K, ", M = ", M,
                      ", Tr = ", Tr, ", W∞ = ", Winf, " g"),
    x = "Mortalidad por pesca F (año⁻¹)",
    y = "Y/R (g por recluta)",
    color = "Tc (años)"
  ) +
  theme_minimal()
print(p1)
ggsave("resultados_practica10/04_curvas_YR_todas.png", p1,
       width = 10, height = 6, dpi = 300)

# ==============================================================================
# 8. GRÁFICA 2: UNA CURVA POR Tc CON PUNTO DE RMS
# ==============================================================================
p2 <- ggplot(resultados, aes(x = F, y = YR)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_point(size = 2, color = "darkblue") +
  geom_vline(data = RMS, aes(xintercept = F_RMS),
             linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_hline(data = RMS, aes(yintercept = YR_max),
             linetype = "dotted", color = "red", linewidth = 0.6) +
  geom_point(data = RMS, aes(x = F_RMS, y = YR_max),
             size = 4, color = "red", shape = 18) +
  geom_text(data = RMS,
            aes(x = F_RMS, y = YR_max,
                label = paste0("F_RMS = ", F_RMS,
                               "\nY/R máx = ", round(YR_max, 2), " g")),
            vjust = -0.5, hjust = -0.1, size = 3.5, color = "red") +
  facet_wrap(~Tc, scales = "free_y", ncol = 3,
             labeller = labeller(Tc = function(x) paste0("Tc = ", x, " años"))) +
  labs(
    title    = "Curvas de Y/R para diferentes edades de primera captura (Tc)",
    x = "Mortalidad por pesca F (año⁻¹)",
    y = "Y/R (g por recluta)"
  ) +
  theme_minimal()
print(p2)
ggsave("resultados_practica10/05_curvas_YR_facet.png", p2,
       width = 12, height = 5, dpi = 300)

# ==============================================================================
# 9. GRÁFICA 3: CURVA DESTACADA PARA Tc = 1.0 (ESTILO FIG. 3 DEL MANUAL)
# ==============================================================================
datos_tc1 <- resultados %>% filter(Tc == 1.0)
RMS_tc1   <- RMS       %>% filter(Tc == 1.0)

p3 <- ggplot(datos_tc1, aes(x = F, y = YR)) +
  geom_line(linewidth = 1.3, color = "darkblue") +
  geom_point(size = 2, color = "darkblue") +
  geom_vline(xintercept = RMS_tc1$F_RMS, linetype = "dashed",
             color = "red", linewidth = 1) +
  geom_hline(yintercept = RMS_tc1$YR_max, linetype = "dotted",
             color = "red") +
  annotate("point", x = RMS_tc1FRMS,y=RMStc1YR_max,
           size = 5, color = "red", shape = 18) +
  annotate("text", x = RMS_tc1FRMS,y=RMStc1YR_max,
           label = paste0("Y/R máx = ", round(RMS_tc1$YR_max, 2),
                          " g\nF_RMS = ", RMS_tc1$F_RMS, " año⁻¹"),
           hjust = -0.15, vjust = 1.2, color = "red", size = 3.5) +
  annotate("segment", x = RMS_tc1$F_RMS + 0.2, xend = 6,
           y = RMS_tc1YRmax-0.2,yend=RMStc1YR_max - 0.2,
           arrow = arrow(length = unit(0.3, "cm"), ends = "last"),
           color = "gray40") +
  annotate("text", x = 4.5, y = RMS_tc1$YR_max - 0.5,
           label = "Sobrepesca de crecimiento",
           color = "gray30", size = 3.5) +
  labs(
    title    = "Curva de rendimiento por recluta — Tc = 1.0 año",
    subtitle = "El máximo define el Rendimiento Máximo Sostenible (RMS)",
    x = "Mortalidad por pesca F (año⁻¹)",
    y = "Y/R (g por recluta)"
  ) +
  theme_minimal()
print(p3)
ggsave("resultados_practica10/06_curva_YR_Tc1.png", p3,
       width = 10, height = 6, dpi = 300)

# ==============================================================================
# 10. VISUALIZAR TABLAS
# ==============================================================================
View(resultados)
View(tabla_pivot)
View(RMS)

cat("\n✔ Práctica 10 lista. Archivos en 'resultados_practica10'\n")
