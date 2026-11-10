# ============================================================
# PRÁCTICA 6: ESTIMACIÓN DE PARÁMETROS DEL CRECIMIENTO (vB)
# Métodos: Ford-Walford + Gulland (1977)
# Script único para Posit Cloud
# ============================================================
# INSTRUCCIONES:
# 1) Sube "P6 PARAMETROS vB.xls" a la carpeta del proyecto.
# 2) En el bloque 1 elige el DATASET con el que trabajarás.
# 3) Ejecuta TODO el script (Ctrl + Shift + Enter).
# 4) Revisa Plots y Files. El CSV principal es
#    "05_parametros_vB.csv".
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

# ============================================================
# 1. DATOS DE ENTRADA (EDITAR AQUÍ)
# ============================================================
# Elige uno de los datasets disponibles:
#   "M_curema"    -> práctica (6 edades)  [POR DEFECTO]
#   "M_cephalus"  -> ejemplo (6 edades)
#   "Phycis"      -> ejemplo del documento (8 edades)
#   "Personalizado" -> introduce tus datos abajo
dataset <- "M_curema"

datos <- switch(dataset,
                "M_curema" = data.frame(
                  Edad = 1:6,
                  Lt   = c(187, 224, 251, 276, 299, 320)
                ),
                "M_cephalus" = data.frame(
                  Edad = 1:6,
                  Lt   = c(158, 204, 246, 282, 317, 345)
                ),
                "Phycis" = data.frame(
                  Edad = 1:8,
                  Lt   = c(80, 132, 177, 215, 250, 280, 310, 340)
                ),
                "Personalizado" = data.frame(
                  Edad = c(1, 2, 3, 4, 5, 6),
                  Lt   = c(187, 224, 251, 276, 299, 320)
                ),
                stop("dataset no reconocido")
)

# Especie / etiqueta para los gráficos
etiqueta_especie <- switch(dataset,
                           "M_curema"   = expression(italic("Mugil curema")),
                           "M_cephalus" = expression(italic("Mugil cephalus")),
                           "Phycis"     = expression(italic("Phycis blennoides")),
                           "Personalizado" = "Especie"
)

cat("\n>>> Dataset:", dataset, "\n")
print(datos)

# ============================================================
# 2. FORD-WALFORD: REORDENAMIENTO Y REGRESIÓN
# ============================================================
# Pares (Lt, Lt+1)
fw <- data.frame(
  Lt  = datos$Lt[-nrow(datos)],
  Lt1 = datos$Lt[-1]
)

cat("\n>>> Pares Lt vs Lt+1 (Ford-Walford):\n")
print(fw)

reg_fw <- lm(Lt1 ~ Lt, data = fw)
a <- unname(coef(reg_fw)[1])
b <- unname(coef(reg_fw)[2])
r2 <- summary(reg_fw)$r.squared

# Parámetros de von Bertalanffy
Linf <- a / (1 - b)
k    <- -log(b)

cat("\n>>> Regresión Ford-Walford: Lt+1 = a + b·Lt\n")
cat(" a (intercepto) =", round(a, 4), "\n")
cat(" b (pendiente)  =", round(b, 4), "\n")
cat(" R²             =", round(r2, 6), "\n")
cat("\n>>> Parámetros de von Bertalanffy:\n")
cat(" L∞ =", round(Linf, 3), "mm\n")
cat(" k  =", round(k, 6), "año⁻¹\n")

# ============================================================
# 3. GULLAND: ESTIMACIÓN DE t0
# ============================================================
t0_i <- datosEdad+(1/k)*log((Linf-datosLt) / Linf)
t0   <- mean(t0_i)

cat("\n>>> t0 por edad (Gulland):\n")
print(data.frame(Edad = datos$Edad,
                 Lt   = datos$Lt,
                 t0_i = round(t0_i, 4)))
cat("\n>>> t0 promedio =", round(t0, 4), "años\n")

# ============================================================
# 4. LONGITUDES ESTIMADAS Y RESIDUOS
# ============================================================
datos <- datos %>%
  mutate(
    Lt_est = Linf * (1 - exp(-k * (Edad - t0))),
    Resid  = Lt - Lt_est,
    SD2    = Resid^2
  )

cat("\n>>> Ajuste: observado vs estimado\n")
print(datos %>% mutate(across(where(is.numeric), ~round(.x, 3))))

cat("\n>>> SD² total (suma de cuadrados) =",
    round(sum(datos$SD2), 4), "\n")
cat(">>> Error típico (raíz de la media de SD²) =",
    round(sqrt(mean(datos$SD2)), 4), "mm\n")

# ============================================================
# 5. GRÁFICO 1: FORD-WALFORD
# ============================================================
g_fw <- ggplot(fw, aes(x = Lt, y = Lt1)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    x = expression(L[t] ~ "(mm)"),
    y = expression(L[t+1] ~ "(mm)"),
    title = paste0("Ford-Walford: ", dataset,
                   "   (Lt+1 = ", round(a,2), " + ", round(b,4), "·Lt)"),
    subtitle = paste0("R² = ", round(r2, 4))
  ) +
  theme_minimal(base_size = 12)
print(g_fw)
ggsave("01_ford_walford.png", g_fw, width = 8, height = 6, dpi = 150)

# ============================================================
# 6. GRÁFICO 2: CURVA DE VON BERTALANFFY
# ============================================================
# Curva suave para graficar
t_seq  <- seq(min(datosEdad)-0.5,max(datosEdad) + 2, length.out = 200)
L_pred <- Linf * (1 - exp(-k * (t_seq - t0)))
curva  <- data.frame(Edad = t_seq, Lt = L_pred)

g_vb <- ggplot() +
  geom_line(data = curva, aes(x = Edad, y = Lt),
            color = "darkgreen", linewidth = 1.2) +
  geom_point(data = datos, aes(x = Edad, y = Lt),
             size = 3, color = "black") +
  geom_point(data = datos, aes(x = Edad, y = Lt_est),
             size = 3, color = "red", shape = 4, stroke = 1.2) +
  geom_hline(yintercept = Linf, linetype = "dashed", color = "blue") +
  annotate("text", x = min(datos$Edad), y = Linf,
           label = paste0("L∞ = ", round(Linf, 1), " mm"),
           vjust = -0.5, hjust = 0, color = "blue") +
  labs(
    x = "Edad (años)",
    y = "Longitud (mm)",
    title = paste("Curva de von Bertalanffy -", dataset),
    subtitle = paste0("L∞ = ", round(Linf,2),
                      " mm,  k = ", round(k,4),
                      " año⁻¹,  t0 = ", round(t0,3), " años"),
    caption = "X = observados, + = estimados"
  ) +
  theme_minimal(base_size = 12)
print(g_vb)
ggsave("02_curva_vb.png", g_vb, width = 9, height = 6, dpi = 150)

# ============================================================
# 7. GRÁFICO 3: RESIDUOS vs EDAD
# ============================================================
g_res <- ggplot(datos, aes(x = Edad, y = Resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(size = 3, color = "steelblue") +
  geom_segment(aes(xend = Edad, yend = 0),
               color = "gray40", linewidth = 0.4) +
  labs(x = "Edad (años)", y = "Residuo (Lt obs - Lt est) [mm]",
       title = paste("Residuos del modelo vB -", dataset)) +
  theme_minimal(base_size = 12)
print(g_res)
ggsave("03_residuos.png", g_res, width = 8, height = 6, dpi = 150)

# ============================================================
# 8. RESUMEN DE PARÁMETROS Y EXPORTAR
# ============================================================
parametros <- data.frame(
  Parametro = c("a (intercepto FW)", "b (pendiente FW)", "R2 Ford-Walford",
                "L∞ (mm)", "k (año^-1)", "t0 (años)",
                "SD² total", "Error típico (mm)",
                "n edades", "Dataset"),
  Valor = c(round(a, 4), round(b, 6), round(r2, 6),
            round(Linf, 3), round(k, 6), round(t0, 4),
            round(sum(datosSD2),3),round(sqrt(mean(datosSD2)), 3),
            nrow(datos), dataset)
)

cat("\n=== RESUMEN DE PARÁMETROS ===\n")
print(parametros)

write.csv(datos,      "04_ajuste_por_edad.csv",       row.names = FALSE)
write.csv(parametros, "05_parametros_vB.csv",         row.names = FALSE)
write.csv(fw,         "06_pares_ford_walford.csv",    row.names = FALSE)

# ============================================================
# 9. RESPUESTAS A LAS PREGUNTAS DE LA PRÁCTICA
# ============================================================
cat("\n============================================================\n")
cat("RESULTADOS PRINCIPALES (Práctica 6)\n")
cat("============================================================\n")
cat(sprintf(" L∞ = %.2f mm\n", Linf))
cat(sprintf(" k  = %.4f año⁻¹\n", k))
cat(sprintf(" t0 = %.3f años\n", t0))
cat(sprintf(" Ecuación: Lt = %.2f · (1 - exp(-%.4f·(t - (%.3f))))\n",
            Linf, k, t0))
cat("------------------------------------------------------------\n")
cat("ARCHIVOS GENERADOS:\n")
cat(" 01_ford_walford.png        (regresión Lt vs Lt+1)\n")
cat(" 02_curva_vb.png            (curva vB con datos)\n")
cat(" 03_residuos.png            (residuos vs edad)\n")
cat(" 04_ajuste_por_edad.csv     (obs, est y SD² por edad)\n")
cat(" 05_parametros_vB.csv       <-- RESULTADO PRINCIPAL\n")
cat(" 06_pares_ford_walford.csv  (pares Lt, Lt+1)\n")
cat("============================================================\n")
