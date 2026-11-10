# ============================================================
# PRÁCTICA 5: RETROCÁLCULO DE TALLAS
# Métodos: Dahl-Lea, Fraser-Lee, Whitney & Carlander, Regresión
# Script único para Posit Cloud
# ============================================================
# INSTRUCCIONES:
# 1) Sube el archivo "Retrocalculo.xls" a la carpeta del proyecto
#    (panel Files -> Upload).
# 2) Verifica el nombre en el bloque 1 (PARÁMETROS).
# 3) Ejecuta TODO el script con Ctrl + Shift + Enter.
# 4) Revisa Plots (gráficos) y Files (resultados .csv).
# 5) Consulta 3 publicaciones recientes (2010+) sobre retrocálculo.
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
# 1. PARÁMETROS EDITABLES
# ------------------------------------------------------------
archivo     <- "Retrocalculo.xls"
hoja        <- "Todos"
skip_rows   <- 6        # filas antes de la primera fila de datos
cols_datos  <- 1:6      # columnas A:F = Fecha, Lt, St, Sn1, Sn2, Sn3
etiquetas_excluir <- c("Average", "Std", "Min", "Max")

# ------------------------------------------------------------
# 2. LEER DATOS
# ------------------------------------------------------------
datos_raw <- readxl::read_excel(archivo, sheet = hoja,
                                skip = skip_rows, col_names = FALSE)
datos_raw <- datos_raw[, cols_datos]
colnames(datos_raw) <- c("Fecha", "Lt", "St", "Sn1", "Sn2", "Sn3")

datos <- datos_raw %>%
  mutate(
    Fecha = as.character(Fecha),
    across(c(Lt, St, Sn1, Sn2, Sn3),
           ~suppressWarnings(as.numeric(.x)))
  ) %>%
  filter(
    !is.na(Fecha),
    !(Fecha %in% etiquetas_excluir),
    !is.na(Lt), Lt > 0,
    !is.na(St), St > 0
  )

cat("\n>>> Registros cargados:", nrow(datos), "\n")
cat(">>> Rango de Lt:", range(datos$Lt), "mm\n")
cat(">>> Rango de St:", range(datos$St), "mm\n")
cat(">>> Peces con annulus 1:", sum(!is.na(datos$Sn1)), "\n")
cat(">>> Peces con annulus 2:", sum(!is.na(datos$Sn2)), "\n")
cat(">>> Peces con annulus 3:", sum(!is.na(datos$Sn3)), "\n")

# ------------------------------------------------------------
# 3. REGRESIÓN Lt = c + b·St
# ------------------------------------------------------------
reg <- lm(Lt ~ St, data = datos)
c_  <- coef(reg)[1]
b_  <- coef(reg)[2]
r2  <- summary(reg)$r.squared

cat("\n>>> Regresión lineal Lt ~ St\n")
cat(" c (intercepto) =", round(c_, 4), "\n")
cat(" b (pendiente)  =", round(b_, 4), "\n")
cat(" R²             =", round(r2, 4), "\n")
cat(" (Comparar con: c = 16.32, b = 0.87 del Excel)\n")

g_reg <- ggplot(datos, aes(x = St, y = Lt)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(x = "Radio total de la escama St (mm)",
       y = "Longitud furcal Lt (mm)",
       title = paste0("Lt = ", round(c_,2), " + ", round(b_,2),
                      "·St   (R² = ", round(r2,3), ")")) +
  theme_minimal(base_size = 12)
print(g_reg)
ggsave("01_regresion_Lt_St.png", g_reg, width = 8, height = 6, dpi = 150)

# ------------------------------------------------------------
# 4. RETROCÁLCULO POR PEZ
# ------------------------------------------------------------
retro <- datos %>%
  mutate(ID = row_number()) %>%
  select(ID, Fecha, Lt, St, Sn1, Sn2, Sn3) %>%
  mutate(
    # Dahl-Lea
    DL_1 = (Sn1 / St) * Lt,
    DL_2 = (Sn2 / St) * Lt,
    DL_3 = (Sn3 / St) * Lt,
    # Fraser-Lee
    FL_1 = (Sn1 / St) * (Lt - c_) + c_,
    FL_2 = (Sn2 / St) * (Lt - c_) + c_,
    FL_3 = (Sn3 / St) * (Lt - c_) + c_,
    # Whitney & Carlander
    WC_1 = (c_ / b_) + (Lt + c_ / b_) * (Sn1 / St),
    WC_2 = (c_ / b_) + (Lt + c_ / b_) * (Sn2 / St),
    WC_3 = (c_ / b_) + (Lt + c_ / b_) * (Sn3 / St),
    # Regresión (Lt = c + b·Sn)
    RG_1 = c_ + b_ * Sn1,
    RG_2 = c_ + b_ * Sn2,
    RG_3 = c_ + b_ * Sn3
  )

# ------------------------------------------------------------
# 5. RESUMEN POR ANNULUS (media ± DE)
# ------------------------------------------------------------
m <- function(x) mean(x, na.rm = TRUE)
s <- function(x) sd(x,   na.rm = TRUE)
n <- function(x) sum(!is.na(x))

resumen <- data.frame(
  Annulus     = 1:3,
  n           = c(n(retroSn1),n(retroSn2), n(retro$Sn3)),
  DahlLea     = c(m(retroDL1),m(retroDL_2), m(retro$DL_3)),
  FraserLee   = c(m(retroFL1),m(retroFL_2), m(retro$FL_3)),
  WhitneyCarl = c(m(retroWC1),m(retroWC_2), m(retro$WC_3)),
  Regresion   = c(m(retroRG1),m(retroRG_2), m(retro$RG_3)),
  SD_DL       = c(s(retroDL1),s(retroDL_2), s(retro$DL_3)),
  SD_FL       = c(s(retroFL1),s(retroFL_2), s(retro$FL_3)),
  SD_WC       = c(s(retroWC1),s(retroWC_2), s(retro$WC_3)),
  SD_RG       = c(s(retroRG1),s(retroRG_2), s(retro$RG_3))
) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

cat("\n>>> Longitudes medias retrocalculadas (mm):\n")
print(resumen)

# ------------------------------------------------------------
# 6. GRÁFICO COMPARATIVO DE MÉTODOS
# ------------------------------------------------------------
resumen_long <- resumen %>%
  select(Annulus, DahlLea, FraserLee, WhitneyCarl, Regresion) %>%
  pivot_longer(-Annulus, names_to = "Metodo", values_to = "Longitud")

g_comp <- ggplot(resumen_long,
                 aes(x = factor(Annulus), y = Longitud, fill = Metodo)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = round(Longitud, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(x = "Annulus", y = "Longitud media retrocalculada (mm)",
       title = "Comparación de métodos de retrocálculo",
       fill = "Método") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
print(g_comp)
ggsave("02_comparacion_metodos.png", g_comp, width = 10, height = 6, dpi = 150)

# ------------------------------------------------------------
# 7. FENÓMENO DE LEE (Fraser-Lee, annulus 1)
# ------------------------------------------------------------
retro_lee <- retro %>%
  mutate(
    EdadCaptura = case_when(
      !is.na(Sn3) ~ "3+",
      !is.na(Sn2) ~ "2",
      !is.na(Sn1) ~ "1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(EdadCaptura), !is.na(FL_1))

g_lee <- ggplot(retro_lee, aes(x = EdadCaptura, y = FL_1)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(x = "Edad al momento de la captura",
       y = "Talla retrocalculada al annulus 1 (mm)",
       title = "Fenómeno de Lee (Fraser-Lee)") +
  theme_minimal(base_size = 12)
print(g_lee)
ggsave("03_fenomeno_lee.png", g_lee, width = 8, height = 6, dpi = 150)

# ------------------------------------------------------------
# 8. EXPORTAR RESULTADOS
# ------------------------------------------------------------
write.csv(retro,   "04_retrocalculos_individuales.csv", row.names = FALSE)
write.csv(resumen, "05_resumen_medias.csv",             row.names = FALSE)

# ------------------------------------------------------------
# 9. RESPUESTAS A LAS PREGUNTAS DE LA PRÁCTICA
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("ARCHIVOS GENERADOS:\n")
cat(" 01_regresion_Lt_St.png            (regresión Lt vs St)\n")
cat(" 02_comparacion_metodos.png        (comparación de métodos)\n")
cat(" 03_fenomeno_lee.png               (Fenómeno de Lee)\n")
cat(" 04_retrocalculos_individuales.csv (por pez)\n")
cat(" 05_resumen_medias.csv             <-- RESULTADO PRINCIPAL\n")
cat("============================================================\n")

cat("\n=== RESPUESTAS: Talla media retrocalculada por edad ===\n\n")
edades <- c("Edad 1 (annulus 1)", "Edad 2 (annulus 2)", "Edad 3 (annulus 3)")
for (i in 1:3) {
  cat(edades[i], ":\n")
  cat("   Dahl-Lea           :", resumen$DahlLea[i],     "mm\n")
  cat("   Fraser-Lee         :", resumen$FraserLee[i],   "mm\n")
  cat("   Whitney & Carlander:", resumen$WhitneyCarl[i], "mm\n")
  cat("   Regresión          :", resumen$Regresion[i],   "mm\n")
  cat("   n =", resumen$n[i], "peces\n\n")
}

cat("=== FIN DEL SCRIPT ===\n")
