# ============================================================
# PRÁCTICA: CRECIMIENTO RELATIVO TALLA-PESO
# Relación longitud-peso en Mugil cephalus
# ============================================================

# Instalar/paquetes necesarios
if (!require(readxl)) install.packages("readxl")
library(readxl)

# ------------------------------------------------------------
# 1. LEER DATOS
# ------------------------------------------------------------
cat("Selecciona el archivo Excel de datos (DATOS DE PRÁCTICA.xls)...\n")
archivo <- file.choose()

# Leer la primera hoja del archivo
datos_raw <- read_excel(archivo, sheet = 1)

# Convertir a data.frame
datos_raw <- as.data.frame(datos_raw)

# Detectar columnas por nombre aproximado
nombres <- names(datos_raw)
col_localidad <- grep("Localidad", nombres, ignore.case = TRUE)[1]
col_longitud  <- grep("Longitud",  nombres, ignore.case = TRUE)[1]
col_peso      <- grep("Peso",      nombres, ignore.case = TRUE)[1]

# Verificar que existan
if (any(is.na(c(col_localidad, col_longitud, col_peso)))) {
  stop("No se encontraron las columnas Localidad, Longitud total y Peso total.")
}

# Crear data.frame limpio
datos <- data.frame(
  Localidad = datos_raw[[col_localidad]],
  Lt = as.numeric(datos_raw[[col_longitud]]),
  P  = as.numeric(datos_raw[[col_peso]])
)

# Eliminar filas con NA
datos <- na.omit(datos)

# Asegurar que Localidad sea factor
datosLocalidad<-as.factor(datosLocalidad)

# Transformación logarítmica
datoslnLt<-log(datosLt)
datoslnP <-log(datosP)

# ------------------------------------------------------------
# 2. AJUSTE GLOBAL
# ------------------------------------------------------------
mod_global <- lm(ln_P ~ ln_Lt, data = datos)

# Resumen del modelo
cat("\n================ MODELO GLOBAL =================\n")
print(summary(mod_global))

# Parámetros
a_global <- exp(coef(mod_global)[1])
b_global <- coef(mod_global)[2]
se_b_global <- summary(mod_global)$coefficients[2, 2]
df_global <- df.residual(mod_global)

# Prueba t para b vs 3
t_global <- (b_global - 3) / se_b_global
p_global <- 2 * pt(-abs(t_global), df_global)

# IC 95% para b
ic_b_global <- confint(mod_global, "ln_Lt", level = 0.95)

cat("\n--- Resultado global ---\n")
cat("a =", round(a_global, 6), "\n")
cat("b =", round(b_global, 4), "\n")
cat("Error estándar de b =", round(se_b_global, 4), "\n")
cat("t = (b - 3)/SE_b =", round(t_global, 4), "\n")
cat("gl =", df_global, "\n")
cat("p-valor =", format.pval(p_global, digits = 4), "\n")
cat("IC 95% para b: [", round(ic_b_global[1], 4), ", ", round(ic_b_global[2], 4), "]\n", sep = "")
cat("R² =", round(summary(mod_global)$r.squared, 4), "\n")

# Interpretación automática
cat("\nInterpretación:\n")
if (p_global < 0.05) {
  if (b_global > 3) {
    cat("b > 3 y p < 0.05: crecimiento alométrico positivo. El peso aumenta más rápido de lo esperado.\n")
  } else {
    cat("b < 3 y p < 0.05: crecimiento alométrico negativo. El peso aumenta más lento de lo esperado.\n")
  }
} else {
  cat("p >= 0.05: no se rechaza b = 3. Crecimiento isométrico.\n")
}

# ------------------------------------------------------------
# 3. AJUSTE POR LOCALIDAD
# ------------------------------------------------------------
locales <- levels(datos$Localidad)

resultados_loc <- lapply(locales, function(loc) {
  sub <- subset(datos, Localidad == loc)
  mod <- lm(ln_P ~ ln_Lt, data = sub)
  
  a <- exp(coef(mod)[1])
  b <- coef(mod)[2]
  se_b <- summary(mod)$coefficients[2, 2]
  df <- df.residual(mod)
  t_val <- (b - 3) / se_b
  p_val <- 2 * pt(-abs(t_val), df)
  ic <- confint(mod, "ln_Lt", level = 0.95)
  r2 <- summary(mod)$r.squared
  n <- nrow(sub)
  
  data.frame(
    Localidad = loc,
    n = n,
    a = a,
    b = b,
    SE_b = se_b,
    t_vs_3 = t_val,
    gl = df,
    p_valor = p_val,
    IC95_inf = ic[1],
    IC95_sup = ic[2],
    R2 = r2
  )
})

tabla_loc <- do.call(rbind, resultados_loc)

cat("\n================ RESULTADOS POR LOCALIDAD =================\n")
print(tabla_loc, row.names = FALSE)

# ------------------------------------------------------------
# 4. COMPARACIÓN DE PENDIENTES ENTRE LOCALIDADES
# ------------------------------------------------------------
cat("\n================ COMPARACIÓN DE PENDIENTES (ANCOVA) =================\n")

# Modelo con interacción: prueba si las pendientes difieren entre localidades
mod_inter <- lm(ln_P ~ ln_Lt * Localidad, data = datos)
anova_inter <- anova(mod_inter)
print(anova_inter)

cat("\nSi la fila 'ln_Lt:Localidad' tiene p < 0.05, las pendientes b difieren entre localidades.\n")

# Modelo sin interacción (pendiente común)
mod_add <- lm(ln_P ~ ln_Lt + Localidad, data = datos)
cat("\nComparación modelo aditivo vs interactivo:\n")
print(anova(mod_add, mod_inter))

# Opcional: comparaciones pareadas de pendientes con emmeans
if (requireNamespace("emmeans", quietly = TRUE)) {
  library(emmeans)
  cat("\n--- Tendencias por localidad (emmeans) ---\n")
  print(emtrends(mod_inter, pairwise ~ Localidad, var = "ln_Lt"))
} else {
  cat("\nPara comparaciones pareadas de pendientes, instala el paquete 'emmeans':\n")
  cat("install.packages('emmeans')\n")
}

# ------------------------------------------------------------
# 5. GRÁFICOS
# ------------------------------------------------------------
colores <- rainbow(length(locales))

# 5.1 Gráfico log-log
plot(ln_P ~ ln_Lt, data = datos,
     col = colores[as.numeric(datos$Localidad)],
     pch = 19,
     xlab = "ln(Longitud total, cm)",
     ylab = "ln(Peso total, g)",
     main = "Relación talla-peso linealizada")

abline(mod_global, lwd = 2, lty = 2, col = "black")

for (i in seq_along(locales)) {
  sub <- subset(datos, Localidad == locales[i])
  mod <- lm(ln_P ~ ln_Lt, data = sub)
  abline(mod, col = colores[i], lwd = 2)
}

legend("topleft",
       legend = c("Global", locales),
       col = c("black", colores),
       lwd = 2,
       lty = c(2, rep(1, length(locales))),
       bty = "n")

# 5.2 Gráfico en escala original
plot(P ~ Lt, data = datos,
     col = colores[as.numeric(datos$Localidad)],
     pch = 19,
     xlab = "Longitud total (cm)",
     ylab = "Peso total (g)",
     main = "Relación talla-peso")

Lt_seq <- seq(min(datosLt),max(datosLt), length.out = 100)

# Curva global
lines(Lt_seq, a_global * Lt_seq^b_global, lwd = 2, lty = 2, col = "black")

# Curvas por localidad
for (i in seq_along(locales)) {
  sub <- subset(datos, Localidad == locales[i])
  mod <- lm(ln_P ~ ln_Lt, data = sub)
  a <- exp(coef(mod)[1])
  b <- coef(mod)[2]
  lines(Lt_seq, a * Lt_seq^b, col = colores[i], lwd = 2)
}

legend("topleft",
       legend = c("Global", locales),
       col = c("black", colores),
       lwd = 2,
       lty = c(2, rep(1, length(locales))),
       bty = "n")

# ------------------------------------------------------------
# 6. EXPORTAR RESULTADOS
# ------------------------------------------------------------
write.csv(tabla_loc, "resultados_talla_peso_por_localidad.csv", row.names = FALSE)
cat("\nResultados por localidad guardados en: resultados_talla_peso_por_localidad.csv\n")

# Fin del script
