# ============================================================
# PRÁCTICA 1: ANÁLISIS DE TALLAS DE MUGIL CUREMA (LISA)
# TAMAÑO DE MUESTRA, DISTRIBUCIÓN NORMAL,
# PROBABILIDADES DE CAPTURA Y LÍMITES DE CONFIANZA
# ============================================================
# VERSIÓN 1
# - Tamaño de muestra: C = 1.645, d = 1.0 cm (consistente con manual)
# - Frecuencias teóricas: método del manual (punto medio) para secciones 6-8
# - Prueba χ²: usando pnorm() y agrupación con esperadas ≥ 5
# ============================================================

# LIMPIEZA DEL ENTORNO DE TRABAJO
rm(list = ls())
cat("\014")
options(scipen = 999)

# ============================================================
# 1. CARGA DE DATOS DESDE ARCHIVO EXCEL
# ============================================================

archivo_excel <- "datos_tallo_hoja.xls"

if (!file.exists(archivo_excel)) {
  cat("⚠️  ERROR: No se encuentra el archivo 'datos_tallo_hoja.xls'\n")
  cat("Directorio actual:", getwd(), "\n")
  stop("Archivo no encontrado.")
}

if (!require("readxl")) {
  install.packages("readxl")
}
library(readxl)

datos_excel <- read_excel(archivo_excel, sheet = "TyH", col_names = FALSE)

tallas_raw <- datos_excel[[1]]

tallas <- as.numeric(tallas_raw[!is.na(tallas_raw) & 
                                  tallas_raw != "Mugil curema" & 
                                  tallas_raw != "Captura de red trasmallo" & 
                                  tallas_raw != "3 pulgadas"])

tallas <- tallas[!is.na(tallas) & is.numeric(tallas)]

cat("✅ DATOS CARGADOS CORRECTAMENTE\n")
cat("================================\n")
cat("Especie: Mugil curema (Lisa)\n")
cat("Arte de pesca: Red trasmallo de 3 pulgadas\n")
cat("Número total de ejemplares:", length(tallas), "\n")
cat("Rango de tallas:", min(tallas), "-", max(tallas), "cm\n\n")

# ============================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS BÁSICAS
# ============================================================

cat("========== ESTADÍSTICAS DESCRIPTIVAS ==========\n")

n <- length(tallas)
media <- mean(tallas)
mediana <- median(tallas)
varianza <- var(tallas)
desv_est <- sd(tallas)
rango <- max(tallas) - min(tallas)
cv <- (desv_est / media) * 100

cat("Número de ejemplares (n) =", n, "\n")
cat("Talla mínima =", min(tallas), "cm\n")
cat("Talla máxima =", max(tallas), "cm\n")
cat("Rango =", round(rango, 2), "cm\n")
cat("Talla media =", round(media, 2), "cm\n")
cat("Talla mediana =", round(mediana, 2), "cm\n")
cat("Varianza =", round(varianza, 3), "cm²\n")
cat("Desviación estándar =", round(desv_est, 2), "cm\n")
cat("Coeficiente de variación =", round(cv, 2), "%\n\n")

# ============================================================
# 3. ANÁLISIS DE TALLO Y HOJA
# ============================================================

cat("========== DIAGRAMA DE TALLO Y HOJA ==========\n")
cat("(Parte entera = Tallo, Decimales = Hoja)\n\n")

stem(tallas, scale = 2)

cat("\n--- DIAGRAMA DE TALLO Y HOJA DETALLADO ---\n")

tallos <- floor(tallas)
hojas <- round((tallas - tallos) * 10)

tabla_th <- data.frame(Talla = tallas, Tallo = tallos, Hoja = hojas)
tabla_th <- tabla_th[order(tabla_th$Tallo, tabla_th$Hoja), ]

tallos_unicos <- sort(unique(tallos))
for (t in tallos_unicos) {
  hojas_t <- sort(tabla_th$Hoja[tabla_th$Tallo == t])
  hojas_str <- paste(hojas_t, collapse = " ")
  cat(sprintf("%2d | %s\n", t, hojas_str))
}

# ============================================================
# 4. TABLA DE FRECUENCIAS (INTERVALOS DE 1 cm)
# ============================================================

cat("\n========== TABLA DE FRECUENCIAS ==========\n")

intervalos <- c("16.0-16.9", "17.0-17.9", "18.0-18.9", "19.0-19.9",
                "20.0-20.9", "21.0-21.9", "22.0-22.9", "23.0-23.9",
                "24.0-24.9", "25.0-25.9", "26.0-26.9", "27.0-27.9", "28.0-28.9")
punto_medio <- seq(16.5, 28.5, by = 1)

breaks <- seq(16, 29, by = 1)
frecuencias <- table(cut(tallas, breaks = breaks, right = FALSE))

frec_vector <- numeric(length(intervalos))
names(frec_vector) <- intervalos

for (i in 1:length(intervalos)) {
  for (j in 1:length(frecuencias)) {
    if (names(frecuencias)[j] == paste0("[", breaks[i], ",", breaks[i+1], ")")) {
      frec_vector[i] <- as.numeric(frecuencias[j])
      break
    }
  }
}

tabla_frec <- data.frame(
  Intervalo = intervalos,
  Punto_Medio = punto_medio,
  Frecuencia = frec_vector,
  stringsAsFactors = FALSE
)

indices_no_cero <- which(tabla_frec$Frecuencia > 0)
tabla_frec_filtrada <- tabla_frec[min(indices_no_cero):max(indices_no_cero), ]

print(tabla_frec_filtrada)
cat("\nTotal de ejemplares =", sum(tabla_frec_filtrada$Frecuencia), "\n\n")

# ============================================================
# 5. CÁLCULO DEL TAMAÑO DE MUESTRA
# ============================================================
# *** CONSISTENTE CON MANUAL: C = 1.645, d = 1.0 ***

cat("========== CÁLCULO DEL TAMAÑO DE MUESTRA ==========\n")

C <- 1.645
d <- 1.0

tamano_muestra <- (C^2 * varianza) / (d^2)
tamano_muestra_redondeado <- ceiling(tamano_muestra)

cat("Fórmula: n = (C² × s²) / d²\n")
cat("C =", C, "(95% confianza, α = 0.05)\n")
cat("s² =", round(varianza, 3), "\n")
cat("d =", d, "cm\n")
cat("\nTamaño de muestra estimado =", round(tamano_muestra, 2), "≈", 
    tamano_muestra_redondeado, "ejemplares\n")

cat("\nTamaño de muestra actual (n) =", n, "ejemplares\n")
if (n >= tamano_muestra_redondeado) {
  cat("✓ La muestra actual es suficiente (", n, "≥", tamano_muestra_redondeado, ")\n")
} else {
  cat("✗ Se necesitan", tamano_muestra_redondeado - n, "ejemplares más\n")
}

# ============================================================
# 6. DISTRIBUCIÓN NORMAL TEÓRICA (MÉTODO DEL MANUAL)
# ============================================================
# *** Se mantiene el método del manual: evaluar en punto medio ***
# *** Esto permite comparación directa con Excel ***

cat("\n========== DISTRIBUCIÓN NORMAL TEÓRICA ==========\n")
cat("(Método del manual: evaluación en el punto medio de cada intervalo)\n\n")

x_valores <- tabla_frec_filtrada$Punto_Medio
n_obs <- n
ancho_intervalo <- 1

frec_teorica <- function(x, n, media, desv_est, dL) {
  constante <- (n * dL) / (desv_est * sqrt(2 * pi))
  exponente <- exp(-0.5 * ((x - media) / desv_est)^2)
  return(constante * exponente)
}

frec_teoricas <- numeric(length(x_valores))
for (i in 1:length(x_valores)) {
  frec_teoricas[i] <- frec_teorica(x_valores[i], n, media, desv_est, ancho_intervalo)
}

tabla_normal <- data.frame(
  Intervalo = tabla_frec_filtrada$Intervalo,
  x = x_valores,
  Frecuencia_Observada = tabla_frec_filtrada$Frecuencia,
  Frecuencia_Teorica = round(frec_teoricas, 2)
)

print(tabla_normal)

# ============================================================
# 7. PROBABILIDADES DE CAPTURA
# ============================================================

cat("\n========== PROBABILIDADES DE CAPTURA ==========\n")

probabilidades <- frec_teoricas / n

tabla_prob <- data.frame(
  Intervalo = tabla_frec_filtrada$Intervalo,
  Punto_Medio = x_valores,
  Frecuencia_Observada = tabla_frec_filtrada$Frecuencia,
  Frecuencia_Teorica = round(frec_teoricas, 2),
  Probabilidad = round(probabilidades, 4),
  Probabilidad_Porcentual = round(probabilidades * 100, 2)
)

print(tabla_prob)

# ============================================================
# 8. RESPUESTA A LAS PREGUNTAS
# ============================================================

cat("\n========== RESPUESTAS ==========\n")

cat("\na) Probabilidades porcentuales para cada intervalo de tallas:\n")
cat("-------------------------------------------------------------\n")
for (i in 1:nrow(tabla_prob)) {
  cat(sprintf("   %s cm: %.2f%%\n", 
              tabla_prob$Intervalo[i], 
              tabla_prob$Probabilidad_Porcentual[i]))
}

max_prob <- which.max(tabla_prob$Probabilidad)
cat("\nb) Intervalo con mayor probabilidad de captura:\n")
cat("   Intervalo:", tabla_prob$Intervalo[max_prob], "cm\n")
cat("   Probabilidad:", tabla_prob$Probabilidad_Porcentual[max_prob], "%\n")

# ============================================================
# 9. LÍMITES DE CONFIANZA
# ============================================================

cat("\n========== LÍMITES DE CONFIANZA (95%) ==========\n")

gl <- n - 1
t_student <- qt(0.975, df = gl)
error_estandar <- desv_est / sqrt(n)

limite_inferior <- media - t_student * error_estandar
limite_superior <- media + t_student * error_estandar

cat("Grados de libertad (gl) =", gl, "\n")
cat("t(0.975,", gl, ") =", round(t_student, 4), "\n")
cat("Error estándar =", round(error_estandar, 4), "\n")
cat("\nLímite inferior (95%):", round(limite_inferior, 2), "cm\n")
cat("Límite superior (95%):", round(limite_superior, 2), "cm\n")
cat("\nIntervalo de confianza (95%): [", round(limite_inferior, 2), ",", 
    round(limite_superior, 2), "] cm\n")

# ============================================================
# 10. GRÁFICOS
# ============================================================

par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# Gráfico 1: Histograma con curva normal
hist(tallas, breaks = seq(16, 29, by = 1), 
     main = paste("Distribución de Tallas - Mugil curema"),
     xlab = "Talla (cm)", ylab = "Frecuencia",
     col = "lightblue", border = "darkblue", 
     probability = TRUE)

curve(dnorm(x, mean = media, sd = desv_est), 
      from = min(tallas), to = max(tallas), 
      add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Histograma", "Curva Normal"),
       fill = c("lightblue", NA), col = c(NA, "red"), 
       lty = c(NA, 1), lwd = c(NA, 2), bty = "n")

# Gráfico 2: Frecuencias observadas vs teóricas
plot(x_valores, tabla_normal$Frecuencia_Observada, 
     type = "b", pch = 16, col = "blue", 
     xlab = "Talla (cm)", ylab = "Frecuencia",
     main = "Frecuencias Observadas vs Teóricas",
     ylim = c(0, max(c(tabla_normal$Frecuencia_Observada, 
                       tabla_normal$Frecuencia_Teorica)) + 2))
lines(x_valores, tabla_normal$Frecuencia_Teorica, 
      type = "b", pch = 17, col = "red", lty = 2)
legend("topright", legend = c("Observada", "Teórica"),
       col = c("blue", "red"), pch = c(16, 17), 
       lty = c(1, 2), bty = "n")

# Gráfico 3: Probabilidades de captura
barplot(tabla_prob$Probabilidad_Porcentual, 
        names.arg = tabla_prob$Intervalo,
        main = "Probabilidades de Captura por Intervalo",
        xlab = "Intervalo de Talla (cm)", 
        ylab = "Probabilidad (%)",
        col = "lightgreen", border = "darkgreen",
        las = 2, cex.names = 0.7)

# Gráfico 4: Intervalo de confianza
plot(1, 1, type = "n", xlim = c(0, 1), ylim = c(media - 3, media + 3),
     main = "Intervalo de Confianza (95%)",
     xlab = "", ylab = "Talla (cm)", xaxt = "n")
points(0.5, media, pch = 16, col = "black", cex = 1.5)
arrows(0.5, limite_inferior, 0.5, limite_superior, 
       angle = 90, code = 3, length = 0.1, col = "darkred", lwd = 2)
text(0.5, media + 1.2, paste("Media =", round(media, 2)), cex = 0.9)
text(0.5, media - 1.2, paste("IC 95%: [", round(limite_inferior, 2), ",", 
                             round(limite_superior, 2), "]"), cex = 0.9)
abline(h = media, col = "gray", lty = 3)
grid()

par(mfrow = c(1, 1))

# ============================================================
# 11. PRUEBA DE BONDAD DE AJUSTE (χ²) - VERSIÓN ESTADÍSTICA
# ============================================================
# *** CÁLCULO CORRECTO USANDO pnorm() PARA PROBABILIDADES ***
# *** AGRUPACIÓN PARA ESPERADAS ≥ 5 ***

cat("\n========== PRUEBA DE BONDAD DE AJUSTE (χ²) ==========\n")
cat("(Cálculo estadísticamente robusto usando pnorm())\n\n")

# Definir límites de los intervalos (usando los límites reales)
limites <- seq(16, 29, by = 1)

# Calcular probabilidades teóricas usando la CDF normal
prob_teoricas <- numeric(length(limites) - 1)
for (i in 1:(length(limites) - 1)) {
  prob_teoricas[i] <- pnorm(limites[i+1], mean = media, sd = desv_est) - 
    pnorm(limites[i], mean = media, sd = desv_est)
}

# Frecuencias esperadas (sin redondear)
frec_esp_cdf <- prob_teoricas * n

# Crear tabla con todas las categorías
categorias <- paste0(limites[1:(length(limites)-1)], "-", 
                     limites[2:length(limites)] - 0.1)

tabla_chi <- data.frame(
  Categoria = categorias,
  Frec_Observada = as.vector(frecuencias),
  Prob_Teorica = round(prob_teoricas, 4),
  Frec_Esperada = round(frec_esp_cdf, 2)
)

cat("Tabla inicial (todas las categorías):\n")
print(tabla_chi)

# --- AGRUPACIÓN DE CATEGORÍAS CON ESPERADAS ≥ 5 ---

# Función para agrupar de derecha a izquierda
agrupar_categorias <- function(obs, esp) {
  i <- 1
  while (i <= length(esp)) {
    if (esp[i] < 5 && i < length(esp)) {
      # Combinar con la siguiente categoría
      esp[i + 1] <- esp[i + 1] + esp[i]
      obs[i + 1] <- obs[i + 1] + obs[i]
      esp[i] <- NA
      obs[i] <- NA
    } else if (esp[i] < 5 && i == length(esp)) {
      # Si la última es < 5, combinar con la anterior
      if (i > 1) {
        esp[i - 1] <- esp[i - 1] + esp[i]
        obs[i - 1] <- obs[i - 1] + obs[i]
        esp[i] <- NA
        obs[i] <- NA
      }
    }
    i <- i + 1
  }
  return(list(obs = obs, esp = esp))
}

# Aplicar agrupación
resultado <- agrupar_categorias(tabla_chi$Frec_Observada, tabla_chi$Frec_Esperada)
frec_obs_agrup <- resultado$obs[!is.na(resultado$obs)]
frec_esp_agrup <- resultado$esp[!is.na(resultado$esp)]

# Verificar que todas las esperadas ≥ 5
if (any(frec_esp_agrup < 5)) {
  cat("\n⚠️  Advertencia: Algunas frecuencias esperadas son < 5 después de agrupar.\n")
  cat("   Se procederá con la agrupación disponible.\n")
}

# Calcular χ²
chi_cuadrado <- sum((frec_obs_agrup - frec_esp_agrup)^2 / frec_esp_agrup)
gl_chi <- length(frec_obs_agrup) - 3  # 3 parámetros estimados: n, media, desv_est
valor_critico <- qchisq(0.95, df = gl_chi)
valor_p <- 1 - pchisq(chi_cuadrado, df = gl_chi)

# Mostrar resultados
cat("\n--- RESULTADOS DE LA PRUEBA χ² ---\n")
cat("Categorías agrupadas (esperadas ≥ 5):", length(frec_obs_agrup), "\n")
cat("Frecuencias observadas agrupadas:", frec_obs_agrup, "\n")
cat("Frecuencias esperadas agrupadas:", round(frec_esp_agrup, 2), "\n\n")

cat("χ² calculado =", round(chi_cuadrado, 3), "\n")
cat("Grados de libertad =", gl_chi, "\n")
cat("χ² crítico (α=0.05) =", round(valor_critico, 3), "\n")
cat("Valor p =", round(valor_p, 4), "\n\n")

# Conclusión
if (chi_cuadrado < valor_critico) {
  cat("✓ CONCLUSIÓN: χ² calculado < χ² crítico\n")
  cat("  No se rechaza H₀: los datos provienen de una distribución normal\n")
  cat("  (Los datos se ajustan a la distribución normal)\n")
} else {
  cat("✗ CONCLUSIÓN: χ² calculado ≥ χ² crítico\n")
  cat("  Se rechaza H₀: los datos NO provienen de una distribución normal\n")
  cat("  (Los datos NO se ajustan a la distribución normal)\n")
}

# ============================================================
# 12. RESUMEN FINAL
# ============================================================

cat("\n========== RESUMEN FINAL DE LA PRÁCTICA ==========\n")
cat("1. Especie: Mugil curema (Lisa)\n")
cat("2. Arte de pesca: Red trasmallo de 3 pulgadas\n")
cat("3. Número de ejemplares (n):", n, "\n")
cat("4. Talla media:", round(media, 2), "cm\n")
cat("5. Desviación estándar:", round(desv_est, 2), "cm\n")
cat("6. Tamaño de muestra estimado:", tamano_muestra_redondeado, "ejemplares\n")
cat("7. Intervalo de confianza (95%): [", round(limite_inferior, 2), ",", 
    round(limite_superior, 2), "] cm\n")
cat("8. Intervalo con mayor probabilidad de captura:", 
    tabla_prob$Intervalo[max_prob], "cm", 
    "(", tabla_prob$Probabilidad_Porcentual[max_prob], "% )\n")

# ============================================================
# 13. EXPORTACIÓN DE RESULTADOS
# ============================================================

if (!dir.exists("resultados")) {
  dir.create("resultados")
}

write.csv(tabla_frec_filtrada, "resultados/tabla_frecuencias.csv", row.names = FALSE)
write.csv(tabla_normal, "resultados/tabla_normal.csv", row.names = FALSE)
write.csv(tabla_prob, "resultados/tabla_probabilidades.csv", row.names = FALSE)
write.csv(tabla_chi, "resultados/tabla_chi_cuadrado.csv", row.names = FALSE)

sink("resultados/resumen_practica.txt")
cat("========== PRÁCTICA 1 - ANÁLISIS DE TALLAS DE MUGIL CUREMA ==========\n\n")
cat("Fecha de análisis:", Sys.Date(), "\n\n")
cat("Especie: Mugil curema (Lisa)\n")
cat("Arte de pesca: Red trasmallo de 3 pulgadas\n\n")
cat("Número de ejemplares (n):", n, "\n")
cat("Talla media:", round(media, 2), "cm\n")
cat("Desviación estándar:", round(desv_est, 2), "cm\n")
cat("Varianza:", round(varianza, 3), "\n")
cat("Tamaño de muestra estimado:", tamano_muestra_redondeado, "ejemplares\n\n")
cat("Límites de confianza (95%):\n")
cat("  Límite inferior:", round(limite_inferior, 2), "cm\n")
cat("  Límite superior:", round(limite_superior, 2), "cm\n\n")
cat("Probabilidades de captura:\n")
print(tabla_prob[, c("Intervalo", "Probabilidad_Porcentual")])
cat("\nIntervalo con mayor probabilidad:", 
    tabla_prob$Intervalo[max_prob], "cm (", 
    tabla_prob$Probabilidad_Porcentual[max_prob], "% )\n\n")
cat("Prueba de bondad de ajuste (χ²):\n")
cat("  χ² calculado =", round(chi_cuadrado, 3), "\n")
cat("  χ² crítico (α=0.05) =", round(valor_critico, 3), "\n")
cat("  Valor p =", round(valor_p, 4), "\n")
cat("  Conclusión:", ifelse(chi_cuadrado < valor_critico, 
                            "Los datos se ajustan a la distribución normal", 
                            "Los datos NO se ajustan a la distribución normal"), "\n")
sink()

cat("\n✓ Resultados exportados a la carpeta 'resultados/'\n")

# ============================================================
# FIN DEL SCRIPT
# ============================================================
