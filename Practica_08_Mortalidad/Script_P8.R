# ==============================================================================
# PRÁCTICA 8: MORTALIDAD TOTAL, NATURAL Y POR PESCA
# Especies: Lutjanus campechanus y Mugil curema
# Entorno: Posit Cloud
# ==============================================================================

# 1. PAQUETES
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# 2. CARPETA DE RESULTADOS
if(!dir.exists("resultados_practica8")) dir.create("resultados_practica8")

# ==============================================================================
# 3. DATOS PARA LA CURVA DE CAPTURA
# ==============================================================================
# La tabla de abundancias por edad es la misma que aparece en el manual y en el
# documento "DATOS PARA LA PRACTICA DE MORTALIDAD".
datos_captura <- tibble(
  Edad = 0:5,
  Abundancia = c(40, 60, 73, 66, 54, 37)
)

# ==============================================================================
# 4. FUNCIÓN PARA ESTIMAR Z POR CURVA DE CAPTURA
# ==============================================================================
# Se usa la rama descendente, a partir de la edad 2 (reclutamiento completo).
calcular_Z <- function(df, especie, edad_inicio = 2) {
  
  df2 <- df %>%
    mutate(ln_Abundancia = log(Abundancia))
  
  datos_ajuste <- df2 %>%
    filter(Edad >= edad_inicio)
  
  ajuste <- lm(ln_Abundancia ~ Edad, data = datos_ajuste)
  
  pendiente <- coef(ajuste)[2]
  intercepto <- coef(ajuste)[1]
  Z <- -pendiente
  S <- exp(-Z)
  A <- 1 - S
  
  resultado <- tibble(
    Especie = especie,
    Edad_inicio = edad_inicio,
    Intercepto = intercepto,
    Pendiente = pendiente,
    Z = Z,
    Supervivencia_S = S,
    Mortalidad_anual_A = A
  )
  
  p <- ggplot(df2, aes(x = Edad, y = ln_Abundancia)) +
    geom_point(size = 3, color = "gray30") +
    geom_point(data = datos_ajuste, aes(x = Edad, y = ln_Abundancia),
               size = 4, color = "red") +
    geom_abline(intercept = intercepto, slope = pendiente,
                color = "blue", linewidth = 1) +
    labs(
      title = paste("Curva de captura -", especie),
      subtitle = paste("Z =", round(Z, 3), "por año; edades usadas:", 
                       paste(range(datos_ajuste$Edad), collapse = "-")),
      x = "Edad (años)",
      y = "ln(Abundancia relativa)"
    ) +
    theme_minimal()
  
  return(list(resultado = resultado, plot = p, ajuste = ajuste))
}

# ==============================================================================
# 5. CALCULAR Z PARA LAS DOS ESPECIES
# ==============================================================================
especies <- c("Lutjanus campechanus", "Mugil curema")

resultados_Z <- map_dfr(especies, function(sp) {
  calc <- calcular_Z(datos_captura, sp, edad_inicio = 2)
  
  archivo <- paste0("resultados_practica8/01_curva_captura_", 
                    str_replace_all(sp, " ", "_"), ".png")
  ggsave(archivo, plot = calc$plot, width = 8, height = 6, dpi = 300)
  print(calc$plot)
  
  calc$resultado
})

write.csv(resultados_Z, "resultados_practica8/01_resultados_Z.csv", row.names = FALSE)
print("Resultados de mortalidad total Z:")
print(resultados_Z)

# ==============================================================================
# 6. FUNCIÓN PAULY (1980)
# ==============================================================================
pauly_M <- function(Linf, K, T, cardumen = FALSE) {
  M <- exp(-0.0152 - 0.279 * log(Linf) + 0.6543 * log(K) + 0.463 * log(T))
  if(cardumen) M <- 0.8 * M
  return(M)
}

# ==============================================================================
# 7. MORTALIDAD NATURAL PARA Lutjanus campechanus
# ==============================================================================
# Malla del manual: L∞ = 10, 80, 200 cm; K = 0.1, 0.5, 1, 2; T = 10, 15, 20 °C.
# No se aplica corrección por cardumen porque el manual no la indica para esta especie.
M_lutjanus <- expand_grid(
  Linf = c(10, 80, 200),
  K = c(0.1, 0.5, 1, 2),
  T = c(10, 15, 20)
) %>%
  mutate(M = pauly_M(Linf, K, T, cardumen = FALSE)) %>%
  pivot_wider(
    names_from = T,
    values_from = M,
    names_prefix = "M_T_"
  ) %>%
  arrange(Linf, K)

write.csv(M_lutjanus, "resultados_practica8/02_M_Lutjanus_grid.csv", row.names = FALSE)
print("Mortalidad natural M para Lutjanus campechanus:")
print(M_lutjanus)

# ==============================================================================
# 8. MORTALIDAD NATURAL PARA Mugil curema
# ==============================================================================
# Parámetros: L∞ = 46.1 cm; K = 0.14; T = 21, 23, 25, 27 °C.
# Se calcula M normal y M corregida por cardumen (×0.8).
M_mugil <- tibble(
  T = c(21, 23, 25, 27),
  Linf = 46.1,
  K = 0.14
) %>%
  mutate(
    M = pauly_M(Linf, K, T, cardumen = FALSE),
    M_cardumen = pauly_M(Linf, K, T, cardumen = TRUE)
  )

write.csv(M_mugil, "resultados_practica8/03_M_Mugil_curema.csv", row.names = FALSE)
print("Mortalidad natural M para Mugil curema:")
print(M_mugil)

# Gráfica de M vs temperatura para Mugil curema
p_mugil <- M_mugil %>%
  select(T, M, M_cardumen) %>%
  pivot_longer(cols = c(M, M_cardumen), names_to = "Tipo", values_to = "M") %>%
  mutate(Tipo = recode(Tipo,
                       M = "M normal",
                       M_cardumen = "M corregida por cardumen (×0.8)")) %>%
  ggplot(aes(x = T, y = M, color = Tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Mortalidad natural de Mugil curema según temperatura",
    x = "Temperatura (°C)",
    y = "M (por año)",
    color = "Estimación"
  ) +
  theme_minimal()

print(p_mugil)
ggsave("resultados_practica8/03_M_Mugil.png", plot = p_mugil, 
       width = 8, height = 5, dpi = 300)

# ==============================================================================
# 9. VISUALIZAR TABLAS
# ==============================================================================
View(resultados_Z)
View(M_lutjanus)
View(M_mugil)

# ==============================================================================
# 10. MENSAJE FINAL
# ==============================================================================
cat("\n=======================================================\n")
cat("✔ Práctica 8 completada.\n")
cat("✔ Archivos guardados en la carpeta 'resultados_practica8'.\n")
cat("✔ Revisa el panel Files para descargar tablas e imágenes.\n")
cat("=======================================================\n")
