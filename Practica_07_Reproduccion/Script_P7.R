#==============================================================================
# PRÁCTICA 7: INDICADORES DE LA REPRODUCCIÓN E ÍNDICES DE MADUREZ GONÁDICA
# Especie: Mugil cephalus (Lisa)
# Entorno: Posit Cloud
# ==============================================================================

# 1. INSTALAR PAQUETES (Solo la primera vez, luego puedes comentar esta línea)
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl))   install.packages("readxl")
if(!require(scales))   install.packages("scales")

# 2. CARGAR LIBRERÍAS
library(tidyverse)
library(readxl)
library(scales)

# 3. CREAR CARPETA DE RESULTADOS
if(!dir.exists("resultados_practica7")) dir.create("resultados_practica7")

# ==============================================================================
# 4. CARGAR DATOS (Lectura a ciegas desde la fila 3)
# ==============================================================================
datos_raw <- read_excel("LISA DATOS PARA PRÁCTICA.XLS", sheet = "A", 
                        skip = 2, col_names = FALSE)
names(datos_raw) <- c("Fecha", "ID", "Lt", "Pt", "Pe", "Pv", "Pg", "Sexo", "Edo", "Ph")

# ==============================================================================
# 5. LIMPIEZA Y PREPARACIÓN
# ==============================================================================
datos <- datos_raw %>%
  mutate(Fecha = as.character(Fecha)) %>%
  fill(Fecha, .direction = "down") %>%
  filter(!is.na(Lt), !is.na(Pt), !is.na(Sexo)) %>%
  mutate(
    across(c(Lt, Pt, Pe, Pv, Pg, Ph), ~suppressWarnings(as.numeric(ifelse(.x == "X", NA, .x)))),
    Edo = suppressWarnings(as.numeric(Edo)),
    Sexo = toupper(trimws(as.character(Sexo)))
  ) %>%
  filter(Sexo %in% c("H", "M")) %>%
  mutate(
    Pe = ifelse(Pe < 0, NA, Pe),
    Pg = ifelse(Pg < 0, NA, Pg),
    Ph = ifelse(Ph < 0, NA, Ph)
  ) %>%
  mutate(
    Fecha = factor(Fecha, levels = c("OCTUBRE", "NOVIEMBRE", "DICIEMBRE", 
                                     "ENERO", "FEBRERO", "MARZO", "ABRIL", 
                                     "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE"))
  ) %>%
  mutate(
    IGS = (Pg / Pt) * 100,
    IHS = (Ph / Pt) * 100,
    K   = (Pe / (Lt^3)) * 100
  )

write.csv(datos, "resultados_practica7/00_datos_limpios.csv", row.names = FALSE)
cat("✔ Base de datos limpia guardada. Registros válidos:", nrow(datos), "\n")

# ==============================================================================
# 6. ANÁLISIS Y GRÁFICAS
# ==============================================================================

# ------------------------------------------------------------------------------
# 6.1. ESTADIOS DE MADUREZ SEXUAL MENSUAL
# ------------------------------------------------------------------------------
estadios_freq <- datos %>%
  group_by(Fecha, Sexo, Edo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Fecha, Sexo) %>%
  mutate(Porcentaje = (n / sum(n)) * 100) %>%
  ungroup()

write.csv(estadios_freq, "resultados_practica7/01_tabla_estadios_madurez.csv", row.names = FALSE)

p1 <- ggplot(estadios_freq, aes(x = Fecha, y = Porcentaje, fill = factor(Edo))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~Sexo, labeller = labeller(Sexo = c("H" = "Hembras", "M" = "Machos"))) +
  scale_fill_brewer(palette = "YlOrRd", name = "Estadio") +
  labs(title = "Estadios de Madurez Sexual Mensual", x = "Mes", y = "Frecuencia (%)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)
ggsave("resultados_practica7/01_estadios_madurez.png", plot = p1, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 6.2. PROPORCIÓN DE SEXOS
# ------------------------------------------------------------------------------
prop_sexos <- datos %>%
  group_by(Fecha, Sexo) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Fecha) %>%
  mutate(Porcentaje = (n / sum(n)) * 100) %>%
  ungroup()

write.csv(prop_sexos, "resultados_practica7/02_tabla_proporcion_sexos.csv", row.names = FALSE)

p2 <- ggplot(prop_sexos, aes(x = Fecha, y = Porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("H" = "purple", "M" = "skyblue"), 
                    labels = c("Hembras", "Machos"), name = "Sexo") +
  labs(title = "Proporción Mensual de Sexos", x = "Mes", y = "Proporción") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)
ggsave("resultados_practica7/02_proporcion_sexos.png", plot = p2, width = 10, height = 6, dpi = 300)

# Prueba Chi-cuadrado (proporción 1:1)
chi_result <- chisq.test(table(datos$Sexo))
chi_df <- data.frame(
  Prueba   = "Chi-cuadrado Proporción 1:1",
  Chi_valor = as.numeric(chi_result$statistic),
  gl       = as.numeric(chi_result$parameter),
  p_value  = chi_result$p.value
)
write.csv(chi_df, "resultados_practica7/02_chi_cuadrado.csv", row.names = FALSE)
print("Prueba de Chi-cuadrado:")
print(chi_result)

# ------------------------------------------------------------------------------
# 6.3. TALLA MÍNIMA DE MADUREZ SEXUAL
# ------------------------------------------------------------------------------
talla_minima <- datos %>% 
  filter(Edo >= 4) %>% 
  group_by(Sexo) %>% 
  summarise(Talla_Minima_Madurez_cm = min(Lt, na.rm = TRUE), .groups = "drop")

write.csv(talla_minima, "resultados_practica7/03_talla_minima_madurez.csv", row.names = FALSE)
print("Talla mínima de madurez (cm):")
print(talla_minima)

# ------------------------------------------------------------------------------
# 6.4. TALLA DE PRIMERA MADUREZ (L50) - HEMBRAS
# ------------------------------------------------------------------------------
datos_L50 <- datos %>%
  filter(Sexo == "H") %>%
  mutate(Madura = ifelse(Edo >= 4, 1, 0)) %>%
  mutate(Clase_Talla = cut(Lt, breaks = seq(20, 55, by = 2), include.lowest = TRUE)) %>%
  group_by(Clase_Talla) %>%
  summarise(Total = n(), Maduras = sum(Madura, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Prop_Maduras = Maduras / Total,
    Punto_Medio  = as.numeric(sub(".*,([0-9.]+).*", "\\1", as.character(Clase_Talla))) - 1
  )

modelo_L50 <- glm(cbind(Maduras, Total - Maduras) ~ Punto_Medio, 
                  data = datos_L50, family = binomial)
L50_valor <- -coef(modelo_L50)[1] / coef(modelo_L50)[2]

write.csv(datos_L50, "resultados_practica7/04_tabla_L50.csv", row.names = FALSE)
write.csv(data.frame(L50_cm = round(L50_valor, 2)), "resultados_practica7/04_valor_L50.csv", row.names = FALSE)
cat("✔ L50 estimado:", round(L50_valor, 2), "cm\n")

p3 <- ggplot(datos_L50, aes(x = Punto_Medio, y = Prop_Maduras)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = L50_valor, linetype = "dotted", color = "darkred") +
  labs(title = "Talla de Primera Madurez (L50) - Hembras",
       subtitle = paste("L50 =", round(L50_valor, 2), "cm"),
       x = "Longitud Total (cm)", y = "Proporción de Maduras") +
  scale_y_continuous(labels = percent_format()) + theme_minimal()
print(p3)
ggsave("resultados_practica7/03_L50.png", plot = p3, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 6.5. ÍNDICES (IGS, IHS, K)
# ------------------------------------------------------------------------------
indices_mensuales <- datos %>%
  group_by(Fecha, Sexo) %>%
  summarise(
    IGS_Media = mean(IGS, na.rm = TRUE), IGS_SD = sd(IGS, na.rm = TRUE),
    IHS_Media = mean(IHS, na.rm = TRUE), IHS_SD = sd(IHS, na.rm = TRUE),
    K_Media   = mean(K, na.rm = TRUE),   K_SD   = sd(K, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(indices_mensuales, "resultados_practica7/05_tabla_indices.csv", row.names = FALSE)

graficar_indice <- function(data, col_media, col_sd, titulo, ylab) {
  ggplot(data, aes(x = Fecha, y = .data[[col_media]], color = Sexo, group = Sexo)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    geom_errorbar(aes(ymin = .data[[col_media]] - .data[[col_sd]], 
                      ymax = .data[[col_media]] + .data[[col_sd]]), width = 0.2) +
    scale_color_manual(values = c("H" = "purple", "M" = "skyblue"), 
                       labels = c("Hembras", "Machos"), name = "Sexo") +
    labs(title = titulo, x = "Mes", y = ylab) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p4 <- graficar_indice(indices_mensuales, "IGS_Media", "IGS_SD", 
                      "Índice Gonadosomático (IGS)", "IGS (%)")
print(p4)
ggsave("resultados_practica7/05_IGS.png", plot = p4, width = 10, height = 6, dpi = 300)

p5 <- graficar_indice(indices_mensuales, "IHS_Media", "IHS_SD", 
                      "Índice Hepatosomático (IHS)", "IHS (%)")
print(p5)
ggsave("resultados_practica7/06_IHS.png", plot = p5, width = 10, height = 6, dpi = 300)

p6 <- graficar_indice(indices_mensuales, "K_Media", "K_SD", 
                      "Factor de Condición (K)", "K")
print(p6)
ggsave("resultados_practica7/07_Factor_K.png", plot = p6, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 7. VISUALIZAR TABLAS EN PANTALLA (View)
# ==============================================================================
View(datos)               # Base de datos completa limpia
View(estadios_freq)       # Tabla de estadios mensuales
View(prop_sexos)          # Tabla de proporción de sexos
View(talla_minima)        # Talla mínima por sexo
View(datos_L50)           # Tabla para el cálculo de L50
View(indices_mensuales)   # Tabla de IGS, IHS, K mensuales

# ==============================================================================
# 8. MENSAJE FINAL
# ==============================================================================
cat("\n=======================================================\n")
cat("✔ ¡Análisis completado con éxito!\n")
cat("✔ Todos los archivos se guardaron en 'resultados_practica7'\n")
cat("✔ Ve a la pestaña Files (abajo a la derecha) para descargarlos.\n")
cat("✔ Las tablas también se abrieron en pestañas nuevas del visor.\n")
cat("=======================================================\n")
