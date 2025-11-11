library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
set.seed(123)
n_estaciones <- 50
promedio_pm25 <- 45.3
desviacion_pm25 <- 8.7
limite_oms <- 15

# --- a) INTERVALO DE CONFIANZA ---
error_tipico <- desviacion_pm25 / sqrt(n_estaciones)
z_95 <- 1.96
lim_inf <- promedio_pm25 - z_95 * error_tipico
lim_sup <- promedio_pm25 + z_95 * error_tipico

cat("RESULTADOS DEL ANÁLISIS DE PM2.5 EN LIMA\n")
cat("=========================================\n")
cat("Concentración promedio:", promedio_pm25, "μg/m³\n")
cat("Desviación estándar:", desviacion_pm25, "μg/m³\n")
cat("Intervalo de confianza 95%: [", round(lim_inf, 1), "-", round(lim_sup, 1), "] μg/m³\n")
cat("Límite OMS:", limite_oms, "μg/m³\n")
cat("Se supera", round(promedio_pm25/limite_oms, 1), "veces el límite recomendado\n\n")

# --- b) GRÁFICO DE DENSIDAD ---
# Generar datos simulados para la distribución
datos_simulados <- data.frame(
  pm25 = rnorm(1000, mean = promedio_pm25, sd = desviacion_pm25)
)

grafico_densidad <- ggplot(datos_simulados, aes(x = pm25)) +
  geom_density(fill = "#E74C3C", alpha = 0.8, color = NA) +
  geom_vline(xintercept = promedio_pm25, color = "#2C3E50", linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = c(lim_inf, lim_sup), color = "#2C3E50", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = limite_oms, color = "#27AE60", linewidth = 1.2, linetype = "solid") +
  annotate("text", x = promedio_pm25, y = 0.025, label = "Promedio 45.3", 
           color = "#2C3E50", hjust = -0.1, size = 3.5) +
  annotate("text", x = limite_oms, y = 0.03, label = "Límite OMS 15", 
           color = "#27AE60", hjust = -0.1, size = 3.5) +
  labs(title = "Distribución de concentraciones de PM2.5 en Lima",
       subtitle = "Líneas discontinuas: intervalo de confianza 95%",
       x = expression("Concentración de PM2.5 (μg/m"^3*")"),
       y = "Densidad") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_line(color = "grey90"))
distritos <- c("San Juan de Lurigancho", "Ate", "Villa El Salvador", 
               "Los Olivos", "San Martín de Porres")

# Crear datos simulados para los distritos
datos_distritos <- data.frame(
  distrito = factor(distritos, levels = distritos),
  concentracion = c(51.2, 47.8, 42.1, 44.9, 49.5),
  lat = c(-12.03, -12.06, -12.21, -12.07, -12.04),
  lon = c(-76.98, -76.85, -76.94, -77.07, -77.08)
)

# Simular puntos de monitoreo en el mapa
puntos_mapa <- data.frame(
  lat = runif(200, -12.25, -11.90),
  lon = runif(200, -77.15, -76.80),
  valor = runif(200, 35, 60)
)

mapa_calor <- ggplot() +
  geom_point(data = puntos_mapa, aes(x = lon, y = lat, color = valor), size = 2, alpha = 0.7) +
  scale_color_viridis(option = "inferno", name = "PM2.5") +
  geom_point(data = datos_distritos, aes(x = lon, y = lat), 
             color = "red", size = 4, shape = 18) +
  geom_text(data = datos_distritos, aes(x = lon, y = lat, label = distrito),
            color = "black", size = 3, hjust = -0.1, check_overlap = TRUE) +
  labs(title = "Concentraciones de PM2.5 por zonas de Lima",
       x = "Longitud", y = "Latitud") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

# --- d) COMPARACIÓN CON LÍMITE OMS ---
datos_comparacion <- data.frame(
  zona = c("Lima Metropolitana", distritos),
  media = c(promedio_pm25, datos_distritos$concentracion),
  n = c(50, 8, 7, 9, 8, 8),
  desv = c(8.7, 7.2, 6.8, 7.5, 7.1, 7.8)
) %>%
  mutate(
    error = desv / sqrt(n),
    lim_inf_zona = media - 1.96 * error,
    lim_sup_zona = media + 1.96 * error,
    supera_limite = media > limite_oms
  )

grafico_barras <- ggplot(datos_comparacion, aes(x = media, y = reorder(zona, media))) +
  geom_col(aes(fill = supera_limite), width = 0.7) +
  geom_errorbarh(aes(xmin = lim_inf_zona, xmax = lim_sup_zona), 
                 height = 0.3, color = "black", linewidth = 0.6) +
  geom_vline(xintercept = limite_oms, color = "#27AE60", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = limite_oms, y = 5.8, label = "Límite OMS 15", 
           color = "#27AE60", hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("#F39C12", "#E74C3C")) +
  labs(title = "Comparación con estándar OMS",
       subtitle = "Barras: promedio | Líneas: intervalo de confianza 95%",
       x = expression("PM2.5 (μg/m"^3*")"),
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA))

# --- e) SERIE TEMPORAL ---
# Generar datos diarios simulados
dias <- 1:30
set.seed(456)
datos_diarios <- data.frame(
  dia = dias,
  concentracion = 45.3 + 10 * sin(dias/30 * 2 * pi) + rnorm(30, 0, 4)
) %>%
  mutate(
    media_movil = zoo::rollmean(concentracion, k = 5, fill = NA),
    lim_inf_dia = media_movil - 1.96 * (desviacion_pm25/sqrt(10)),
    lim_sup_dia = media_movil + 1.96 * (desviacion_pm25/sqrt(10))
  )

serie_temporal <- ggplot(datos_diarios, aes(x = dia)) +
  geom_ribbon(aes(ymin = lim_inf_dia, ymax = lim_sup_dia), 
              fill = "#3498DB", alpha = 0.3) +
  geom_line(aes(y = concentracion), color = "gray50", alpha = 0.6, linewidth = 0.5) +
  geom_line(aes(y = media_movil), color = "#2980B9", linewidth = 1.2) +
  geom_hline(yintercept = limite_oms, color = "#27AE60", linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = promedio_pm25, color = "#E74C3C", linewidth = 0.8, linetype = "dotted") +
  annotate("text", x = 5, y = limite_oms + 2, label = "Límite OMS", 
           color = "#27AE60", size = 3.5, fontface = "bold") +
  annotate("text", x = 25, y = promedio_pm25 + 2, label = "Promedio mensual", 
           color = "#E74C3C", size = 3.5) +
  labs(title = "Evolución diaria de PM2.5",
       subtitle = "Línea azul: media móvil 5 días | Área: intervalo de confianza",
       x = "Día del mes",
       y = expression("PM2.5 (μg/m"^3*")")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))

cat("Generando gráficos...\n")
(grafico_densidad + mapa_calor) / (grafico_barras + serie_temporal) +
  plot_annotation(
    title = "ANÁLISIS DE CALIDAD DEL AIRE - LIMA METROPOLITANA",
    subtitle = "Concentraciones de material particulado PM2.5 - Dirección General de Salud Ambiental",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5))
  )
cat("\nCONCLUSIONES:\n")
cat("=============\n")
cat("• Todas las zonas monitoreadas superan el límite OMS\n")
cat("• San Juan de Lurigancho muestra las mayores concentraciones\n")
cat("• La variabilidad diaria es significativa pero siempre por encima del estándar\n")
cat("• Se requieren medidas urgentes para mejorar la calidad del aire\n")