library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)
library(lubridate)

# Configuración
set.seed(123)
options(scipen = 999)

# --- DATOS INICIALES ---
total_publicaciones <- 2400
positivo <- 1680
neutral <- 480
negativo <- 240

prop_positivo <- positivo / total_publicaciones
prop_neutral <- neutral / total_publicaciones
prop_negativo <- negativo / total_publicaciones

semanas <- 12

cat("ANÁLISIS DE SENTIMIENTO EN REDES SOCIALES - CUSCO\n")
cat("=================================================\n")
cat("PROMPERÚ - Temporada alta (junio-agosto)\n")
cat("Total publicaciones:", total_publicaciones, "\n")
cat("Período:", semanas, "semanas\n\n")

cat("DISTRIBUCIÓN GLOBAL DE SENTIMIENTOS:\n")
cat("Positivo: ", positivo, "(", round(prop_positivo * 100, 1), "%)\n")
cat("Neutral:  ", neutral, "(", round(prop_neutral * 100, 1), "%)\n")
cat("Negativo: ", negativo, "(", round(prop_negativo * 100, 1), "%)\n\n")

# --- a) INTERVALOS DE CONFIANZA AL 95% ---
# Método normal
z <- qnorm(0.975)
se_normal <- sqrt(prop_positivo * (1 - prop_positivo) / total_publicaciones)
ic_normal_inf <- prop_positivo - z * se_normal
ic_normal_sup <- prop_positivo + z * se_normal

# Método Wilson
p <- prop_positivo
n <- total_publicaciones
wilson_center <- (p + z^2/(2*n)) / (1 + z^2/n)
wilson_se <- (z/(1 + z^2/n)) * sqrt(p*(1-p)/n + z^2/(4*n^2))
ic_wilson_inf <- wilson_center - wilson_se
ic_wilson_sup <- wilson_center + wilson_se

cat("a) INTERVALOS DE CONFIANZA 95% PARA SENTIMIENTO POSITIVO:\n")
cat("---------------------------------------------------------\n")
cat("Método Normal: [", round(ic_normal_inf * 100, 1), "%, ", 
    round(ic_normal_sup * 100, 1), "%]\n")
cat("Método Wilson: [", round(ic_wilson_inf * 100, 1), "%, ", 
    round(ic_wilson_sup * 100, 1), "%]\n\n")

# --- GENERAR DATOS SEMANALES SIMULADOS ---
set.seed(456)
datos_semanales <- data.frame(
  semana = 1:semanas,
  fecha = seq.Date(from = as.Date("2024-06-01"), by = "week", length.out = semanas),
  total = rep(total_publicaciones / semanas, semanas)
) %>%
  mutate(
    # Simular variación semanal con tendencia estacional
    tendencia_positivo = 0.7 + 0.1 * sin(2 * pi * (semana - 1) / semanas),
    ruido_positivo = rnorm(semanas, 0, 0.03),
    prop_positivo_semana = pmax(0.6, pmin(0.85, tendencia_positivo + ruido_positivo)),
    
    prop_neutral_semana = runif(semanas, 0.15, 0.25),
    prop_negativo_semana = 1 - prop_positivo_semana - prop_neutral_semana,
    
    # Asegurar que no sean negativos
    prop_negativo_semana = pmax(0.05, prop_negativo_semana),
    prop_neutral_semana = 1 - prop_positivo_semana - prop_negativo_semana,
    
    # Calcular conteos
    positivo = round(total * prop_positivo_semana),
    neutral = round(total * prop_neutral_semana),
    negativo = round(total * prop_negativo_semana),
    
    # Ajustar para que sumen exactamente al total semanal
    total_ajustado = positivo + neutral + negativo,
    factor_ajuste = total / total_ajustado,
    positivo = round(positivo * factor_ajuste),
    neutral = round(neutral * factor_ajuste),
    negativo = total - positivo - neutral,
    
    # Recalcular proporciones
    prop_positivo = positivo / total,
    prop_neutral = neutral / total,
    prop_negativo = negativo / total,
    
    # Error estándar semanal
    se_semana = sqrt(prop_positivo * (1 - prop_positivo) / total),
    ic_inf_semana = prop_positivo - 1.96 * se_semana,
    ic_sup_semana = prop_positivo + 1.96 * se_semana
  )

# --- b) GRÁFICO DE BARRAS APILADAS SEMANAL ---
datos_largos <- datos_semanales %>%
  select(semana, positivo, neutral, negativo) %>%
  pivot_longer(cols = c(positivo, neutral, negativo), 
               names_to = "sentimiento", 
               values_to = "conteo") %>%
  mutate(
    sentimiento = factor(sentimiento, levels = c("negativo", "neutral", "positivo")),
    prop = conteo / (total_publicaciones / semanas)
  )

grafico_barras_apiladas <- ggplot(datos_largos, aes(x = factor(semana), y = conteo, fill = sentimiento)) +
  geom_col(position = "fill", alpha = 0.9, color = "white", size = 0.3) +
  geom_text(aes(label = paste0(round(prop * 100, 0), "%")), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3) +
  scale_fill_manual(values = c("positivo" = "#27AE60", "neutral" = "#F39C12", "negativo" = "#E74C3C"),
                    labels = c("Positivo", "Neutral", "Negativo")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "DISTRIBUCIÓN SEMANAL DE SENTIMIENTOS",
    subtitle = "Barras apiladas al 100% - Proporciones semanales",
    x = "Semana",
    y = "Proporción",
    fill = "Sentimiento"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- c) GRÁFICO DE LÍNEAS CON BANDA DE CONFIANZA ---
grafico_lineas <- ggplot(datos_semanales, aes(x = semana, y = prop_positivo)) +
  geom_ribbon(aes(ymin = ic_inf_semana, ymax = ic_sup_semana), 
              fill = "#27AE60", alpha = 0.2) +
  geom_line(color = "#27AE60", size = 1.5, alpha = 0.8) +
  geom_point(color = "#27AE60", size = 3, fill = "white", shape = 21, stroke = 2) +
  geom_text(aes(label = paste0(round(prop_positivo * 100, 0), "%")), 
            vjust = -1.2, fontface = "bold", color = "#27AE60", size = 4) +
  geom_hline(yintercept = prop_positivo, linetype = "dashed", color = "#2C3E50", alpha = 0.7) +
  annotate("text", x = 6, y = prop_positivo + 0.02, 
           label = paste("Promedio:", round(prop_positivo * 100, 1), "%"), 
           color = "#2C3E50", fontface = "bold") +
  scale_y_continuous(labels = percent_format(), limits = c(0.5, 0.9)) +
  scale_x_continuous(breaks = 1:semanas) +
  labs(
    title = "EVOLUCIÓN SEMANAL DE SENTIMIENTO POSITIVO",
    subtitle = "Línea: proporción semanal | Área: intervalo de confianza 95%",
    x = "Semana",
    y = "Proporción de Sentimiento Positivo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- d) GRÁFICO DE WAFFLE MANUAL ---
crear_waffle_manual <- function() {
  # Crear datos para el waffle (10x10 grid)
  waffle_data <- expand.grid(x = 1:10, y = 1:10)
  
  # Asignar sentimientos basados en las proporciones
  n_positivo <- round(prop_positivo * 100)
  n_neutral <- round(prop_neutral * 100)
  n_negativo <- 100 - n_positivo - n_neutral
  
  waffle_data$sentimiento <- c(
    rep("Positivo", n_positivo),
    rep("Neutral", n_neutral),
    rep("Negativo", n_negativo)
  )
  
  ggplot(waffle_data, aes(x = x, y = y, fill = sentimiento)) +
    geom_tile(color = "white", size = 1.5, alpha = 0.9) +
    scale_fill_manual(values = c("Positivo" = "#27AE60", "Neutral" = "#F39C12", "Negativo" = "#E74C3C")) +
    coord_fixed() +
    labs(
      title = "DISTRIBUCIÓN GLOBAL DE SENTIMIENTOS",
      subtitle = paste("Cada cuadrado representa 1% (", total_publicaciones/100, "publicaciones)"),
      fill = "Sentimiento"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 9)
    )
}

waffle_plot <- crear_waffle_manual()

# --- e) FUNNEL PLOT PARA DETECCIÓN DE ATÍPICOS ---
datos_funnel <- datos_semanales %>%
  mutate(
    prop_positivo = positivo / total,
    se = sqrt(prop_positivo * (1 - prop_positivo) / total),
    limite_superior = prop_positivo + 2 * se,
    limite_inferior = prop_positivo - 2 * se,
    atipico = prop_positivo_semana > limite_superior | prop_positivo_semana < limite_inferior
  )

funnel_plot <- ggplot(datos_funnel, aes(x = total, y = prop_positivo)) +
  geom_point(aes(color = atipico, size = atipico), alpha = 0.8) +
  geom_line(aes(y = prop_positivo), color = "#27AE60", linetype = "dashed", alpha = 0.5) +
  geom_ribbon(aes(ymin = prop_positivo - 2 * se, ymax = prop_positivo + 2 * se),
              fill = "#27AE60", alpha = 0.1) +
  geom_text(aes(label = ifelse(atipico, paste0("Semana ", semana), "")),
            vjust = -0.8, size = 3, fontface = "bold") +
  scale_color_manual(values = c("FALSE" = "#27AE60", "TRUE" = "#E74C3C")) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4)) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "FUNNEL PLOT - DETECCIÓN DE SEMANAS ATÍPICAS",
    subtitle = "Área gris: ±2 errores estándar | Puntos rojos: semanas atípicas",
    x = "Total de Publicaciones por Semana",
    y = "Proporción de Sentimiento Positivo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- f) GRÁFICO DE ÁREA APILADA NORMALIZADA ---
grafico_area <- ggplot(datos_largos, aes(x = semana, y = prop, fill = sentimiento)) +
  geom_area(alpha = 0.8, color = "white", size = 0.3) +
  scale_fill_manual(values = c("positivo" = "#27AE60", "neutral" = "#F39C12", "negativo" = "#E74C3C"),
                    labels = c("Positivo", "Neutral", "Negativo")) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = 1:semanas) +
  labs(
    title = "EVOLUCIÓN TEMPORAL DE SENTIMIENTOS",
    subtitle = "Áreas apiladas al 100% - Tendencia semanal",
    x = "Semana",
    y = "Proporción",
    fill = "Sentimiento"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- GRÁFICO ADICIONAL: PIE CHART PARA PROPORCIONES GLOBALES ---
datos_pie <- data.frame(
  sentimiento = c("Positivo", "Neutral", "Negativo"),
  valor = c(positivo, neutral, negativo),
  prop = c(prop_positivo, prop_neutral, prop_negativo)
) %>%
  mutate(
    posicion = cumsum(prop) - prop/2,
    etiqueta = paste0(sentimiento, "\n", round(prop * 100, 1), "%\n", "(", valor, ")")
  )

pie_plot <- ggplot(datos_pie, aes(x = "", y = prop, fill = sentimiento)) +
  geom_col(color = "white", alpha = 0.9) +
  geom_text(aes(y = posicion, label = etiqueta), 
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Positivo" = "#27AE60", "Neutral" = "#F39C12", "Negativo" = "#E74C3C")) +
  coord_polar(theta = "y") +
  labs(
    title = "DISTRIBUCIÓN GLOBAL",
    subtitle = paste("Total:", total_publicaciones, "publicaciones"),
    fill = "Sentimiento"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

# --- GRÁFICO ADICIONAL: COMPARACIÓN ICs MÉTODOS ---
datos_ics <- data.frame(
  Metodo = c("Normal", "Wilson"),
  Punto = c(prop_positivo, wilson_center),
  IC_inf = c(ic_normal_inf, ic_wilson_inf),
  IC_sup = c(ic_normal_sup, ic_wilson_sup)
)

grafico_ics <- ggplot(datos_ics, aes(x = Punto, y = Metodo)) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2, size = 1.5, color = "#2980B9") +
  geom_point(size = 4, color = "#2980B9") +
  geom_text(aes(label = paste0(round(Punto * 100, 1), "%")), 
            vjust = -1.2, fontface = "bold", color = "#2980B9") +
  scale_x_continuous(labels = percent_format(), limits = c(0.65, 0.75)) +
  labs(
    title = "COMPARACIÓN DE MÉTODOS PARA IC",
    subtitle = "Intervalos de confianza al 95% para sentimiento positivo",
    x = "Proporción de Sentimiento Positivo",
    y = "Método"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12)
  )

# --- COMBINAR TODOS LOS GRÁFICOS ---
cat("Generando visualizaciones completas...\n")

# Diseño con patchwork
(grafico_barras_apiladas | grafico_lineas) /
  (waffle_plot | pie_plot) /
  (funnel_plot | grafico_area) /
  (grafico_ics) +
  plot_annotation(
    title = 'ANÁLISIS COMPLETO DE SENTIMIENTO EN REDES SOCIALES - CUSCO',
    subtitle = 'PROMPERÚ - Temporada alta junio-agosto 2024 (2,400 publicaciones analizadas)',
    caption = 'Fuente: Monitoreo de redes sociales - Dirección de Inteligencia Comercial',
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# --- RESUMEN ESTADÍSTICO ---
cat("\nRESUMEN ESTADÍSTICO DEL ANÁLISIS\n")
cat("=================================\n")
cat("Publicaciones totales:", total_publicaciones, "\n")
cat("Período de análisis:", semanas, "semanas\n")
cat("Publicaciones por semana:", round(total_publicaciones/semanas), "\n\n")

cat("PROPORCIONES GLOBALES:\n")
cat("Positivo: ", round(prop_positivo * 100, 1), "% (IC 95%: [", 
    round(ic_wilson_inf * 100, 1), "%, ", round(ic_wilson_sup * 100, 1), "%])\n")
cat("Neutral:  ", round(prop_neutral * 100, 1), "%\n")
cat("Negativo: ", round(prop_negativo * 100, 1), "%\n\n")

cat("VARIACIÓN SEMANAL (Sentimiento Positivo):\n")
cat("Mínimo:  ", round(min(datos_semanales$prop_positivo) * 100, 1), "% (Semana ", 
    which.min(datos_semanales$prop_positivo), ")\n")
cat("Máximo:  ", round(max(datos_semanales$prop_positivo) * 100, 1), "% (Semana ", 
    which.max(datos_semanales$prop_positivo), ")\n")
cat("Promedio:", round(mean(datos_semanales$prop_positivo) * 100, 1), "%\n")
cat("Desviación estándar:", round(sd(datos_semanales$prop_positivo) * 100, 1), "%\n\n")

cat("SEMANAS ATÍPICAS IDENTIFICADAS:\n")
semanas_atipicas <- which(datos_funnel$atipico)
if(length(semanas_atipicas) > 0) {
  for(sem in semanas_atipicas) {
    cat("• Semana", sem, ":", round(datos_funnel$prop_positivo[sem] * 100, 1), "%\n")
  }
} else {
  cat("• No se identificaron semanas con comportamiento atípico significativo\n")
}

cat("\nCONCLUSIONES:\n")
cat("============\n")
cat("• El sentimiento predominante es claramente positivo (>70%)\n")
cat("• La estabilidad semanal sugiere una experiencia turística consistente\n")
cat("• Los intervalos de confianza muestran alta precisión en las estimaciones\n")
cat("• Cusco mantiene una excelente percepción en redes sociales durante temporada alta\n")