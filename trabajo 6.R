library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Configuración
set.seed(123)
options(scipen = 999)

# --- DATOS INICIALES ---
# Grupos principales
grupo_control <- list(
  promedio = 13.2,
  sd = 2.1,
  n = 450
)

grupo_experimental <- list(
  promedio = 14.5,
  sd = 1.8,
  n = 430
)

# Diferencias por área académica
areas_academicas <- data.frame(
  Area = c("Ingenierías", "Ciencias de la Salud", "Ciencias Sociales", "Humanidades"),
  Diferencia = c(1.1, 1.5, 0.9, 1.3),
  n_control = c(120, 110, 100, 120),
  n_experimental = c(115, 105, 95, 115)
)

n_universidades <- 30

cat("ANÁLISIS DE IMPACTO DE TUTORÍA VIRTUAL - SUNEDU\n")
cat("===============================================\n")
cat("Año académico 2024 -", n_universidades, "universidades\n\n")

cat("GRUPO CONTROL (sin tutoría):\n")
cat("- Promedio:", grupo_control$promedio, "\n")
cat("- Desviación estándar:", grupo_control$sd, "\n")
cat("- Estudiantes:", grupo_control$n, "\n\n")

cat("GRUPO EXPERIMENTAL (con tutoría):\n")
cat("- Promedio:", grupo_experimental$promedio, "\n")
cat("- Desviación estándar:", grupo_experimental$sd, "\n")
cat("- Estudiantes:", grupo_experimental$n, "\n\n")

# --- a) INTERVALOS DE CONFIANZA AL 99% PARA CADA GRUPO ---
z_99 <- qnorm(0.995)

ic_control_inf <- grupo_control$promedio - z_99 * (grupo_control$sd / sqrt(grupo_control$n))
ic_control_sup <- grupo_control$promedio + z_99 * (grupo_control$sd / sqrt(grupo_control$n))

ic_experimental_inf <- grupo_experimental$promedio - z_99 * (grupo_experimental$sd / sqrt(grupo_experimental$n))
ic_experimental_sup <- grupo_experimental$promedio + z_99 * (grupo_experimental$sd / sqrt(grupo_experimental$n))

cat("a) INTERVALOS DE CONFIANZA 99%:\n")
cat("------------------------------\n")
cat("Grupo Control: [", round(ic_control_inf, 2), "-", round(ic_control_sup, 2), "]\n")
cat("Grupo Experimental: [", round(ic_experimental_inf, 2), "-", round(ic_experimental_sup, 2), "]\n\n")

# --- d) INTERVALO DE CONFIANZA PARA DIFERENCIA GLOBAL ---
diferencia_global <- grupo_experimental$promedio - grupo_control$promedio
se_diferencia <- sqrt((grupo_control$sd^2 / grupo_control$n) + (grupo_experimental$sd^2 / grupo_experimental$n))
ic_diff_inf <- diferencia_global - z_99 * se_diferencia
ic_diff_sup <- diferencia_global + z_99 * se_diferencia

cat("d) DIFERENCIA GLOBAL ENTRE GRUPOS:\n")
cat("----------------------------------\n")
cat("Diferencia:", round(diferencia_global, 2), "puntos\n")
cat("IC 99% diferencia: [", round(ic_diff_inf, 2), "-", round(ic_diff_sup, 2), "]\n")
cat("¿Diferencia significativa?", ifelse(ic_diff_inf > 0, "SÍ", "NO"), "\n\n")

# --- GENERAR DATOS SIMULADOS ---
set.seed(456)
# Datos para gráficos de densidad
datos_densidad <- data.frame(
  Grupo = rep(c("Control", "Experimental"), 
              c(grupo_control$n, grupo_experimental$n)),
  Promedio = c(rnorm(grupo_control$n, grupo_control$promedio, grupo_control$sd),
               rnorm(grupo_experimental$n, grupo_experimental$promedio, grupo_experimental$sd))
)

# Datos por área académica con ICs
areas_academicas <- areas_academicas %>%
  mutate(
    se_diferencia = sqrt((grupo_control$sd^2 / n_control) + (grupo_experimental$sd^2 / n_experimental)),
    ic_inf = Diferencia - 1.96 * se_diferencia,
    ic_sup = Diferencia + 1.96 * se_diferencia
  )

# --- b) GRÁFICO DE VIOLÍN COMPARATIVO ---
violin_plot <- ggplot(datos_densidad, aes(x = Grupo, y = Promedio, fill = Grupo)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8, fill = "white") +
  geom_point(aes(x = Grupo, y = c(grupo_control$promedio, grupo_experimental$promedio)), 
             color = "red", shape = 18, size = 4) +
  annotate("text", x = 1, y = grupo_control$promedio - 0.3, 
           label = paste("Media:", grupo_control$promedio), color = "red", fontface = "bold") +
  annotate("text", x = 2, y = grupo_experimental$promedio - 0.3, 
           label = paste("Media:", grupo_experimental$promedio), color = "red", fontface = "bold") +
  scale_fill_manual(values = c("Control" = "#E74C3C", "Experimental" = "#27AE60")) +
  labs(
    title = "COMPARACIÓN DE DISTRIBUCIONES - GRUPOS CONTROL vs EXPERIMENTAL",
    subtitle = "Violin plot: densidad de probabilidad + boxplot",
    x = "Grupo",
    y = "Promedio Ponderado"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- c) FOREST PLOT POR ÁREA ACADÉMICA ---
forest_areas <- ggplot(areas_academicas, aes(x = Diferencia, y = reorder(Area, Diferencia))) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0.2, size = 1, color = "#2980B9") +
  geom_point(size = 4, fill = "#2980B9", color = "white", shape = 21, stroke = 1.5) +
  geom_text(aes(label = paste0("+", round(Diferencia, 2), " [", round(ic_inf, 2), ", ", round(ic_sup, 2), "]")),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  scale_x_continuous(limits = c(0, 2.5)) +
  labs(
    title = "IMPACTO DE TUTORÍA POR ÁREA ACADÉMICA",
    subtitle = "Forest plot: diferencia de medias con IC 95%",
    x = "Diferencia de Promedio (Experimental - Control)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank()
  )

# --- e) GRÁFICO DE DENSIDAD SUPERPUESTA ---
densidad_plot <- ggplot(datos_densidad, aes(x = Promedio, fill = Grupo)) +
  geom_density(alpha = 0.6, color = NA) +
  geom_vline(xintercept = grupo_control$promedio, color = "#E74C3C", linetype = "dashed", size = 1) +
  geom_vline(xintercept = grupo_experimental$promedio, color = "#27AE60", linetype = "dashed", size = 1) +
  annotate("rect", xmin = ic_control_inf, xmax = ic_control_sup, ymin = 0, ymax = 0.02,
           fill = "#E74C3C", alpha = 0.3) +
  annotate("rect", xmin = ic_experimental_inf, xmax = ic_experimental_sup, ymin = 0, ymax = 0.02,
           fill = "#27AE60", alpha = 0.3) +
  annotate("text", x = grupo_control$promedio, y = 0.22, 
           label = paste("Control:", grupo_control$promedio), color = "#E74C3C", fontface = "bold") +
  annotate("text", x = grupo_experimental$promedio, y = 0.25, 
           label = paste("Experimental:", grupo_experimental$promedio), color = "#27AE60", fontface = "bold") +
  scale_fill_manual(values = c("Control" = "#E74C3C", "Experimental" = "#27AE60")) +
  labs(
    title = "DENSIDAD SUPERPUESTA DE PROMEDIOS",
    subtitle = "Áreas sombreadas: intervalos de confianza 99%",
    x = "Promedio Ponderado",
    y = "Densidad",
    fill = "Grupo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# --- f) GRÁFICO DE BARRAS AGRUPADAS POR UNIVERSIDAD (TOP 10) ---
# Simular datos para top 10 universidades
set.seed(789)
universidades <- paste0("Universidad ", LETTERS[1:10])

datos_universidades <- data.frame(
  Universidad = rep(universidades, 2),
  Grupo = rep(c("Control", "Experimental"), each = 10)
) %>%
  group_by(Universidad, Grupo) %>%
  mutate(
    Promedio = ifelse(Grupo == "Control",
                      rnorm(1, grupo_control$promedio, grupo_control$sd/3),
                      rnorm(1, grupo_experimental$promedio, grupo_experimental$sd/3)),
    SE = ifelse(Grupo == "Control", grupo_control$sd/10, grupo_experimental$sd/10),
    IC_inf = Promedio - 1.96 * SE,
    IC_sup = Promedio + 1.96 * SE
  )

barras_universidades <- ggplot(datos_universidades, 
                               aes(x = reorder(Universidad, Promedio), y = Promedio, fill = Grupo)) +
  geom_col(position = position_dodge(0.8), alpha = 0.9) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup),
                position = position_dodge(0.8), width = 0.3, size = 0.8) +
  geom_hline(yintercept = grupo_control$promedio, linetype = "dashed", color = "#E74C3C", alpha = 0.7) +
  geom_hline(yintercept = grupo_experimental$promedio, linetype = "dashed", color = "#27AE60", alpha = 0.7) +
  scale_fill_manual(values = c("Control" = "#E74C3C", "Experimental" = "#27AE60")) +
  labs(
    title = "COMPARACIÓN POR UNIVERSIDAD (TOP 10)",
    subtitle = "Barras agrupadas con intervalos de confianza 95%",
    x = "",
    y = "Promedio Ponderado",
    fill = "Grupo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# --- g) SCATTER PLOT BIVARIADO CON ELIPSES ---
# Simular datos para scatter plot
set.seed(321)
datos_scatter <- data.frame(
  Universidad = universidades,
  Promedio_Inicial = runif(10, 12.5, 13.5),
  Mejora = runif(10, 0.8, 1.6)
) %>%
  mutate(
    Promedio_Final = Promedio_Inicial + Mejora,
    Tamaño = sample(500:2000, 10),
    SE_mejora = Mejora * 0.1
  )

scatter_plot <- ggplot(datos_scatter, aes(x = Promedio_Inicial, y = Mejora)) +
  geom_point(aes(size = Tamaño, fill = Mejora), shape = 21, color = "white", stroke = 1) +
  geom_smooth(method = "lm", color = "#2980B9", linetype = "dashed", se = FALSE) +
  geom_text(aes(label = Universidad), vjust = -1.2, size = 3, fontface = "bold") +
  stat_ellipse(level = 0.95, color = "#E74C3C", size = 1, linetype = "dashed") +
  scale_fill_gradient(low = "#F39C12", high = "#27AE60", name = "Mejora") +
  scale_size_continuous(range = c(4, 10), name = "N° Estudiantes") +
  labs(
    title = "RELACIÓN: PROMEDIO INICIAL vs MEJORA",
    subtitle = "Elipse: región de confianza 95% | Tamaño: cantidad de estudiantes",
    x = "Promedio Ponderado Inicial",
    y = "Mejora con Tutoría (puntos)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# --- GRÁFICO ADICIONAL: RESUMEN GLOBAL ---
datos_resumen <- data.frame(
  Grupo = c("Control", "Experimental"),
  Promedio = c(grupo_control$promedio, grupo_experimental$promedio),
  IC_inf = c(ic_control_inf, ic_experimental_inf),
  IC_sup = c(ic_control_sup, ic_experimental_sup)
)

resumen_global <- ggplot(datos_resumen, aes(x = Grupo, y = Promedio, fill = Grupo)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2, size = 1.2) +
  geom_text(aes(label = paste0(round(Promedio, 2), " [", round(IC_inf, 2), ",", round(IC_sup, 2), "]")),
            vjust = -1.5, fontface = "bold", size = 4) +
  annotate("text", x = 1.5, y = max(datos_resumen$IC_sup) + 0.2,
           label = paste("Diferencia:", round(diferencia_global, 2), "puntos"),
           color = "#2980B9", fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Control" = "#E74C3C", "Experimental" = "#27AE60")) +
  labs(
    title = "RESUMEN GLOBAL - IMPACTO DE TUTORÍA VIRTUAL",
    subtitle = "Barras con intervalos de confianza 99%",
    x = "",
    y = "Promedio Ponderado"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# --- COMBINAR TODOS LOS GRÁFICOS ---
cat("Generando visualizaciones completas...\n")

# Diseño con patchwork
(resumen_global | violin_plot) /
  (forest_areas | densidad_plot) /
  (barras_universidades | scatter_plot) +
  plot_annotation(
    title = 'EVALUACIÓN DE IMPACTO - PROGRAMA DE TUTORÍA VIRTUAL SUNEDU',
    subtitle = 'Análisis comparativo entre grupos control y experimental en 30 universidades',
    caption = 'Fuente: SUNEDU - Dirección de Supervisión Universitaria',
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# --- RESUMEN ESTADÍSTICO ---
cat("\nRESUMEN ESTADÍSTICO - IMPACTO DE TUTORÍA VIRTUAL\n")
cat("================================================\n")
cat("Total estudiantes:", grupo_control$n + grupo_experimental$n, "\n")
cat("Universidades participantes:", n_universidades, "\n\n")

cat("RESULTADOS GLOBALES:\n")
cat("Grupo Control: ", grupo_control$promedio, " [", round(ic_control_inf, 2), "-", 
    round(ic_control_sup, 2), "]\n")
cat("Grupo Experimental: ", grupo_experimental$promedio, " [", round(ic_experimental_inf, 2), "-", 
    round(ic_experimental_sup, 2), "]\n")
cat("Diferencia: +", round(diferencia_global, 2), " puntos [", round(ic_diff_inf, 2), "-", 
    round(ic_diff_sup, 2), "]\n\n")

cat("IMPACTO POR ÁREA ACADÉMICA:\n")
for(i in 1:nrow(areas_academicas)) {
  area <- areas_academicas[i, ]
  cat(sprintf("%-20s: +%1.1f puntos\n", as.character(area$Area), area$Diferencia))
}

cat("\nCONCLUSIONES:\n")
cat("============\n")
cat("• La tutoría virtual tiene un impacto positivo y estadísticamente significativo\n")
cat("• La mejora promedio es de +", round(diferencia_global, 2), " puntos\n")
cat("• Ciencias de la Salud muestra el mayor beneficio (+1.5 puntos)\n")
cat("• Todas las áreas académicas muestran mejoras consistentes\n")
cat("• Se recomienda la implementación generalizada del programa\n")