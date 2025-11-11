library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(maps)
library(viridis)

# Configuración
set.seed(123)
options(scipen = 999)

# --- DATOS INICIALES ---
cobertura_A <- 68.5
sd_A <- 12.3
n_A <- 13

cobertura_B <- 76.2
sd_B <- 10.8
n_B <- 12

total_regiones <- n_A + n_B

cat("EVALUACIÓN DE EFECTIVIDAD DE CAMPAÑAS DE VACUNACIÓN\n")
cat("===================================================\n")
cat("MINSA -", total_regiones, "regiones del Perú\n\n")

# --- a) INTERVALOS DE CONFIANZA AL 95% ---
t_critico_A <- qt(0.975, df = n_A - 1)
t_critico_B <- qt(0.975, df = n_B - 1)

se_A <- sd_A / sqrt(n_A)
se_B <- sd_B / sqrt(n_B)

ic_A_inf <- cobertura_A - t_critico_A * se_A
ic_A_sup <- cobertura_A + t_critico_A * se_A

ic_B_inf <- cobertura_B - t_critico_B * se_B
ic_B_sup <- cobertura_B + t_critico_B * se_B

cat("a) INTERVALOS DE CONFIANZA AL 95%:\n")
cat("----------------------------------\n")
cat("Estrategia A: [", round(ic_A_inf, 1), "-", round(ic_A_sup, 1), "] %\n")
cat("Estrategia B: [", round(ic_B_inf, 1), "-", round(ic_B_sup, 1), "] %\n\n")

# --- d) INTERVALO DE CONFIANZA PARA DIFERENCIA ---
diferencia <- cobertura_B - cobertura_A
se_diferencia <- sqrt((sd_A^2 / n_A) + (sd_B^2 / n_B))

df_diferencia <- (se_diferencia^4) / 
  (((sd_A^2 / n_A)^2 / (n_A - 1)) + ((sd_B^2 / n_B)^2 / (n_B - 1)))

t_critico_diff <- qt(0.975, df = df_diferencia)

ic_diff_inf <- diferencia - t_critico_diff * se_diferencia
ic_diff_sup <- diferencia + t_critico_diff * se_diferencia

cat("d) DIFERENCIA ENTRE ESTRATEGIAS:\n")
cat("-------------------------------\n")
cat("Diferencia (B - A):", round(diferencia, 1), "%\n")
cat("IC 95% diferencia: [", round(ic_diff_inf, 1), "-", round(ic_diff_sup, 1), "] %\n")
cat("¿Diferencia significativa?", ifelse(ic_diff_inf > 0, "SÍ", "NO"), "\n\n")

# --- b) FOREST PLOT COMPARATIVO ---
datos_forest <- data.frame(
  Estrategia = c("A: Tradicional", "B: Brigadas Móviles", "Diferencia (B - A)"),
  Punto = c(cobertura_A, cobertura_B, diferencia),
  IC_inf = c(ic_A_inf, ic_B_inf, ic_diff_inf),
  IC_sup = c(ic_A_sup, ic_B_sup, ic_diff_sup),
  Tipo = c("Estrategia", "Estrategia", "Diferencia")
)

forest_plot <- ggplot(datos_forest, aes(x = Punto, y = factor(Estrategia, levels = rev(Estrategia)))) +
  geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 0.8) +
  geom_vline(xintercept = seq(60, 80, 5), color = "gray90", linetype = "dotted") +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup, color = Tipo), 
                 height = 0.15, size = 1.8) +
  geom_point(aes(fill = Tipo), size = 5, shape = 21, color = "white", stroke = 1.2) +
  geom_text(aes(label = paste0(round(Punto, 1), "%")), 
            color = "white", fontface = "bold", size = 3.5) +
  geom_text(aes(x = IC_sup + 3, label = paste0("[", round(IC_inf, 1), "-", round(IC_sup, 1), "]")),
            size = 3.2, color = "gray40") +
  scale_color_manual(values = c("#E74C3C", "#2980B9", "#27AE60")) +
  scale_fill_manual(values = c("#E74C3C", "#2980B9", "#27AE60")) +
  scale_x_continuous(limits = c(-5, 90)) +
  labs(
    title = "COMPARACIÓN DE ESTRATEGIAS DE VACUNACIÓN",
    subtitle = "Forest plot con intervalos de confianza al 95%",
    x = "Cobertura de Vacunación (%)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid.major.y = element_blank()
  )

# --- c) GRÁFICO DE BARRAS AGRUPADAS POR REGIÓN ---
regiones_peru <- c(
  "Tumbes", "Piura", "Lambayeque", "La Libertad", "Ancash", "Lima", "Ica", "Arequipa", "Moquegua", "Tacna",
  "Cajamarca", "Amazonas", "San Martín", "Huánuco", "Pasco", "Junín", "Huancavelica", "Ayacucho", "Apurímac", "Cusco", "Puno",
  "Loreto", "Ucayali", "Madre de Dios"
)

macroregion <- c(
  rep("Costa", 10),  # 10 regiones costa
  rep("Sierra", 11), # 11 regiones sierra  
  rep("Selva", 3)    # 3 regiones selva
)

# Simular datos regionales
set.seed(456)
datos_regionales <- data.frame(
  Region = regiones_peru,
  Macroregion = macroregion
) %>%
  group_by(Macroregion) %>%
  mutate(
    Estrategia = sample(rep(c("A", "B"), length.out = n())),
    Cobertura_base = ifelse(Estrategia == "A", cobertura_A, cobertura_B),
    Variabilidad = ifelse(Estrategia == "A", sd_A/2, sd_B/2),
    Cobertura = rnorm(n(), mean = Cobertura_base, sd = Variabilidad),
    Cobertura = pmax(50, pmin(90, Cobertura)),
    n_muestras = sample(5:15, n(), replace = TRUE),
    se_region = Variabilidad / sqrt(n_muestras),
    ic_inf_region = Cobertura - 1.96 * se_region,
    ic_sup_region = Cobertura + 1.96 * se_region
  )

datos_barras <- datos_regionales %>%
  group_by(Macroregion, Estrategia) %>%
  summarise(
    Cobertura_promedio = mean(Cobertura),
    se = sd(Cobertura) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    Estrategia_nombre = ifelse(Estrategia == "A", "Tradicional", "Brigadas Móviles"),
    ic_inf = Cobertura_promedio - 1.96 * se,
    ic_sup = Cobertura_promedio + 1.96 * se
  )

grafico_barras <- ggplot(datos_barras, aes(x = Macroregion, y = Cobertura_promedio, fill = Estrategia_nombre)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.9) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup),
                position = position_dodge(0.8), width = 0.25, size = 0.8) +
  geom_text(aes(label = paste0(round(Cobertura_promedio, 1), "%"), group = Estrategia_nombre),
            position = position_dodge(0.8), vjust = -0.8, fontface = "bold") +
  geom_hline(yintercept = 70, linetype = "dashed", color = "#E74C3C", size = 1) +
  annotate("text", x = 2, y = 72, label = "Meta 70%", color = "#E74C3C", fontface = "bold") +
  scale_fill_manual(values = c("#2980B9", "#E74C3C")) +
  labs(
    title = "COBERTURA POR MACRORREGIÓN Y ESTRATEGIA",
    subtitle = "Barras con intervalos de confianza al 95%",
    x = "Macrorregión",
    y = "Cobertura Promedio (%)",
    fill = "Estrategia"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- e) MAPA DEL PERÚ CON COBERTURAS ---
# Crear datos de coordenadas aproximadas para las regiones
coordenadas_regiones <- data.frame(
  Region = regiones_peru,
  lat = c(
    -3.57, -5.18, -6.70, -8.11, -9.53, -12.04, -14.06, -16.40, -17.20, -18.01,
    -6.61, -5.95, -6.48, -9.93, -10.69, -11.95, -12.79, -13.16, -13.63, -13.52, -15.84,
    -3.75, -8.38, -12.60
  ),
  lon = c(
    -80.45, -80.63, -79.90, -79.04, -77.53, -77.03, -75.73, -71.54, -70.93, -70.25,
    -78.78, -78.17, -76.37, -76.24, -75.27, -75.00, -74.97, -74.22, -72.94, -71.97, -70.02,
    -73.25, -74.53, -70.08
  )
)

datos_mapa <- datos_regionales %>%
  left_join(coordenadas_regiones, by = "Region") %>%
  mutate(
    Categoria = cut(Cobertura, 
                    breaks = c(0, 60, 70, 80, 100),
                    labels = c("<60%", "60-70%", "70-80%", ">80%"),
                    include.lowest = TRUE)
  )

mapa_peru <- ggplot(datos_mapa, aes(x = lon, y = lat)) +
  geom_point(aes(size = Cobertura, color = Categoria, fill = Categoria), 
             alpha = 0.8, shape = 21, stroke = 1) +
  geom_text(aes(label = Region), size = 2.5, hjust = 0.5, vjust = -1.2, check_overlap = TRUE) +
  scale_size_continuous(range = c(3, 10), name = "Cobertura (%)") +
  scale_color_viridis_d(option = "plasma", name = "Categoría") +
  scale_fill_viridis_d(option = "plasma", name = "Categoría") +
  labs(
    title = "MAPA DE COBERTURA DE VACUNACIÓN POR REGIÓN",
    subtitle = "Tamaño y color según porcentaje de cobertura",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# --- f) DOT PLOT ORDENADO CON ICs ---
datos_dot <- datos_regionales %>%
  mutate(
    Region = factor(Region),
    Region = reorder(Region, Cobertura)
  )

dot_plot <- ggplot(datos_dot, aes(x = Cobertura, y = Region)) +
  geom_vline(xintercept = 70, linetype = "dashed", color = "#E74C3C", size = 0.8) +
  geom_errorbarh(aes(xmin = ic_inf_region, xmax = ic_sup_region, color = Estrategia), 
                 height = 0.3, size = 0.8, alpha = 0.7) +
  geom_point(aes(fill = Estrategia, size = Cobertura), shape = 21, color = "white", stroke = 0.8) +
  geom_text(aes(label = paste0(round(Cobertura, 0), "%")), 
            color = "white", size = 2.5, fontface = "bold") +
  scale_fill_manual(values = c("#E74C3C", "#2980B9"), 
                    labels = c("Tradicional", "Brigadas Móviles")) +
  scale_color_manual(values = c("#E74C3C", "#2980B9"), 
                     labels = c("Tradicional", "Brigadas Móviles")) +
  scale_size_continuous(range = c(2, 5)) +
  labs(
    title = "COBERTURA DE VACUNACIÓN POR REGIÓN",
    subtitle = "Ordenado de mayor a menor cobertura con intervalos de confianza",
    x = "Cobertura de Vacunación (%)",
    y = "",
    fill = "Estrategia",
    color = "Estrategia"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "gray90"),
    axis.text.y = element_text(size = 8)
  )

# --- DISEÑO FINAL ---
cat("Generando visualizaciones completas...\n")

# Combinar todos los gráficos
design <- "
AABB
AABB
CCDD
EEFF
EEFF
"

wrap_plots(
  A = forest_plot,
  B = grafico_barras,
  C = mapa_peru,
  D = dot_plot,
  design = design
) +
  plot_annotation(
    title = 'EVALUACIÓN COMPLETA: EFECTIVIDAD DE CAMPAÑAS DE VACUNACIÓN MINSA',
    subtitle = 'Análisis comparativo de estrategias tradicionales vs. brigadas móviles en 25 regiones del Perú',
    caption = 'Fuente: Ministerio de Salud - Dirección General de Intervenciones Estratégicas en Salud Pública',
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# --- RESUMEN ESTADÍSTICO ---
cat("\nRESUMEN ESTADÍSTICO COMPLETO\n")
cat("============================\n")
cat("Estrategia A (Tradicional):\n")
cat("  • Cobertura:", cobertura_A, "%\n")
cat("  • IC 95%: [", round(ic_A_inf, 1), "-", round(ic_A_sup, 1), "] %\n")
cat("  • Regiones:", n_A, "\n\n")

cat("Estrategia B (Brigadas Móviles):\n")
cat("  • Cobertura:", cobertura_B, "%\n")
cat("  • IC 95%: [", round(ic_B_inf, 1), "-", round(ic_B_sup, 1), "] %\n")
cat("  • Regiones:", n_B, "\n\n")

cat("Comparación:\n")
cat("  • Diferencia: +", round(diferencia, 1), "% a favor de Brigadas Móviles\n")
cat("  • IC 95% diferencia: [", round(ic_diff_inf, 1), "-", round(ic_diff_sup, 1), "] %\n")
cat("  • Significancia:", ifelse(ic_diff_inf > 0, "ESTADÍSTICAMENTE SIGNIFICATIVA", "No significativa"), "\n\n")

cat("Distribución por Macrorregión:\n")
cat("  • Costa :", round(mean(datos_regionales$Cobertura[datos_regionales$Macroregion == "Costa"]), 1), "%\n")
cat("  • Sierra:", round(mean(datos_regionales$Cobertura[datos_regionales$Macroregion == "Sierra"]), 1), "%\n")
cat("  • Selva :", round(mean(datos_regionales$Cobertura[datos_regionales$Macroregion == "Selva"]), 1), "%\n")