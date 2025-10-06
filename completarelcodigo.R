library(stats)
library(nortest)
library(car)

ph_agua <- c(8.2, 8.5, 7.9, 8.3, 7.8, 8.6, 8.1, 8.4, 7.7, 8.7,
             8.3, 8.0, 8.8, 7.6, 8.5, 8.2, 8.1, 8.6, 7.9, 8.4,
             8.5, 8.0, 8.3, 8.2, 8.1)

oxigeno_disuelto <- c(7.2, 8.1, 6.5, 7.8, 6.2, 8.5, 7.0, 7.9, 6.8, 8.3,
                      7.5, 7.1, 8.7, 6.4, 8.0, 7.3, 7.2, 8.4, 6.9, 7.8,
                      8.2, 7.0, 7.6, 7.4, 7.3)

temperatura_agua <- c(14.2, 15.8, 12.5, 16.1, 11.8, 17.2, 13.9, 15.5, 12.3, 16.8,
                      15.1, 13.7, 17.5, 11.5, 16.3, 14.6, 14.2, 17.0, 12.8, 15.9,
                      16.5, 13.2, 15.4, 14.8, 14.5)

zona_muestreo <- factor(c("Norte", "Sur", "Centro", "Norte",
                          "Centro", "Sur", "Norte", "Centro",
                          "Sur", "Norte", "Centro", "Norte",
                          "Sur", "Centro", "Norte", "Centro",
                          "Sur", "Norte", "Centro", "Sur",
                          "Norte", "Centro", "Sur", "Norte", "Centro"))

datos_agua <- data.frame(
  ph = ph_agua,
  oxigeno = oxigeno_disuelto,
  temperatura = temperatura_agua,
  zona = zona_muestreo
)

ingresos_mensuales <- c(850, 920, 750, 1100, 890, 980, 820,
                        1050, 870, 940, 960, 810, 1080, 780,
                        1020, 900, 950, 830, 1120, 800,
                        1060, 880, 930, 820, 1140, 770, 1000,
                        890, 960, 840, 1090, 790, 1030, 910, 940,
                        850, 1110, 760, 1010, 920,
                        980, 860, 1070, 810, 1040, 930, 890, 1000,
                        820, 1130, 780, 960, 900, 1050, 840, 1080,
                        790, 980, 920, 950,870, 1100, 750, 1010, 880,
                        960, 830, 1090, 800, 1040, 930, 910, 980, 840,
                        1120, 790, 1020, 890, 960, 850,
                        1080, 820, 1010, 920, 950, 860, 1110, 770, 980,
                        900, 1060, 840, 1090, 810, 960, 930, 880,
                        1050, 820, 1130)

anos_experiencia <- c(15, 20, 12, 25, 18, 22, 14, 24, 17, 21, 19,
                      13, 26, 11, 23, 16, 20, 14, 27, 12,
                      24, 17, 21, 13, 28, 10, 22, 18, 20, 15,
                      25, 12, 23, 16, 19, 14, 26, 9, 21, 17,
                      22, 15, 24, 13, 23, 19, 17, 22, 14, 27,
                      11, 20, 16, 24, 15, 25, 12, 22, 18, 20,
                      16, 26, 10, 21, 17, 20, 14, 25, 13, 23,
                      19, 16, 22, 15, 27, 12, 23, 17, 20, 14,
                      25, 13, 21, 18, 20, 15, 26, 11, 22, 16,
                      24, 15, 25, 13, 20, 19, 17, 24, 14, 28)

captura_semanal_kg <- c(45, 52, 38, 58, 42, 55, 40, 56, 44,
                        53, 51, 39, 59, 36, 54, 46, 50, 41, 60, 37,
                        57, 43, 52, 40, 61, 35, 54, 45, 50,
                        42, 58, 38, 55, 47, 51, 43, 59, 34, 53, 48,
                        55, 44, 57, 40, 56, 50, 45, 54, 41,
                        60, 37, 51, 46, 56, 42, 58, 38, 52, 47, 50,
                        44, 59, 35, 53, 45, 50, 41, 57, 39,
                        56, 50, 46, 54, 42, 60, 38, 55, 45, 50, 43,
                        58, 40, 53, 47, 51, 44, 59, 36, 52,
                        46, 57, 42, 58, 40, 50, 49, 45, 56, 41, 61)

tipo_embarcacion <- factor(c(rep("Tradicional", 40),
                             rep("Motor", 35),
                             rep("Vela", 25)))

datos_pescadores <- data.frame(
  ingresos = ingresos_mensuales,
  experiencia = anos_experiencia,
  captura = captura_semanal_kg,
  embarcacion = tipo_embarcacion
)

tabla_zona <- table(datos_agua$zona)
tabla_embarcacion <- table(datos_pescadores$embarcacion)

evaluar_normalidad <- function(variable, nombre, n) {
  cat("\n", nombre, " (n=", n, "):\n", sep="")
  
  if(n <= 50) {
    shapiro_result <- shapiro.test(variable)
    if(shapiro_result$p.value > 0.05) {
      # Completar
      print("Distribución normal (Shapiro-Wilk)")
      return(TRUE)
    } else {
      # Completar
      print("Distribución no normal (Shapiro-Wilk)")
      return(FALSE)
    }
  } else {
    ks_result <- ks.test(variable, "pnorm", mean(variable), sd(variable))
    if(ks_result$p.value > 0.05) {
      # Completar
      print("Distribución normal (Kolmogorov-Smirnov)")
      return(TRUE)
    } else {
      # Completar
      print("Distribución no normal (Kolmogorov-Smirnov)")
      return(FALSE)
    }
  }
}

ph_normal <- evaluar_normalidad(datos_agua$ph, "pH", 25)
oxigeno_normal <- evaluar_normalidad(datos_agua$oxigeno, "Oxígeno disuelto", 25)
temp_normal <- evaluar_normalidad(datos_agua$temperatura, "Temperatura", 25)
ingresos_normal <- evaluar_normalidad(datos_pescadores$ingresos, "Ingresos mensuales", 100)
exp_normal <- evaluar_normalidad(datos_pescadores$experiencia, "Años de experiencia", 100)
captura_normal <- evaluar_normalidad(datos_pescadores$captura, "Captura semanal", 100)

datos_comparacion <- datos_pescadores[
  datos_pescadores$embarcacion %in% c("Tradicional", "Motor"), ]

datos_comparacion$embarcacion <- droplevels(datos_comparacion$embarcacion)

levene_test <- car::leveneTest(ingresos ~ embarcacion, data = datos_comparacion)
trad_stats <- datos_comparacion[datos_comparacion$embarcacion == "Tradicional", "ingresos"]
motor_stats <- datos_comparacion[datos_comparacion$embarcacion == "Motor", "ingresos"]
t_test_ingresos <- t.test(ingresos ~ embarcacion,
                          data = datos_comparacion,
                          var.equal = var_equal)

if(t_test_ingresos$p.value < 0.05) {
  diferencia <- mean(motor_stats) - mean(trad_stats)
  if(diferencia > 0) {
    #Completar
    print("Ingresos en Motor son significativamente mayores")
  } else {
    #Completar
    print("Ingresos en Tradicional son significativamente mayores")
  }
} else {
  #Completar
  print("No hay diferencia significativa en ingresos entre embarcaciones")
}

wilcox_test <- wilcox.test(datos_agua$oxigeno, mu = 7.5)

mann_whitney <- wilcox.test(ingresos ~ embarcacion,
                            data = datos_comparacion)

kruskal_test <- kruskal.test(captura ~ embarcacion,
                             data = datos_pescadores)

spearman_cor <- cor.test(datos_pescadores$ingresos,
                         datos_pescadores$captura,
                         method = "spearman")

#Completar
print("Interpretación final: Se aplicaron pruebas de normalidad, comparación de medias y correlación")
