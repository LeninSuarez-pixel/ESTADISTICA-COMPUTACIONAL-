# Librerías necesarias
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggpubr)) install.packages("ggpubr")

library(tidyverse)
library(ggpubr)

set.seed(123)

# Datos simulados: dos grupos de estudiantes
datos <- data.frame(
  grupo = rep(c("Ciencias", "Humanidades"), each = 15),
  horas_estudio = c(
    5.1,4.8,6.0,5.4,4.9,5.2,4.7,5.5,6.1,4.3,5.8,4.5,5.0,5.6,4.2,
    3.8,4.5,5.1,4.0,6.2,3.5,4.8,5.5,3.0,4.2,5.8,4.7,3.9,5.0,4.1
  ),
  rendimiento = c(
    7.5,8.0,7.2,8.5,7.8,7.0,8.2,7.9,6.8,8.1,7.4,8.3,7.6,6.9,8.4,
    6.5,7.0,8.1,6.8,7.5,6.0,7.2,8.0,5.8,7.1,8.2,6.9,6.2,7.8,6.5
  )
)

# Estadísticos básicos
resumen <- datos %>%
  group_by(grupo) %>%
  summarise(
    media_horas = mean(horas_estudio),
    sd_horas = sd(horas_estudio),
    media_rend = mean(rendimiento),
    sd_rend = sd(rendimiento),
    cv_horas = sd(horas_estudio)/mean(horas_estudio)*100,
    cv_rend = sd(rendimiento)/mean(rendimiento)*100
  )

# Prueba t y correlación
t_test <- t.test(horas_estudio ~ grupo, data = datos)
cor_test <- cor.test(datos$horas_estudio, datos$rendimiento)

# Boxplot de horas de estudio
g1 <- ggplot(datos, aes(x = grupo, y = horas_estudio, fill = grupo)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(title="Horas de estudio por grupo", y="Horas diarias", x="") +
  theme_minimal()

# Dispersión horas vs rendimiento
g2 <- ggplot(datos, aes(x = horas_estudio, y = rendimiento, color = grupo)) +
  geom_point(size=3) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Relación entre horas de estudio y rendimiento",
       x="Horas de estudio", y="Rendimiento") +
  theme_minimal()

# Resultados en pantalla
print(resumen)
print(t_test)
print(cor_test)

# Gráficas
print(g1)
print(g2)

# Guardar archivos
ggsave("boxplot_horas.png", g1, width=7, height=5)
ggsave("dispersion.png", g2, width=7, height=5)
write.csv(datos, "datos_estudio.csv", row.names = FALSE)

