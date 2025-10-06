# ==============================
# 1. Tests para tendencia central – una muestra
# ==============================

# 1.1 One-sample t-test
# Pregunta: ¿El tiempo medio de espera en fila de un banco es distinto de 5 min?
# Se compara la media muestral con un valor hipotético (mu = 5).
tiempo <- c(4.8 ,5.2 ,5.0 ,4.7 ,5.3 ,5.1 ,4.9 ,5.0)
t.test(tiempo , mu = 5)

# 1.2 One-sample z-test
# Pregunta: ¿La media de llenado de botellas de 500 mL es 500 mL?
# Se usa z.test porque la varianza poblacional (σ²) es conocida (0.25).
volumen <- c(499.8 ,500.2 ,499.9 ,500.1 ,500.0 ,499.7 ,500.3 ,500.0)
BSDA::z.test(volumen , sigma.x = sqrt (0.25) , mu = 500)

# 1.3 Wilcoxon signed-rank test
# Pregunta: ¿La mediana de colesterol LDL difiere de 100 mg/dL?
# Se usa Wilcoxon para prueba no paramétrica de la mediana.
ldl <- c(98 ,102 ,105 ,97 ,99 ,104 ,106 ,95)
wilcox.test(ldl , mu = 100)

# 1.4 Sign test (binomial test)
# Pregunta: ¿Más del 50% de los estudiantes aprueban un examen?
# Se prueba la proporción de aprobados frente a p = 0.5.
aprobado <- c(1,1,0,1,1,1,0,1,1,1,1,0)
binom.test(sum(aprobado), length(aprobado), p = 0.5, alt = "greater")


# ==============================
# 2. Tests comparando dos grupos independientes
# ==============================

# 2.1 Independent-samples t-test (varianzas iguales)
# Pregunta: ¿Las calificaciones difieren entre dos secciones?
nota <- c(78,82,80, 72 ,75 ,74)
seccion <- factor(rep(c("A","B"), each = 3))
t.test(nota ~ seccion , var.equal = TRUE)

# 2.2 Welch t-test (varianzas no iguales)
# Pregunta: ¿Difieren los ingresos mensuales entre hombres y mujeres?
ingreso <- c(1200 ,1350 ,1280 , 1100 ,1050 ,1080)
genero <- factor(rep(c("M","F"), each = 3))
t.test(ingreso ~ genero , var.equal = FALSE)

# 2.3 Mann–Whitney U test
# Pregunta: ¿La satisfacción laboral difiere entre turnos?
# Prueba no paramétrica (U de Mann-Whitney).
satisf <- c(8,7,9, 5,6,4)
turno <- factor(rep(c("Dia","Noche"), each = 3))
wilcox.test(satisf ~ turno)


# ==============================
# 3. Tests comparando dos grupos pareados
# ==============================

# 3.1 Paired t-test
# Pregunta: ¿Disminuye la presión arterial sistólica tras dieta baja en sal?
pas_antes <- c(140 ,138 ,142 ,139 ,141)
pas_despues <- c(135 ,132 ,136 ,134 ,133)
t.test(pas_antes , pas_despues , paired = TRUE)

# 3.2 Wilcoxon signed-rank test (pareado)
# Pregunta: ¿Mejora la puntuación de dolor tras fisioterapia?
dolor_pre <- c(8,7,9,8,7)
dolor_post <- c(5,4,6,5,4)
wilcox.test(dolor_pre , dolor_post , paired = TRUE)


# ==============================
# 4. Tests comparando tres o más grupos
# ==============================

# 4.1 One-way ANOVA
# Pregunta: ¿Difieren las alturas de plantas con tres fertilizantes?
altura <- c(20,21,19, 24,23,25, 18 ,17 ,19)
fertil <- factor(rep(c("A","B","C"), each = 3))
model <- aov(altura ~ fertil)        # ANOVA
summary(model)                       # Resumen ANOVA
TukeyHSD(model)                      # Comparación múltiple post-hoc

# 4.2 Kruskal-Wallis
# Pregunta: ¿Difieren los tiempos de respuesta entre tres interfaces?
tiempo_ms <- c(150 ,160 ,155 , 170,175 ,168 , 140 ,145 ,142)
interfaz <- factor(rep(c("I1","I2","I3"), each = 3))
kruskal.test(tiempo_ms ~ interfaz)

# 4.3 RM-ANOVA (medidas repetidas)
# Pregunta: ¿Cambia la concentración antes, durante y después de una tarea?
# Se transforma a formato largo y se aplica ANOVA de medidas repetidas.
library(tidyr)
pre <- c(7,6,8,7); dur <- c(5,4,6,5); post <- c(6,5,7,6)
datos <- data.frame(id = factor (1:4), pre , dur , post)
datos_long <- pivot_longer(datos , -id , names_to ="tiempo", values_to ="punt")
summary(aov(punt ~ tiempo + Error(id/tiempo), data = datos_long))

# 4.4 Friedman test (no paramétrico para medidas repetidas)
# Pregunta: ¿Varía la calidad del sueño en tres semanas consecutivas?
sem1 <- c(3,4,3,4); sem2 <- c(4,4,3,5); sem3 <- c(2,3,2,3)
friedman.test(cbind(sem1 , sem2 , sem3))


# ==============================
# 5. Tests para la varianza
# ==============================

# 5.1 F-test
# Pregunta: ¿Las varianzas de los tiempos de entrega son iguales entre rutas?
tiempo <- c(15,14,16, 20 ,22 ,21)
ruta <- factor(rep(c("A","B"), each = 3))
var.test(tiempo ~ ruta)

# 5.2 Levene test
# Pregunta: ¿Las varianzas de peso perdido son iguales bajo tres dietas?
library(car)
peso_perdido <- c(4,5,4, 6,7,6, 3,2,3)
dieta <- factor(rep(c("D1","D2","D3"), each = 3))
leveneTest(peso_perdido ~ dieta)

# 5.3 Bartlett test
# Pregunta: ¿Las varianzas de resistencia son iguales en cuatro aleaciones?
resist <- c(12,13, 11,10, 14,15, 13 ,12)
aleacion <- factor(rep(paste0("Al" ,1:4), each = 2))
bartlett.test(resist ~ aleacion)
