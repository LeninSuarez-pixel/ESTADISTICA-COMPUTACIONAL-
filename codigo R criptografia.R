# Vector de contraseñas de ejemplo
pwds <- c("P@ssw0rd!", "clave2025", "12345678", "MiClave#1")

# ==============================
# Normalidad de longitudes
# ==============================

# Convertimos cada contraseña en longitud (número de caracteres) y lo pasamos a bits (cada caracter = 8 bits)
len_bits <- nchar(pwds) * 8

# Prueba de Shapiro-Wilk para ver si las longitudes en bits siguen distribución normal
shapiro.test(len_bits)

# ==============================
# Uniformidad de bits
# ==============================

# Extraemos los bits de cada carácter de las contraseñas
# charToRaw convierte cada caracter en código raw
# rawToBits lo transforma en secuencia de bits (0/1)
# as.integer convierte esos bits a enteros (0 y 1)
# lapply aplica la función a cada contraseña
# unlist junta todos los bits en un vector
bits <- unlist(lapply(pwds,
                      function(p) as.integer(rawToBits(charToRaw(p)))))

# Prueba Chi-cuadrado para verificar si los bits son equiprobables (H0: p=0.5 para 0 y 1)
chisq.test(table(bits), p = c(0.5,0.5))



# ==============================
# CÓDIGO COMPLETO JUNTO
# ==============================
pwds <- c("P@ssw0rd!", "clave2025", "12345678", "MiClave#1")
len_bits <- nchar(pwds) * 8
shapiro.test(len_bits)
bits <- unlist(lapply(pwds, function(p) as.integer(rawToBits(charToRaw(p)))))
chisq.test(table(bits), p = c(0.5,0.5))

