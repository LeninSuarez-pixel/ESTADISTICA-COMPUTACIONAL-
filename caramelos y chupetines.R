# ============================================================================
#  Juego de caramelos y chupetines – versión única y autónoma
#  Regla A: 1 caramelo de cada tipo → 1 chupetín
#  Regla B: 1 chupetín + 1 caramelo cualquiera → 4 caramelos aleatorios
# ============================================================================

simular_juego <- function(objetivo_chupetines = 10,
                          usar_regla_B      = FALSE,
                          max_iter          = 1000) {
  
  # --- estado inicial -------------------------------------------------------
  caramelos  <- c(cafe = 15, naranja = 15, verde = 15)
  chupetines <- 0
  iter       <- 0
  
  # --- bucle principal ------------------------------------------------------
  while (chupetines < objetivo_chupetines && iter < max_iter) {
    
    # Regla A: siempre que haya al menos 1 de cada tipo
    if (all(caramelos >= 1)) {
      caramelos  <- caramelos - 1
      chupetines <- chupetines + 1
      iter       <- iter + 1
      next
    }
    
    # Si no podemos aplicar A, evaluamos B (si está permitida)
    if (!usar_regla_B || chupetines == 0 || sum(caramelos) == 0) {
      stop("No se puede continuar: no hay suficientes recursos.")
    }
    
    # Regla B: entregamos 1 chupetín + 1 caramelo no nulo
    tipo_entregado <- sample(names(caramelos)[caramelos > 0], 1)
    caramelos[tipo_entregado] <- caramelos[tipo_entregado] - 1
    chupetines <- chupetines - 1
    
    # Recibimos 4 caramelos aleatorios (uniforme entre los 3 tipos)
    nuevos <- sample(names(caramelos), 4, replace = TRUE)
    caramelos <- caramelos + table(factor(nuevos, levels = names(caramelos)))
    
    iter <- iter + 1
  }
  
  # --- resultado ------------------------------------------------------------
  list(interacciones  = iter,
       chupetines     = chupetines,
       caramelos_finales = caramelos)
}

# ---------------------------------------------------------------------------
# Ejecución óptima (solo Regla A)
# ---------------------------------------------------------------------------
set.seed(NULL)  # aleatorio
resultado_optimo <- simular_juego(objetivo_chupetines = 10,
                                  usar_regla_B       = FALSE)
print(resultado_optimo)