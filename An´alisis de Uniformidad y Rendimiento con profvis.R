# ============================================================================
#  Generadores de números aleatorios
#  - LCG (Linear Congruential Generator)
#  - Mersenne Twister (envoltorio sobre runif)
#  - Xorshift32
# ============================================================================

# 1) LCG ----------------------------------------------------------
lcg_generator <- function(n, seed = 12345,
                          a = 1664525,
                          c = 1013904223,
                          m = 2^32) {
  x <- numeric(n)
  x[1] <- seed
  for (i in 2:n) {
    x[i] <- (a * x[i - 1] + c) %% m
  }
  return(x / m)   # normalizado a (0,1)
}

# 2) Mersenne Twister ---------------------------------------------
mt_generator <- function(n, seed = 12345) {
  set.seed(seed)
  runif(n)
}

# 3) Xorshift32 ---------------------------------------------------
xorshift_generator <- function(n, seed = 12345) {
  x <- numeric(n)
  state <- seed
  for (i in 1:n) {
    state <- bitwXor(state, bitwShiftL(state, 13))
    state <- bitwXor(state, bitwShiftR(state, 17))
    state <- bitwXor(state, bitwShiftL(state, 5))
    x[i] <- state
  }
  return(abs(x) / max(abs(x)))   # normalizado a (0,1)
}

# -----------------------------------------------------------------
# Ejemplo de uso:
#   vec_lcg  <- lcg_generator(10000)
#   vec_mt   <- mt_generator(10000)
#   vec_xors <- xorshift_generator(10000)
# -----------------------------------------------------------------

