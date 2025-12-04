# -------------------------------------------------------------
# 1) PARÁMETROS DEL PROBLEMA
# -------------------------------------------------------------
N <- 8              # tamaño de la población
n <- 4              # tamaño de la muestra
x_obs <- 2          # valor observado de X (CAMBIAR si corresponde)

# Espacio paramétrico de theta y A
theta_vals <- seq(0, 1, by = 1/8)   # 0, 1/8, ..., 1
A_vals <- N * theta_vals            # 0,1,2,...,8

# -------------------------------------------------------------
# 2) DEFINIR LA PMF DE LA DISTRIBUCIÓN V (CON VALIDACIONES)
#    f(x | A) = [C(A+x-1, x) * C((N-A)+(n-x)-1, n-x)] / C(N+n-1, n)
# -------------------------------------------------------------
pmf_V <- function(A, N, n, x){
  
  # argumentos combinatorios
  a1 <- A + x - 1;   a2 <- x
  b1 <- (N - A) + (n - x) - 1;   b2 <- n - x
  
  # validaciones para evitar combinaciones inválidas
  if(A < 0 || A > N) return(0)
  if(a1 < a2 || b1 < b2) return(0)
  if(a2 < 0 || b2 < 0) return(0)
  
  # combinación válida
  num1 <- choose(a1, a2)
  num2 <- choose(b1, b2)
  den  <- choose(N + n - 1, n)
  
  return((num1 * num2) / den)
}

# -------------------------------------------------------------
# 3) CALCULAR LA FUNCIÓN DE VEROSIMILITUD PARA CADA A
# -------------------------------------------------------------
likelihoods <- sapply(A_vals, pmf_V, N = N, n = n, x = x_obs)

# Tabla con resultados
tabla_resultados <- data.frame(
  Theta = theta_vals,
  A = A_vals,
  Verosimilitud = likelihoods,
  Verosimilitud_Relativa = likelihoods / max(likelihoods)
)

print(tabla_resultados)

# -------------------------------------------------------------
# 4) ESTIMADOR DE MÁXIMA VEROSIMILITUD (EMV / MLE)
# -------------------------------------------------------------
indice_max <- which.max(likelihoods)

EMV_A <- A_vals[indice_max]
EMV_theta <- theta_vals[indice_max]
L_max <- likelihoods[indice_max]

cat("\n====================================================\n")
cat("                 RESULTADO DEL EMV\n")
cat("====================================================\n")
cat("Valor observado: x =", x_obs, "\n")
cat("EMV de A:", EMV_A, "\n")
cat("EMV de theta:", EMV_theta, "\n")
cat("Verosimilitud máxima:", L_max, "\n")
cat("====================================================\n\n")

# -------------------------------------------------------------
# 5) GRÁFICA DE LA FUNCIÓN DE VEROSIMILITUD
# -------------------------------------------------------------
plot(theta_vals, likelihoods, type = "h", lwd = 5, col = "black",
     main = "Función de Verosimilitud L(theta) - Distribución V",
     xlab = expression(theta), ylab = "L(theta)")
points(EMV_theta, L_max, pch = 19, cex = 2, col = "hotpink")
text(EMV_theta, L_max, labels = paste("EMV =", EMV_theta), pos = 3, col = "red")

