getwd()
messungen <- read.csv("messungen.csv")


#Anlegen der Funktionen und Matrizen

# epsilon <- numeric(length = 50)
# matK <- function(t,s){
#   empty.mat <- matrix(nrow = 50, ncol = 50)
#   k*(abs(t-s))
# }



# Funktion: Kernfunktion k
# Inputs: 
#   - s: Standardabweichung von fDach?
#   - lambda: Varianz von f, mit default = 1
# 
# ```{r}
#t <- messungen$t  #Vektor mit Messzeitpunkten, nicht zufällig
#s <- sqrt(sum((messungen$y - mean(messungen$y))^2) / 50)  #SD der Messungen

#Definiere freie Parameter
lambda <- 1
sigma <- 1
t.Strich <- seq(from = 0, to = 5, length.out =50)


mat.t <- matrix(messungen$t, ncol = 50, nrow = 50)
mat.t.Strich <- matrix(t.Strich, ncol = 50, nrow = 50)


matK <- function(mat1 = mat.t,
                 mat2 = mat.t.Strich,
                 lambda = 1,
                 sigma = 1) {
  mat2 <- t(mat2)
  if (all(mat1 == mat2)) {
    exp(-(abs(mat.t - mat.t.Strich) ^ 2) / lambda) + diag(50) * sigma
  }
  else {
    exp(-(abs(mat.t - mat.t.Strich) ^ 2) / lambda)
  }
}
  
  
#Erstelle Matrizen t und t`
mat.t <- t(matrix(t, ncol = 50, nrow = 50))
mat.t.Strich <- t(matrix(t.Strich, ncol = 50, nrow = 50))

matSigma.a <-
  cbind(matK(mat2 = mat.t), matK())
matSigma.b <-
  cbind(t(matK()), matK(mat1 = mat.t.Strich))
        
matSigma.total <- rbind(matSigma.a, matSigma.b)


# Bedingter Erwartungswert mü2.1