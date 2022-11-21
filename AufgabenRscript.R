getwd()
messungen <- read.csv("messungen.csv")

#Definiere freie Parameter
lambda <- 1
sigma <- 1
t.Strich <- seq(from = 0, to = 5, length.out =50)


#Definiere Hilfsfunktionen zur Erstellung der Sigma-Matrix
mat.t <- matrix(messungen$t, ncol = 50, nrow = 50)
mat.t.Strich <- matrix(t.Strich, ncol = 50, nrow = 50)


matK <- function(mat1 = mat.t,
                 mat2 = mat.t.Strich,
                 lambda = 1,
                 sigma = 1) {
  if ((mat1 == mat.t && mat2 == mat.t)) {
    mat2 <- t(mat2)
    exp(-(abs(mat1 - mat2) ^ 2) / lambda) + diag(50) * sigma
  }
  else {
    mat2 <- t(mat2)
    exp(-(abs(mat1 - mat2) ^ 2) / lambda)
  }
}
  
  
# #Erstelle Matrizen t und t`
# mat.t <- t(matrix(t, ncol = 50, nrow = 50))
# mat.t.Strich <- t(matrix(t.Strich, ncol = 50, nrow = 50))

#Zusammensetzen der Matrix Sigma
matSigma.a <-
  cbind(matK(mat2 = mat.t), matK())
matSigma.b <-
  cbind(t(matK()), matK(mat1 = mat.t.Strich))
        
matSigma.total <- rbind(matSigma.a, matSigma.b)


# Bedingter Erwartungswert mü2.1

mü2.1 <- t(matK()) %*% solve(matK(mat2 = mat.t)) %*% messungen$y

