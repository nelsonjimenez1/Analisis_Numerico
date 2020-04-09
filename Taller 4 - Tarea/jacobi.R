library(pracma)
library(Matrix)

#Funcion para sacar una matriz diagonal dada una matriz
diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
x0 = c(1,2,1,1)
D = diag1(A)
U = triu(A,k=1,diag = FALSE)
L = tril(A,k=-1,diag = FALSE)


jacobiPr <- function(A,b, x0, tol)
{
  x_k = matrix(x0)
  
  it = 0
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    it = it + 1
    if(it == tol)
      break
  }
  cat("Solución a 5 iteraciones: ",x_k,"\n")
}

jacobiPr(A, b, x0, 5)

