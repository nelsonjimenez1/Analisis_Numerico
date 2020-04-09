library(pracma)
library(Matrix)

diag1 <- function(M) 
{
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

crearMatrix = function()
{
  datos = sample(1:20,36,replace=T) ## DAtos de la matrix aleatorios
  
  A = matrix(datos,nrow = 6,ncol = 6)
  
  while(1/rcond(A) < 1000)
  {
    datos = sample(1:20,36,replace=T) ## DAtos de la matrix aleatorios
    A = matrix(datos,nrow = 6,ncol = 6)
  }
  
  return(A)
}

#1
A = crearMatrix()
A
b = c(1,5,2,3,4,5)

D = diag1(A)
L = tril(A,k=-1,diag = FALSE)#triangular inferior
U = triu(A,k=1,diag = FALSE)#triangular superior
I=diag(1,nrow = nrow(A)) 
T3 = -solve(D)
T4 = T3%*%U
T5= solve(D)
T6 = L%*%T5
T7 = I + T6
T8 = solve(T7)
MatTG = T4%*%T8
normaG = norm(MatTG, type = c( "I"))
print("Convergencia Gauss")
print(normaG)
MatTJ = (-solve(D))%*%(L+U)
normaJ = norm(MatTJ, type = c("I"))
print("Convergencia Jacobi")
print(normaJ)
print("Matriz transicion Gauss")
print(MatTG)
print("Matriz transicion Jacobi")
print (MatTJ)

X <- itersolve(A, b, method = "Jacobi")
print(X)
X <- itersolve(A, b, tol = 1e-9 , method = "Gauss-Seidel")
print(X)
solucion<- solve(A,b)
print(solucion)

#sor
w = 2
Qw <- D/w + L
IQw <- solve(Qw)
Transc <- eye(6) - IQw%*%A
Transc
print(norm(Transc,type = c("I")))


#2
A = matrix(c(8, 9, 2,
             2, 7, 2,
             2, 8, 6), nrow=3, byrow=TRUE)
b = c(69,47,68)

D = diag1(A)
L = tril(A,k=-1,diag = FALSE)#triangular inferior
U = triu(A,k=1,diag = FALSE)#triangular superior

T = (-solve(D))%*%(L+U)
print("T")
print(T)
print("Norma")
print(norm(T,type = c("I")))

