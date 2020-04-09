library(pracma)
library(Matrix)
#Punto 1

#a
n=4

D1<-eye(n, m = n)
D2<-ones(n, m = n)
D3<-zeros(n, m = n)

print("D1")
print(D1)
print("D2")
print(D2)
print("D3")
print(D3)


A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=n, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=n, byrow=TRUE)
print("b")
print(b)

diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

#b
w = 2
D<-diag(diag(A))
L <- tril(A, k=-1)
Qw <- D/w + L
IQw <- solve(Qw)
Transc <- eye(4) - IQw%*%A
Transc
print(norm(Transc,type = c("I")))

#Punto 2

#b
print("Gauss-Seidel:")
tol = 1e-9
sol = itersolve(A, b, x0=c(1,2,1,1), tol=1e-9 , method = "Gauss-Seidel")
print(sol)

#c
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
  cat("Solucion a 5 iteraciones: ",x_k,"\n")
}


x0 = c(1,2,1,1)
jacobiPr(A, b, x0, 5)

#Punto 3 

A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
print("A")
print(A)

#a
charpoly <- function(a, info = FALSE) {
  stopifnot(is.numeric(a), is.matrix(a))
  n <- nrow(a); m <- ncol(a)
  if (n != m || n < 2)
    stop("Argument 'a' must be a square matrix.")
  if (n > 100)
    cat("The algorithm will be *very* slow for n > 100.\n")
  
  p <- rep(1, n+1)
  
  a1 <- a
  for (k in 2:n) {
    p[k] <- -1 * sum(diag(a1))/(k-1)
    if (k == n) a2 <- a1
    a1 <- a %*% (a1 + p[k] * diag(1, n))
  }
  p[n+1] <- -1 * sum(diag(a1))/n
  
  if (info) {
    adet <- (-1)^n * p[n+1]
    if (adet != 0)
      ainv <- -1 * (a2 + p[n] * diag(1, n))/p[n+1]
    else
      ainv = NaN
    
    # determine accuracy of the computation
    e <- a2 %*% a + p[n] *a - adet * diag(1, n)
    e <- max(abs(e))
    cat("Error term:", e, "\n")
  }
  
  if (info) return(list(cp = p, det = adet, inv = ainv))
  else      return(p)
}
poli = charpoly(A, info = FALSE)
print(poli)#en orden de mayor grado


#b y c
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

#d
A = matrix(c(4, -1, -1, -1, -1, 4,
             -1, -1, -1, -1, 4, -1,
             -1, -1, -1, 4), nrow=4, byrow=TRUE)
A
b = c(1, 5, 1.5,-2.33)
b

X <- itersolve(A, b, method = "Jacobi")
print(X)
X <- itersolve(A, b, tol = 1e-9 , method = "Gauss-Seidel")
print(X)
solucion<- solve(A,b)
print(solucion)

#PUNTO 3.3

#a
#Funcion modificada para eliminar la diagonal siempre
tril1 <- function(M, k = 0) {
  if (k == 0) 
  {
    M[upper.tri(M, diag = TRUE)] <- 0
  } 
  else 
  {

    M[col(M)==row(M)] <- 0
  }
  return(M)
}
tril2 <- function(M, k = 0) {
  if (k == 0) 
  {
    M[upper.tri(M)] <- 0
  } 
  else 
  {
    M[col(M) >= row(M) + k + 1] <- 0
  }
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
print(M)
print(tril1(M, k=1))
print(tril2(M,k=1))

#b)
#Funcion para sacar una matriz diagonal dada una matriz
diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
print(M)
print(diag1(M))

#Punto 4

gauss = function(A, b)
{ 
  mult = 0
  n = nrow(A) # = ncol(A) para que sea cuadrada
  
  # matriz ampliada
  Ab = cbind(A,b)
  print(Ab)
  # Eliminación
  for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
    if(Ab[k,k]==0){ # intercambio de fila
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      mult = mult + 2*(ncol(Ab))
    }
  }
  
  # Sustitución hacia atrás-------------------------
  # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
  mult = mult + n+1
  
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    mult = mult + 2*(n-2)
  }
  #cat(x, "\n")
  cat("Numero de multiplicaciones:", mult, "\n")
  return(x)
}

A = matrix(c( 0, 2, 3, 3, 3,
              -5, -4, 1, 4, 5,
              0, 0, 0, 3, 7,
              -4, -7, -8, 9,7,
              3, 4, 5, 5, 6), nrow=5, byrow=TRUE)
b = matrix(c(1,0,0,0,1), nrow=5, byrow=TRUE)
gauss(A,b)
 
#Punto 5 

#a
#Se llega a los valores de alpha y beta por las operaciones de +
# alpha > 1+1 
# beta + 1 < 2
# de acuerdo a su posiscion en  la matrix
beta = 0
alpha = 3

A = matrix(c(2, 0, 1,
             beta,2 , -1,
             -1, 1, alpha), nrow=3, byrow=TRUE)
B = matrix (c(1,2,1),nrow=3, byrow=TRUE)
Ab = cbind(A,B)

print(Ab)

#b y c
library("plot3D")

x = 0
y = 0
z = 0

diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

jacobiPr2 <- function(A,b, x0, tol){
  x_k = matrix(x0)
  
  D = diag1(A)
  L = tril(A,k=-1,diag = FALSE)
  U = triu(A,k=1,diag = FALSE)
  
  it = 1
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    
    x[[it]] = x_k[1]
    y[[it]] = x_k[2]
    z[[it]] = x_k[3]
    cat("Solucion iteracion ",it,": ",x[[it]]," ",y[[it]]," ",z[[it]],"\n")
    it = it + 1
    
    if(it == tol)
      break
  }
  lines3D(x, y, z, colvar = z, col = NULL, add = FALSE, theta = 20, phi = 20)
  cat("Solucion a ", tol ," iteraciones: ",x_k,"\n")
}

x1 = c(1,2,3)
jacobiPr2(A, B, x1, 10)

#Punto 6

A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
print("b")
print(b)

#LU
luA = function(A){ 
  n = nrow(A) 
  LU = A 
  for(j in 1:(n-1)){
    # columna j 
    for(i in (j+1): n){ # filas i > j
      LU[i,j]=LU[i,j]/LU[j,j] # Construye de L 
      for(k in (j+1):n){ # Eliminación filas i >j. Pivote LU[j,j] 
        LU[i,k] = LU[i,k] - LU[i,j]*LU[j,k] # Construye U 
      } 
    } 
  } 
  return(LU) 
} 

LU = luA(A)
LU

#factorizacion QR
gs <- gramSchmidt(A)
(Q <- gs$Q); (R <- gs$R)
print(Q)
print(R)
print(Q %*% R)  

#Punto 7

#a
library(BB)
sistema_no_lineal = function(x) {
  n = length(x)
  F = rep(NA, n)
  F[1] = x[1] - x[2]#x-y
  F[2] = x[1]^2 + x[2]^2 -1#x^2+y^2-1
  F
}
x0 = c(1,1) # n initial starting guess
sol = BBsolve(par=x0, fn=sistema_no_lineal)
sol$par

plot(sol$par)
plot(sistema_no_lineal)

#b
trigexp = function(x) {
  
  #Tamaño del vector que llega por parámetro
  n = length(x)
  #se crea un vector F vacío
  F = rep(NA, n)
  
  #Se enuncian las ecuaciones del sistema
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  #Se crea una secuencia de 2 hasta n-1
  tn1 = 2:(n-1)
  #Se evalúan tn1 ecuaciones
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  #Se evalúa la última ecuación n
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  #Se retorna F
  F
}
n = 10000
p0 = runif(n) #generates n uniform random numbers between 0 and 1.
#se halla la solcuión del sistema trigexp usando BBsolve de la librería BB, utilizando n valores iniciales
sol = BBsolve(par=p0, fn=trigexp)
#Muestra el vector solución del sistema para cada n valores iniciales
sol$par

#Punto 8 

N <- 3

A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
A
x0 <- rep(0, N)
b = c(4,5,6,8)

itersolve(A, b, tol=1e-9 , method = "Gauss-Seidel")

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
print("Matriz transicion Gauss")
print(MatTG)




