#punto 1
library(Matrix)
library(pracma)
#b
crearMatrix = function(n)
{
  
  datos = sample(1:20, n*n, replace=T) ## Datos de la matrix aleatorios
  
  A = matrix(datos, nrow = n)
  
  return(A)
  
}

sumarElementosMatriz = function(tamMatrices)
{
  eje_x = c()
  eje_y = c()
  
  
  
  for (m in 1:length(tamMatrices))
  {
    
    n = tamMatrices[m]
    
    eje_y[m] = n*n
    eje_x[m] = n
    
    sum = 0
    
    A = crearMatrix(n)

    
    for (i in 1:n)
    {
      for (j in 1:n)
      {
        sum = sum + A[i,j]
      } 
    }
    
    cat("\nLa suma para la matriz A: \n" )
    #print(A)
    cat("\nes = ", sum)
    
  }
  
  plot(eje_x, eje_y, main = "funcion O(n)", xlab = "Tamaño matriz", ylab = "Iteraciones", type = "o", ylim = c(0,n*n) )
  
}

tamMatrices = c(1:10)

sumarElementosMatriz(tamMatrices)

#a

sumarElementosMatriz = function(tamMatrices, opcion)
{
  eje_x = c()
  eje_y = c()
  
  
  
  for (m in 1:length(tamMatrices))
  {
    
    n = tamMatrices[m]
    
    eje_y[m] = n*n
    eje_x[m] = n
  
    sum = 0
    
    A = crearMatrix(n)
    L = tril(A,k=-1)#triangular inferior
    U = triu(A,k=1)#triangular superior
    
    
    for (i in 1:n)
    {
      for (j in 1:n)
      {
        if (opcion)#triangular inferior
        {
          sum = sum + L[i,j]
        }
        else#triangular superior
        {
          sum = sum + U[i,j]
        }
        
      } 
    }
    
    cat("\nLa suma para la matriz A: \n" )
    #print(A)
    cat("\nes = ", sum)
    
  }
  
  plot(eje_x, eje_y, main = "funcion O(n)", xlab = "Tamaño matriz", ylab = "Iteraciones", type = "o", ylim = c(0,n*n) )
  
}

tamMatrices=c(1:12)
sumarElementosMatriz(tamMatrices, 1)

#En conclusion es O(n^2) porque la grafica es cuadratica
