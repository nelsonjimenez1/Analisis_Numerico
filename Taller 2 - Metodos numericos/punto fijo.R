puntofijo = function(g, x0, tol, maxIteraciones)
{
  eje_x = c()
  eje_y = c()
  k = 1 
  cont = 0
  errores = c()
  #iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  
  cat( "\n", formatC ( c( "Iteracion", "resultado", "error"), width = 10, format = "d", flag = " "  ), "\n")
  
  repeat
  {
    
    cont = cont + 1
    x1 = g(x0)
    dx = abs(x1 - x0) 
    errores[cont] = dx/x1
    x0 = x1 
    #Imprimir estado
    
    
    cat( formatC( c( k, x1, dx/x1 ), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )
    
    k = k+1 
    #until 
    if((dx < tol) || (k > maxIteraciones)) break;
  }
  
  # Mensaje de salida 
  if( dx > tol )
  { 
    cat("No hubo convergencia ") 
    
    #return(NULL)
  } 
  else
  { 
    cat("xlo es aproximadamente ", x1, " con error menor que ", tol)
    iter = c(1:cont)
    cont_n = 0;
    cont_e = 0;
    
    repeat
    {
      eje_x[cont_n] = errores[cont_e]
      eje_y[cont_n] = errores[cont_e+1]
      cont_n = cont_n + 1
      cont_e = cont_e + 1;
      
      if (cont_n == cont)
      {
        break;
      }
    }
    
    plot(eje_x, eje_y)
    plot(iter, errores)
  }
}

f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x
g = function(x)  signif(exp(1),8)^x / signif(pi, 8)
y = function(x) log(signif(pi, 8)*x)
par(mar = rep(2, 4))
curve(f, -2, 2); abline( h = 0, v = 0)
puntofijo(g,  0,  1e-8, 100) 
puntofijo(y,  2,  1e-8, 100) 
