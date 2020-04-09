secante = function(f, x0, x1, tol, maxiter = 100)
{
  f0 = f(x0)
  f1 = f(x1)
  k = 0 
  
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
  iteraciones = 0
  
  while (abs(x1 - x0) > tol && k <= maxiter ) {
    k = k+1
    pendiente = (f1 - f0)/(x1 - x0)
    if (pendiente == 0) return( cero = NA, f.cero = NA, iter = k, ErrorEst = NA)
    x2 = x1 - f1/pendiente
    f2 = f(x2)
    
    errores[k] = abs(x1-x0)/x1
    x0 = x1; f0 = f1
    x1 = x2; f1 = f2
    
    
    # Imprimir iteraciones
    cat(x1, x2, abs(x1-x0)/x1, "\n")
  }
  if (k > maxiter) 
  {
    warning("No se alcanzó el número de iteraciones")
  }
  
  iter = c(1:k)
  cont_n = 0;
  cont_e = 0;
  
  repeat
  {
    eje_x[cont_n] = errores[cont_e]
    eje_y[cont_n] = errores[cont_e+1]
    cont_n = cont_n + 1
    cont_e = cont_e + 1;
    
    if (cont_n == k)
    {
      break;
    }
  }
  
  plot(eje_x, eje_y)
  plot(iter, errores)
  
  return(list(cero=x2, f.cero=f2, iter=k, ErrorEst =abs(x2-x1)))
}

f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x
par(mar = rep(2, 4))
curve(f, -2, 2); abline( h = 0, v = 0)
secante(f, 0, 1, 1e-8, 100)
secante(f, 1, 2, 1e-8, 100)

secante = function (f, x1, x2, num = 1000, eps = 1e-05, eps1 = 1e-05)
{
  i = 0
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
  while ((abs(x1 - x2) > eps) & (i < num)) {
    c = x2 - f(x2) * (x2 - x1)/(f(x2) - f(x1))
    x1 = x2
    x2 = c
    i = i + 1
    errores[i] = abs(x2-x1)/x2
  }
  print(x2)
  print(f(x2))
  if (abs(f(x2)) < eps1) {
    iter = c(1:i)
    cont_n = 0;
    cont_e = 0;
    
    repeat
    {
      eje_x[cont_n] = errores[cont_e]
      eje_y[cont_n] = errores[cont_e+1]
      cont_n = cont_n + 1
      cont_e = cont_e + 1;
      
      if (cont_n == i)
      {
        break;
      }
    }
    
    plot(eje_x, eje_y)
    plot(iter, errores)
    print("finding root is successful")
  }
  else print("finding root is fail")
}


f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x
par(mar = rep(2, 4))
curve(f, -2, 2); abline( h = 0, v = 0)
secante(f, 0, 1)
secante(f, 1, 2)
