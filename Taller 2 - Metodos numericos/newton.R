newtonraphson = function(fun, x0, tol = 0.000000005, maxiter = 100)
{ 
  eje_x = c()
  eje_y = c()
  errores = c()
  cont = 0
  # f = string
  numiter = 0 
  g = parse(text=fun) # parse devuelve tipo "expression"
  g. = D(g,"x") 
  fx = function(x){eval(g)} # convertir f a función
  fp = function(x){eval(g.)} # convertir f' a función
  
  correccion = -fx(x0)/fp(x0)
  cat( "\n", formatC ( c( "Iteracion", "Cero", "f(cero)", "error"), width = 10, format = "d", flag = " "  ), "\n")
  while (abs(correccion) >= tol && numiter <= maxiter)
  {
    cont = cont + 1
    numiter = numiter + 1 
    if (fp(x0) == 0) stop("División por cero")
    x1 = x0 + correccion 
    errores[cont] = abs((x1-x0))/x1
    correccion = -fx(x1)/fp(x1)
    cat( formatC( c(numiter, x0, fx(x0), errores[cont] ), digits = 15, width = -15, format = "f", flag = "  "  ), "\n" )
    x0 = x1 
  }
  
  if (numiter > maxiter)
  {
    warning("Se alcanzó el máximo número de iteraciones.")
    cat("Estado:\n")
    cat("k = ", k, "x = ", x1, " f(x) = ", f(x1), "Error estimado <= ", correccion)
  }
  else 
  {
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
    
    
    return(list(cero = x0, f.cero = fx(x0), numeroiter=numiter, error.est = correccion)) 
    
  }
}

f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x
par(mar = rep(2, 4))
curve(f, -2, 2); abline( h = 0, v = 0)
newtonraphson("2.7182^x-3.1415*x", 0, 0.00000005, 10)
newtonraphson("2.7182^x-3.1415*x", 2, 0.00000005, 10)

#problema de numeros grandes
#biseccion(a, -3.1*10^11, 2.8*10^10, 10^-8)
#puntofijo(b,  -3.1*10^11,  1e-8, 100) 
newtonraphson("x^2+(9^12)*x-3", -3.1*10^11, 0.00000005, 10)
newtonraphson("x^2+(9^12)*x-3", 2.8*10^10, 0.00000005, 10)

xx =(-2*-3)/(9^12+sqrt(9^24+12))
yy =(-2*-3)/(9^12-sqrt(9^24+12))
cat(xx)
cat(yy)
