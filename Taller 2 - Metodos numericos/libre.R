p <- function(x){return(2*x^2 -0.9*x -1) } # Declaramos el objeto.

# Para determinar los ceros de la función hay que establecer un intervalo de búsqueda:
uniroot(p, interval=c(-1,0))

f<-function(x){((exp(1)^x) - (pi*x))};f1<-function(x){((exp(1)^x) - (pi))};
NDHfzero(f,f1,1)
##---- Should be DIRECTLY executable !! ----
##-- ==> Define data, use random,
##--or do help(data=index) for the standard data sets.
## The function is currently defined as
libre = function (f, f1, x0 = 0, num = 1000, eps = 1e-05, eps1 = 1e-05)
{
  a = x0
  b = a - f(a)/f1(a)
  i = 0
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
  while ((abs(b - a) > eps)) {
    c = 1
    j = 0
    while (abs(f(b)) >= abs(f(a))) {
      b = a - c * f(a)/f1(a)
      j = j + 1
      c = 1/(2^j)
    }
    a = b
    b = a - f(a)/f1(a)
    c = 1
    j = 0
    while (abs(f(b)) >= abs(f(a))) {
      b = a - c * f(a)/f1(a)
      j = j + 1
      c = 1/(2^j)
    }
    
    i = i + 1
    cat("X=",b,"\t","E=", abs(b-a)/b, "iteracion", i,"\n")
    errores[i] = abs(b-a)/b
  }
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
  
  print(b)
  print(f(b))
  if (abs(f(b)) < eps1) {
    print("finding root is successful")
    
  }
  else print("finding root is fail")
}

libre(f, f1, 1)
libre(f, f1, 2)
