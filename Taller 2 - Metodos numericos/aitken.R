aitken = function(f, m, x0, tol)
{
  eje_x = c()
  eje_y = c()
  errores = c()
  k =0
  g<-parse(text=f)
  fx = function(x){eval(g[[1]])}
  d.<-D(parse(text=f ), "x")
  df<-function(x) eval(d.)
  
  repeat
  {
    k = k + 1
    x1 = x0 - m*(fx(x0)/df(x0))
    dx = abs(x1-x0)
    error = dx/x1
    errores[k] = error
    cat("X=", x1, "\t", "E=", dx, "iteracion", k,"\n")
    if (dx < tol)
    {
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
      
      break;
    }
    x0 = x1
    
  }
}

f = function(x) ((exp(1)^x) - (pi*x))

aitken("2.7182^x-3.1415*x", 1, 2, 10^-8)

aitken("2.7182^x-3.1415*x", 1, 0, 10^-8)
