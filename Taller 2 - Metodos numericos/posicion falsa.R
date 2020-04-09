# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) ((exp(1)^x) - (pi*x))
F1x <- function(x) ((exp(1)^x)-pi)
# Halla la raiz de Fx
regula <- function(a, b) 
{
  x<-seq(a,b,0.1)
  plot(x,Fx(x),type="l", col="blue")
  abline(h=0,col="blue")
  #x<-b
  #d<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
  error<-1
  cont = 0
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
  while (error > 1.e-8) 
  {
    cont = cont + 1
    x<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) 
    {
      b <- x
    }
    else 
    {
      a <- x
    }
    
    error<-abs(Fx(x)/F1x(x))
    errores[cont] = abs(Fx(x)/F1x(x))
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    cat("X=",x,"\t","E=",error, "iteracion", cont,"\n")
  }
  
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

regula(0,1)
regula(1,2)

