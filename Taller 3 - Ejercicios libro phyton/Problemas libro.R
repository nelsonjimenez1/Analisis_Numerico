#1
steffensen = function(f, x0, tol, nmax)
{
  i = 0
  errores = c()
  eje_x  =  c()
  eje_y  =  c()
  
  repeat
  {
    i = i + 1
    a = f(x0)*f(x0)
    b = f(x0+f(x0))-f(x0)
    x1 = x0 - (a/b)
    error = (abs(x1-x0))/x1
    errores[i] = error
    temp = f(x0)
    x0 = x1
    
    cat("X=", x0,"\t","E=", error, "iteracion", i,"\n")
    
    if(abs(temp) <= tol)
    {
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
      return (x0)
      break;   
    }
  }
}

f = function(x) 2*x^2-10*x+5
x = steffensen(f, 0.5, 10^-8, 10)
y = steffensen(f, 5, 10^-8, 10)
suma = x + y
suma2 = x^2 + y^2
cat("x", suma, suma2)

#13
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

newtonraphson("x^3-4", 2, 0.00000005, 10)

#15
riemman = function(f, a, b, n)
{
    i = 1
    delta = (b-a)/n
    x = a + i*delta
    area = 0
    
    repeat
    {
        area = area + f(x)*delta
        i = i + 1
        if (i == n) break;
        x = a + i*delta
    }
    
    #cat ("area" , area, "\n")
    return (area)
}


ejercicio = function(fun, i, f, tol, avance, n)
{
    sumatoria = 0
    x0 = riemman(fun, i, f, n)
    sumatoria =  sumatoria + x0
    cat ("area" , sumatoria, "\n")
    i = f
    f =  f + avance
    
    repeat
    {
        
        x1 = riemman(fun, i, f, n)
        sumatoria = sumatoria + x1
        cat ("area" , sumatoria, "\n")
        error = abs(x1-x0)
        i = f
        f = f + avance
        x0 = x1
        
        if (sumatoria >  2 | error < tol)
        {
          return (sumatoria)
        }
        
    }
}

f = function(x) 5-(exp(1)^x)
ejercicio(f, 0, 0.1, 10^-8, 0.1, 10000)

#27
polar <- function (theta, r, color=4){
  y <- 0
  x <- 0
  ejex <- 1
  
  for (i in 1:length(r)){
    if(is.nan(r[i])== T){
      r[i] <- 0
    }
  }
  
  dim <- seq(0, 2*pi, by=pi/300) 
  angulo <- seq(-max(dim),max(dim),by=dim[2]-dim[1])
  y <- r*sin(theta)
  x <- r*cos(theta)
  plot.new()
  plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)
  
  aux <- max(r)
  # Dibuja los ejes.
  while (aux > 0){
    fi <- aux*sin(angulo)
    cir <- aux*cos(angulo)
    points(cir,fi,pch="-",col="gray",cex=0.3)
    text(ejex+0.2,-0.2,ejex,col="gray")
    ejex <- ejex + 1
    aux <- aux - 1
  }
  
  abline(v=((max(cir)+min(cir))/2),col="gray")
  abline(h=((max(cir)+min(cir))/2),col="gray")
  segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
  segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
  
  points(x,y,pch=20,col=color,cex=1)
  
}

newton = function(fun, x0, tol = 0.000000005, maxiter = 100)
{
  dim <- seq(0, 2*pi, by=pi/300) 
  r=3*sin(dim)^3-1
  r2=4*sin(dim)*cos(dim)
  polar(dim,r,"blue")
  par(new=TRUE)
  polar(dim,r2,"red")
  title(main="Gráficas de las Funciones Polares (Originales)")
  
  points(0.5,1.29, col = "green", pch = 20)
  
  dim <- seq(0, pi/2, by=pi/300) 
  r=3*sin(dim)^3-1
  r2=4*sin(dim)*cos(dim)
  polar(dim,r,"blue")
  par(new=TRUE)
  polar(dim,r2,"red")
  title(main="Gráficas de las Funciones Polares en intervalo theta = [0, pi/2]")
  
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

newton("3*sin(x)^3-1-4*sin(x)*cos(x)", pi/2, 1e-5, 100)



