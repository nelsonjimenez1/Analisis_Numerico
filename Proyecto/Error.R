library(EpiDynamics)
library(deSolve)
#tama�o poblacional
N = 1
#estado inicial de los compartimentos
init <- c(S = 1-1e-6,
          I = 1e-6,
          R = 0)
#par�metros del modelo (coeficientes de las variables)
param <- c(beta = 1.4247,
           gamma = 0.14286)

comprobarSI = function(arrS, arrs, arrI, arri)
{
  sumS = 0
  sumI = 0
  i = 2
  
  repeat
  {
    sumS = sumS + abs(arrS[i]-arrs[i])/arrs[i]
    sumI = sumI + abs(arrI[i]-arri[i])/arri[i]
    i = i + 1
    
    if(i == 72)
    {
      break;
    }
  }
  
  cat("El error en S es ", sumS/70, " y el error en I es ", sumI/70, "\n")
}

comprobarSIR = function(arrS, arrs, arrI, arri, arrR, arrr)
{
  sumS = 0
  sumI = 0
  sumR = 0
  i = 2
  
  repeat
  {
    
    sumS = sumS + abs(arrS[i]-arrs[i])/arrs[i]
    sumI = sumI + abs(arrI[i]-arri[i])/arri[i]
    sumR = sumR + abs(arrR[i]-arrr[i])/arrr[i]
    i = i + 1
    
    if(i == 72)
    {
      break;
    }
  }
  
  cat("El error en S es ", sumS/70, " y el error en I es ", sumI/70, " y el error en R es", sumR/70, "\n")
}

comprobarSIDIF = function(arrS, arrs, arrI, arri)
{
  sumS = 0
  sumI = 0
  i = 2
  j = 1
  
  repeat
  {
    sumS = sumS + abs(arrS[j]-arrs[i])/arrs[i]
    sumI = sumI + abs(arrI[j]-arri[i])/arri[i]
    i = i + 1
    j = j + 1
    
    if(i == 72)
    {
      break;
    }
  }
  
  cat("El error en S es ", sumS/70, " y el error en I es ", sumI/70, "\n")
}

comprobarSIRDIF = function(arrS, arrs, arrI, arri, arrR, arrr)
{
  sumS = 0
  sumI = 0
  sumR = 0
  i = 2
  j = 1
  
  repeat
  {
    
    sumS = sumS + abs(arrS[j]-arrs[i])/arrs[i]
    sumI = sumI + abs(arrI[j]-arri[i])/arri[i]
    sumR = sumR + abs(arrR[j]-arrr[i])/arrr[i]
    i = i + 1
    j = j + 1
    
    if(i == 72)
    {
      break;
    }
  }
  
  cat("El error en S es ", sumS/70, " y el error en I es ", sumI/70, " y el error en R es", sumR/70, "\n")
}

#SI-EDO-Euler
graficar = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gamma * I
           dR <- gamma * I
           
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  head(out, 10)
  
  sir <- SIR(pars = param, init = init, time = 0:70)
  #PlotMods(sir)
  comprobarSI(out$S, sir$results[,2], out$I, sir$results[,3])
}

#SIR-EDO-Euler
graficar2 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gamma * I
           dR <- gamma * I
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  sir <- SIR(pars = param, init = init, time = 0:70)
  #PlotMods(sir)
  comprobarSIR(out$S, sir$results[,2], out$I, sir$results[,3], out$R, sir$results[,4])
}

#SI-EDO-Runge-kutta4
graficar3 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gamma * I
           dR <- gamma * I
           
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
    
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "rk4")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  sir <- SIR(pars = param, init = init, time = 0:70)
  #PlotMods(sir)
  comprobarSI(out$S, sir$results[,2], out$I, sir$results[,3])
}

#SIR-EDO-Adams
graficar4 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gamma * I
           dR <- gamma * I
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  sir <- SIR(pars = param, init = init, time = 0:70)
  #PlotMods(sir)
  comprobarSIR(out$S, sir$results[,2], out$I, sir$results[,3], out$R, sir$results[,4])
}

#SI-EDF
graficar5 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <- beta * S * I - gamma * I
           dR <- gamma * I
           
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #S
  beta = param[1]
  infectados = out$I
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = t*beta*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = t*beta*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[1]
  b[70] = out$S[71]
  
  ysus = solve(A, b)
  num1 = ysus[64]
  num2 = 4.207208e-05
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),ysus)
  
  #I
  beta = param[1]
  alfa = param[2]
  susceptile = out$S
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = -t*beta*susceptile[t+1]+t*alfa
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*beta*susceptile[t+1]+t*alfa
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[2]
  b[70] = out$I[71]
  
  yinfec = solve(A, b)
  num1 = yinfec[66]
  num2 = 1.067680e-04
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),yinfec)
  yinfec
  
  #-------------------------------
  
  out = matrix(rep(0, times = 70*2), nrow = 70, ncol = 2)
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  sir <- SIR(pars = param, init = init, time = 0:70)
  #PlotMods(sir)
  comprobarSIDIF(out[,1], sir$results[,2], out[,2], sir$results[,3])
}

#SIR-EDF
graficar6 = function(init, param, N)
{
  #crear la función con las ODE
  sir <- function(times, init, param) 
  {
    with(as.list(c(init, param)), 
         {
           #ecuaciones diferenciales   
           dS <- beta*-1 * S * I 
           dI <-  beta * S * I - gamma * I
           dR <- gamma * I
           #resultados de las tasas de cambio    
           return(list(c(dS, dI, dR)))
         })
  }
  
  #intervalo de tiempo y resolución
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param)
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #S
  beta = param[1]
  infectados = out$I
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = t*beta*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = t*beta*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[1]
  b[70] = out$S[71]
  
  ysus = solve(A, b)
  num1 = ysus[64]
  num2 = 4.207208e-05
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),ysus)
  
  #I
  beta = param[1]
  alfa = param[2]
  susceptile = out$S
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = -t*beta*susceptile[t+1]+t*alfa
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*beta*susceptile[t+1]+t*alfa
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[2]
  b[70] = out$I[71]
  
  yinfec = solve(A, b)
  num1 = yinfec[66]
  num2 = 1.067680e-04
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),yinfec)
  yinfec
  
  #R
  beta = param[1]
  alfa = param[2]
  infectados = out$I
  A = matrix(rep(0, times = 70^2), nrow = 70, ncol = 70)
  i = 1
  t = 1
  
  A[i,1] = -t*alfa*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*alfa*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==70)
    {
      break;
    }
  }
  
  A[70,70] = 1
  b = c(rep(0,70))
  b[1] = init[3]
  b[70] = out$R[71]
  
  yrecup = solve(A, b)
  num1 = yrecup[60]
  num2 = 0.99970638
  num1
  num2
  abs(num1-num2)/num1
  #plot(c(1:70),yrecup)
  
  #-------------------------------
  
  out = matrix(rep(0, times = 70*3), nrow = 70, ncol = 3)
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  out[,3] = yrecup
  sir <- SIR(pars = param, init = init, time = 0:70)
  #PlotMods(sir)
  comprobarSIRDIF(out[,1], sir$results[,2], out[,2], sir$results[,3], out[,3], sir$results[,4])
}

graficar(init, param, N)
graficar2(init, param, N)
graficar3(init, param, N)
graficar4(init, param, N)
graficar5(init, param, N)
graficar6(init, param, N)