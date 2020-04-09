library(deSolve)
library(phaseR)
#tamaño poblacional
N = 1
#estado inicial de los compartimentos
init <- c(S = 0.9,
          I = 0.1,
          R = 0)
#parámetros del modelo (coeficientes de las variables)
param <- c(beta = 1.4247,
           gamma = 0)
#crear la función con las ODE
sir <- function(times, init, param) {
  with(as.list(c(init, param)), {
    #ecuaciones diferenciales   
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
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
head(out, 71)
#gráfica
matplot(x = times, y = out, type = "l",
        xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR básico",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
#añadir leyenda de líneas
legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
       pch = 1, col = 2:4, bty = "n", cex = 1)

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
plot(c(1:70),ysus)

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
plot(c(1:70),yinfec)
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
plot(c(1:70),yrecup)

#-------------------------------

out = matrix(rep(0, times = 70*3), nrow = 70, ncol = 3)
out
times = times[-1]
out[,1] = ysus
out[,2] = yinfec
out[,3] = yrecup
matplot(x = times, y = out, type = "l",
        xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR básico",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
#añadir leyenda de líneas
legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
       pch = 1, col = 2:4, bty = "n", cex = 1)
out

