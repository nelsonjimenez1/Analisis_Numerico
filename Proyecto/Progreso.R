#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(deSolve)
library(phaseR)

# Define UI for application that draws a histogram
#-----------
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  sidebarLayout
  (
    
    sidebarPanel
    (
      selectInput("modelo", "Modelo:", choices = c("SIR", "SI", "seleccionar"), selected = "seleccionar" ),
      selectInput("metodologia", "metodologia:", choices = c("EDO", "EDF","seleccionar"), selected = "seleccionar" ),
      
      conditionalPanel
      (
        condition = "input.modelo == 'SI' && input.metodologia == 'EDO'",
        radioButtons(inputId = "metodoSI", "método:", c("Euler", "Adams"), selected = "Euler")
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SIR' && input.metodologia == 'EDO'",
        radioButtons(inputId = "metodoSIR", "método:", c("Euler", "Adams"), selected = "Euler")
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SI'&& input.metodologia != 'seleccionar'",
        numericInput(inputId = "numS", label = "Numero de Suceptibles iniciales", value = 1-1e-6),
        numericInput(inputId = "numI", label = "Numero de Infectados iniciales", value = 1e-6),
        numericInput(inputId = "Betha", label = "Tasa de transmición e infeccion", value = 1.4247)
        #plotOutput("modelSI")
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SIR'&& input.metodologia != 'seleccionar'",
        numericInput(inputId = "nums", label = "Numero de Suceptibles iniciales", value = 1-1e-6),
        numericInput(inputId = "numi", label = "Numero de Infectados iniciales", value = 1e-6),
        numericInput(inputId = "numr", label = "Numero de Recuperados iniciales", value = 0),
        numericInput(inputId = "betha", label = "Tasa de transmición e infeccion", value = 1.4247),
        numericInput(inputId = "gamma", label = "Tasa de recuperacion de la infeccion", value = 0.14286)
        #plotOutput("modelSIR")
      ) 
    ),
    
    mainPanel
    (
      plotOutput("model"),
      tableOutput("datos"),
      plotOutput("campo")
    )
  )
)
#-----------

# Define server logic required to draw a histogram
server = function(input, output)
{
  tablas = NULL
  output$model <- renderPlot({
    #estado inicial de los compartimentos
    #parámetros del modelo (coeficientes de las variables)
    
    if(input$modelo == "SI" && input$metodoSI == "Euler" && input$metodologia == "EDO")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
      N = 1
      
      tablas = graficar(init, param, N)
      
    }
    else if(input$modelo == "SIR" && input$metodoSIR == "Euler" && input$metodologia == "EDO")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
      N = 1
      
      tablas=graficar2(init, param, N)
      
    }
    else if(input$modelo == "SI" && input$metodoSI == "Adams" && input$metodologia == "EDO")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
      N = 1
      
      tablas=graficar3(init, param, N)
    }
    else if(input$modelo == "SIR" && input$metodoSIR == "Adams" && input$metodologia == "EDO")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
      N = 1
      
      
      tablas=graficar4(init, param, N)
      
    }
    else if(input$modelo == "SI" && input$metodologia == "EDF")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
      N = 1
      
      
      tablas=  graficar5(init, param, N)
      
    }
    else if(input$modelo == "SIR" && input$metodologia == "EDF")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
      N = 1
      
      
      tablas=graficar6(init, param, N)
      
    }
    output$datos <- renderTable(tablas)
  })
  
  output$campo <- renderPlot({
  if(input$modelo == "SI" && input$metodologia != "seleccionar")
  {
    
      if(input$metodologia == "EDO")
      {
        colorcito = "red"
      }
      
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
      N = 1
      y=init[2]
      if(input$metodologia == "EDF") colorcito = "black";
      
      apma1 <- function(t, y, param){
        beta = param[1]
        dy <- beta*(N-y)*y
        list(dy)
      } 
      
      apma1.flowField <- flowField(apma1, xlim = c(0, 70), 
                                   ylim    = c(0, 1), parameters = param[1],
                                   points = 9, system = "two.dim", 
                                   add = FALSE, xlab = "time", ylab = "I",col = colorcito, 
                                   main = "Mice Population")
      grid()
    
  }
  })
  
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
  #gráfica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I", main = "Modelo SI básico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  return(out)
##grid()
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
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I.R", main = "Modelo SIR básico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  return(out)
}

#SI-EDO-Adams
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
  out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #gráfica
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I", main = "Modelo SI básico(menor error)",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  return(out)
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
  matplot(x = times, y = out, type = "l", xlab = "Tiempo", ylab = "S.I.R", main = "Modelo SIR básico(menor error)",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de l�?neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  return(out)
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
  out
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  matplot(x = times, y = out, type = "l",
          xlab = "Tiempo", ylab = "S, I", main = "Modelo SI b�sico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #a�adir leyenda de l�neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  
  out$time =times
  return(out)
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
  out
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  out[,3] = yrecup
  matplot(x = times, y = out, type = "l",
          xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR b�sico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #a�adir leyenda de l�neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  
  out$time =times
  return(out)
}

# Run the application 
shinyApp(ui = ui, server = server)