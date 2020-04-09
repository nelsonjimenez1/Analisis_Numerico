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
  titlePanel("Modelos Epidemologicos SI-R para el analisis de virus informaticos."),
  
  sidebarLayout
  (
    
    sidebarPanel
    (
      selectInput("modelo", "Modelo:", choices = c("SIR", "SI", "seleccionar"), selected = "seleccionar" ),
      selectInput("metodologia", "Metodologia:", choices = c("EDO", "EDF","seleccionar"), selected = "seleccionar" ),
      
      conditionalPanel
      (
        condition = "input.modelo == 'SI' && input.metodologia == 'EDO'",
        radioButtons(inputId = "metodoSI", "metodo:", c("Euler", "Adams"), selected = "Euler")
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SIR' && input.metodologia == 'EDO'",
        radioButtons(inputId = "metodoSIR", "metodo:", c("Euler", "Adams"), selected = "Euler")
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SI'&& input.metodologia != 'seleccionar'",
        sliderInput("numS", label = h3("Numero de Suceptibles iniciales"), min = 0,max = 1000, value = 517),
        sliderInput(inputId = "numI", label = h3("Numero de Infectados iniciales"),min = 1,max = 1000, value = 211),
        sliderInput(inputId = "Betha", label = h3("Tasa de transmicion e infeccion"), min = 0.0001,max = 1, value = 0.5802, step = 0.0001)
        #plotOutput("modelSI")
      ),
      conditionalPanel
      (
        condition = "input.modelo == 'SIR'&& input.metodologia != 'seleccionar'",
        sliderInput(inputId = "nums", label = "Numero de Suceptibles iniciales",min =0,max=1000, value = 20),
        sliderInput(inputId = "numi", label = "Numero de Infectados iniciales", min =1,max=1000,value = 15),
        sliderInput(inputId = "numr", label = "Numero de Recuperados iniciales", min=0,max=1000,value = 93),
        sliderInput(inputId = "betha", label = "Tasa de transmicion e infeccion", min=0.0001,max=1,value = 0.0001,step=0.0001),
        sliderInput(inputId = "gamma", label = "Tasa de recuperacion de la infeccion", min=0.0001,max=1,step=0.0001,value =  0.4043)
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
    #parÃ¡metros del modelo (coeficientes de las variables)
    
    if(input$modelo == "SI" && input$metodoSI == "Euler" && input$metodologia == "EDO")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
      
      tablas = graficar(init, param, N)
    }
    else if(input$modelo == "SIR" && input$metodoSIR == "Euler" && input$metodologia == "EDO")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
       
      
      tablas=graficar2(init, param, N)
      
    }
    else if(input$modelo == "SI" && input$metodoSI == "Adams" && input$metodologia == "EDO")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
       
      
      tablas=graficar3(init, param, N)
    }
    else if(input$modelo == "SIR" && input$metodoSIR == "Adams" && input$metodologia == "EDO")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
       
      
      
      tablas=graficar4(init, param, N)
      
    }
    else if(input$modelo == "SI" && input$metodologia == "EDF")
    {
      init <- c(S = input$numS, I = input$numI, R = 0)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$Betha, gamma = 0)
      init = init/N
       
      
      
      tablas=  graficar5(init, param, N)
      
    }
    else if(input$modelo == "SIR" && input$metodologia == "EDF")
    {
      init <- c(S = input$nums, I = input$numi, R = input$numr)
      N = init[1]+init[2]+init[3]
      param <- c(beta = input$betha, gamma = input$gamma)
      init = init/N
       
      
      
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
       
      y=init[2]
      if(input$metodologia == "EDF") colorcito = "black";
      
      apma1 <- function(t, y, param){
        beta = param[1]
        dy <- beta*(N-y)*y
        list(dy)
      } 
      
      apma1.flowField <- flowField(apma1, xlim = c(0, 70)/100, 
                                   ylim    = c(0, 1), parameters = param[1],
                                   points = 9, system = "two.dim", 
                                   add = FALSE, xlab = "time[s]", ylab = "I",col = colorcito, 
                                   main = "Campo de pendientes SI")
      grid()
      
    }
  })
  
}


#SI-EDO-Euler
graficar = function(init, param, N)
{
  
  #crear la funciÃ³n con las ODE
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
  
  #intervalo de tiempo y resolucion
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funcion 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  head(out, 10)
  #grÃ¡fica
  matplot(x = times/100, y = out, type = "l", xlab = "Tiempo[s]", ylab = "Proporcion[%]", main = "Modelo SI basico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #add leyenda de lineas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  out$S = ((out$S * 100)* N )/100
  out$I = ((out$I * 100)* N )/100
  return(out)
  ##grid()
}

#SIR-EDO-Euler
graficar2 = function(init, param, N)
{
  #crear la funciÃ³n con las ODE
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
  
  #intervalo de tiempo y resoluciÃ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #grÃ¡fica
  matplot(x = times/100, y = out, type = "l", xlab = "Tiempo[s]", ylab = "Proporcion[%]", main = "Modelo SIR basico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #aÃ±adir leyenda de lÃ?neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  
  out$S = ((out$S * 100)* N )/100
  out$I = ((out$I * 100)* N )/100
  out$R = ((out$R * 100)* N )/100
  return(out)
}

#SI-EDO-Adams
graficar3 = function(init, param, N)
{
  #crear la funciÃ³n con las ODE
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
  
  #intervalo de tiempo y resoluciÃ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
  #cambiar out a un data.frame
  out <- as.data.frame(out) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  out$R = NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #grÃ¡fica
  matplot(x = times/100, y = out, type = "l", xlab = "Tiempo[s]", ylab = "Proporcion[%]", main = "Modelo SI basico(menor error)",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #aÃ±adir leyenda de lÃ?neas
  legend(40, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  out$S = ((out$S * 100)* N )/100
  out$I = ((out$I * 100)* N )/100
  return(out)
}

#SIR-EDO-Adams
graficar4 = function(init, param, N)
{
  #crear la funciÃ³n con las ODE
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
  
  #intervalo de tiempo y resoluciÃ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
  #cambiar out a un data.frame
  out <- as.data.frame(out) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  #head(out, 10)
  #grÃ¡fica
  matplot(x = times/100, y = out, type = "l", xlab = "Tiempo[s]", ylab = "Proporcion[%]", main = "Modelo SIR bÃ¡sico(menor error)",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #aÃ±adir leyenda de lÃ?neas
  legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  out$time =times
  out$S = ((out$S * 100)* N )/100
  out$I = ((out$I * 100)* N )/100
  return(out)
}

#SI-EDF
graficar5 = function(init, param, N)
{
  #crear la funciÃ³n con las ODE
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
  
  #intervalo de tiempo y resoluciÃ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param, method = "euler")
  #cambiar out a un data.frame
  out <- as.data.frame(out) #aqui puede multiplicar 'out' por N
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
  auxOut = out
  auxOut$time =times
  out = matrix(rep(0, times = 70*2), nrow = 70, ncol = 2)
  out
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  
  

  clotting <- data.frame(
    u = auxOut$S,
    lot1 = c(0,times/100))
  
  summary(fit1 <- glm(lot1 ~ log(u), data = clotting, family = quasi(link = "identity", variance = "constant") ))
  
  rn <- range(clotting$u)
  newdata <- data.frame(u = seq(rn[1], rn[2], length.out = N)) #note the data.fram
  plotData <- xy.coords(x = newdata$u, y = predict(fit1, newdata = newdata))
  
  
  
  
  clotting2 <- data.frame(
    u = auxOut$I,
    lot1 = c(0,times/100))
  
  summary(fit1 <- glm(lot1 ~ log(u), data = clotting2, family = quasi(link = "identity", variance = "constant") ))
  
  rn <- range(clotting2$u)
  newdata <- data.frame(u = seq(rn[1], rn[2], length.out = N)) #note the data.fram
  plotData2 <- xy.coords(x = newdata$u, y = predict(fit1, newdata = newdata))
  
  
  
  Conf2mas1 = matrix(c(1:3, 3), nrow=2, byrow=F)
  layout(Conf2mas1)
  layout.show(3)
  
  plot(plotData2,xlab = "Tiempo[s]" , ylab = "log( dS | dt )", col = "green", main = "Modelo quasi varianza constante y enlace identidad", type = "l")
  plot(plotData,xlab = "Tiempo[s]", ylab = "log( dI | dt )", col ="red",main = "Modelo quasi varianza constante y enlace identidad", type = "l")
  matplot(x = times/100, y = out, type = "l",
          xlab = "Tiempo[s]", ylab = "Proporcion[%]", main = "Modelo SI básico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de líneas
  legend(0.1, 0.7, c("Susceptibles", "Infectados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  
  
  out$time =times
  auxOut$S = ((auxOut$S * 100)* N )/100
  auxOut$I = ((auxOut$I * 100)* N )/100
  return(auxOut)
}

#SIR-EDF
graficar6 = function(init, param, N)
{
  #crear la funciÃ³n con las ODE
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
  
  #intervalo de tiempo y resoluciÃ³n
  times <- seq(0, 70, by = 1)
  #resolver el sistema de ecuaciones con funciÃ³n 'ode'
  out <- ode(y = init, times = times, func = sir, parms = param)
  #cambiar out a un data.frame
  out <- as.data.frame(out) #aqui puede multiplicar 'out' por N
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
  auxOut = out
  auxOut$time =times
  out = matrix(rep(0, times = 70*3), nrow = 70, ncol = 3)
  out
  times = times[-1]
  out[,1] = ysus
  out[,2] = yinfec
  out[,3] = yrecup
  
  
  
  
  clotting <- data.frame(
    u = auxOut$S,
    lot1 = c(0,times/100))
  
  summary(fit1 <- glm(lot1 ~ log(u), data = clotting, family = quasi(link = "identity", variance = "constant") ))
  
  rn <- range(clotting$u)
  newdata <- data.frame(u = seq(rn[1], rn[2], length.out = N)) #note the data.fram
  plotData <- xy.coords(x = newdata$u, y = predict(fit1, newdata = newdata))
  
  
  
  
  clotting2 <- data.frame(
    u = auxOut$I,
    lot1 = c(0,times/100))
  
  summary(fit1 <- glm(lot1 ~ log(u), data = clotting2, family = quasi(link = "identity", variance = "constant") ))
  
  rn <- range(clotting2$u)
  newdata <- data.frame(u = seq(rn[1], rn[2], length.out = N)) #note the data.fram
  plotData2 <- xy.coords(x = newdata$u, y = predict(fit1, newdata = newdata))
  
  
  
  clotting3 <- data.frame(
    u = auxOut$I,
    lot1 = c(0,times/100))
  
  summary(fit1 <- glm(lot1 ~ log(u), data = clotting3, family = quasi(link = "identity", variance = "constant") ))
  
  rn <- range(clotting3$u)
  newdata <- data.frame(u = seq(rn[1], rn[2], length.out = N)) #note the data.fram
  plotData3 <- xy.coords(x = newdata$u, y = predict(fit1, newdata = newdata))
  
  
  Conf2mas1 = matrix(c(1:4, 4,4), nrow=3, byrow=F)
  layout(Conf2mas1) # con "heights" configuramos la altura
  layout.show(4)
  
  
  
  plot(plotData2,xlab = "Tiempo[s]" , ylab = "log( dS | dt )", col = "green", main = "Modelo quasi varianza constante y enlace identidad", type = "l")
  plot(plotData,xlab = "Tiempo[s]", ylab = "log( dI | dt )", col ="red",main = "Modelo quasi varianza constante y enlace identidad", type = "l")
  plot(plotData,xlab = "Tiempo[s]", ylab = "log( dR | dt )", col ="blue",main = "Modelo quasi varianza constante y enlace identidad", type = "l")
  matplot(x = times/100, y = out, type = "l",
          xlab = "Tiempo[s]", ylab = "S, I, R", main = "Modelo SIR básico",
          lwd = 1, lty = 1, bty = "l", col = 2:4)
  #añadir leyenda de líneas
  legend(0.1, 0.7, c("Susceptibles", "Infectados", "Recuperados"), 
         pch = 1, col = 2:4, bty = "n", cex = 1)
  
  
  out$time =times
  auxOut$S = ((auxOut$S * 100)* N )/100
  auxOut$I = ((auxOut$I * 100)* N )/100
  auxOut$R = ((auxOut$R * 100)* N )/100
  return(auxOut)
}

# Run the application 
shinyApp(ui = ui, server = server)

