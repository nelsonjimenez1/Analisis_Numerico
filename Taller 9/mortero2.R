require(bezier)
require(gridBezier)

#-----------
# BEZIER SPLINES 
#-----------
t <- seq(0, 4, length=100)
p <- matrix(c(0,6,1,
              0.33,5.92,1,
              0.63, 5.62,1,
              0.7,5.2,1,#1
              1.44,5.05,1,
              2.88,4.43,1,
              3.44,3.38,1,#2 
              4.43,2.88,1,
              5.05,1.44,1,
              5.2,0.7,1,#3
              5.62,0.63,1,
              5.92,0.33,1,
              6,0,1), nrow=13, ncol=3, byrow=TRUE)

bezier_points <- bezier(t=t, p=p, deg =3)
plot(bezier_points)

#-----------
# PUNTO
#-----------
X1= bezier_points[,1]
Y= bezier_points[,2]
X2= bezier_points[,3]

#-----------
# FONDO (cuadrante 1)
#-----------
contx = length(X1) + 1
conty = 1
ayuda = 1-0.01
ayuda2 = 0.02
ayuda3 = 0.02

repeat
{
  if(X1[conty]-ayuda3 >= 0)
  {
    X1[contx] = X1[conty] -ayuda3
  }
  else
  {
    X1[contx] = 0 
  }
  
  if(Y[conty] -ayuda2 >= 0)
  {
    Y[contx] = Y[conty] -ayuda2
  }
  else
  {
    Y[contx] = 0
  }
  
  X2[contx] = ayuda
  contx = contx + 1
  conty = conty + 1
  
  if(conty == 101) 
  {
    conty = 1
    ayuda = ayuda - 0.01
    ayuda2 = ayuda2 + 0.02
    ayuda3 = ayuda3 + 0.02
    cat("hola", ayuda2, "\n")
  }
  if(contx == 10001)
  {
    cat("z", X2[contx-1], "\n")
    break;
  }
}

#-----------
# PROYECTAR 1 (cuadrante 2)
#-----------
contx = length(X1) + 1
conty = 1
ayuda = 1
ayuda2 = 0
ayuda3 = 0
tam = length(X1)*2+1

repeat
{
  if(-X1[conty]+ayuda3 <= 0)
  {
    X1[contx] = -X1[conty]+ayuda3
  }
  else
  {
    X1[contx] = 0 
  }
  
  if(Y[conty] -ayuda2 >= 0)
  {
    Y[contx] = Y[conty] -ayuda2
  }
  else
  {
    Y[contx] = 0
  }
  
  X2[contx] = ayuda
  contx = contx + 1
  conty = conty + 1
  
  if(conty == 101)
  {
    conty = 1
    ayuda = ayuda - 0.01
    ayuda2 = ayuda2 + 0.02
    ayuda3 = ayuda3 + 0.02
  }
  if(contx == tam)
  {
    break;
  }
}

#-----------
# PROYECTAR 2 (cudrante 4)
#-----------
contx = length(X1) + 1
conty = 1
ayuda = 1
ayuda2 = 0
ayuda3 = 0
tam = 30001

repeat
{
  if(X1[conty]-ayuda3 >= 0)
  {
    X1[contx] = X1[conty] - ayuda3
  }
  else
  {
    X1[contx] = 0 
  }
  
  if(-Y[conty] + ayuda2 <= 0)
  {
    Y[contx] = -Y[conty] + ayuda2
  }
  else
  {
    Y[contx] = 0
  }
  
  X2[contx] = ayuda
  contx = contx + 1
  conty = conty + 1
  
  if(conty == 101)
  {
    conty = 1
    ayuda = ayuda - 0.01
    ayuda2 = ayuda2 + 0.02
    ayuda3 = ayuda3 + 0.02
  }
  if(contx == tam)
  {
    break;
  }
}

#-----------
# PROYECTAR 3 (cuadrante 3)
#-----------
contx = length(X1) + 1
conty = 1
ayuda = 1
ayuda2 = 0
ayuda3 = 0
tam = 40001

repeat
{
  if(-X1[conty]+ayuda3 <= 0)
  {
    X1[contx] = -X1[conty] + ayuda3
  }
  else
  {
    X1[contx] = 0 
  }
  
  if(-Y[conty] + ayuda2 <= 0)
  {
    Y[contx] = -Y[conty] + ayuda2
  }
  else
  {
    Y[contx] = 0
  }
  
  X2[contx] = ayuda
  contx = contx + 1
  conty = conty + 1
  
  if(conty == 101)
  {
    conty = 1
    ayuda = ayuda - 0.01
    ayuda2 = ayuda2 + 0.02
    ayuda3 = ayuda3 + 0.02
  }
  if(contx == tam)
  {
    break;
  }
}

#-----------
# Gráfico 3D
#-----------

X1 = X1[-c(5001:10000)]
Y = Y[-c(5001:10000)]
X2 = X2[-c(5001:10000)]

X1 = X1[-c(10001:15000)]
Y = Y[-c(10001:15000)]
X2 = X2[-c(10001:15000)]

X1 = X1[-c(15001:20000)]
Y = Y[-c(15001:20000)]
X2 = X2[-c(15001:20000)]

X1 = X1[-c(20001:25000)]
Y = Y[-c(20001:25000)]
X2 = X2[-c(20001:25000)]

require(plot3D)
library(rgl)
plot3d(X1, Y, X2, type = "p", col = "red",
       xlab = "x", ylab="y", zlab="z", add = T)

