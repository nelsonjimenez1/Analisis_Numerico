require(bezier)
require(gridBezier)

#-----------
# BEZIER SPLINES 
#-----------
t <- seq(0, 4, length=100)
p <- matrix(c(0,7,1,
              0.33,6.92,1,
              0.63, 6.62,1,
              0.7,6.2,1,#1
              1.44,6.05,1,
              2.88,5.43,1,
              4.44,4.38,1,#2 
              5.43,2.88,1,
              6.05,1.44,1,
              6.2,0.7,1,#3
              6.62,0.63,1,
              6.92,0.33,1,
              7,0,1), nrow=13, ncol=3, byrow=TRUE)

bezier_points <- bezier(t=t, p=p, deg =3)
plot(bezier_points, type = 'l', col = "red")

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
    }
    if(contx == 10001)
    {
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
# Circulo
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
i = 1
x = c()
y = c()
z = c()
cont = 1
num = X2[20000]
limp = 5.3
limn = -5.3

repeat
{
  if(X2[i] == num)
  {
    if(limn <= X1[i] && X1[i] <= limp && limn <= Y[i] && Y[i] <= limp)
    {
      x[cont] = X1[i]
      y[cont] = Y[i]
      z[cont] = X2[i]
      cont = cont + 1
    }
  }
  
  i = i + 1
  
  if(i == 20001)
  {
    break;
  }
}

x = x[-length(x)]
y = y[-length(y)]
z = z[-length(z)]
x = x[-length(x)]
y = y[-length(y)]
z = z[-length(z)]

i = length(x) + 1
j = 1
desfase = 0.01
ayuda = 0.51 - 0.01
marcar = c()
it = 1

repeat
{
  if (x[j] >= 0 && y[j]>= 0)
  {
    x[i] = x[j] - desfase
    y[i] = y[j] - desfase
    z[i] = ayuda
    
    if(x[i]< 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(y[i] < 0)
    {
      marcar[it] = i
      it = it + 1
    }
  }
  else if (x[j] <= 0 && y[j]>= 0)
  {
    x[i] = x[j] + desfase
    y[i] = y[j] - desfase
    z[i] = ayuda
    
    if(x[i] > 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(y[i] < 0)
    {
      marcar[it] = i
      it = it + 1
    }
  }
  else if(x[j] >= 0 && y[j]<= 0)
  {
    x[i] = x[j] - desfase
    y[i] = y[j] + desfase
    z[i] = ayuda
    
    if(x[i]< 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(y[i] > 0)
    {
      marcar[it] = i
      it = it + 1
    }
  }
  else if(x[j] <= 0 && y[j]<= 0)
  {
    x[i] = x[j] + desfase
    y[i] = y[j] + desfase
    z[i] = ayuda
    
    if(x[i]> 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(y[i] > 0)
    {
      marcar[it] = i
      it = it + 1
    }
  }
  
  i = i + 1
  j = j + 1
  
  if(j == 207)
  {
    j = 1
    desfase = desfase + 0.01
    ayuda = ayuda - 0.01
  }
  if(i == 206*100+1)
  {
    break;
  }
}

x=x[-marcar]
y=y[-marcar]
z=z[-marcar]

#-----------
# Piso
#-----------
ultimosx = c()
ultimosy = c()
ultimosz = c()
i = 1
cont = 1
num = z[length(x)]
num2 = num + 0.01
tam = length(x) + 1

repeat
{
  if(z[i] == num || z[i] == num2)
  {

      ultimosx[cont] = x[i]
      ultimosy[cont] = y[i]
      ultimosz[cont] = z[i]
      cont = cont + 1
  }
  
  i = i + 1
  
  if(i == tam)
  {
    break;
  }
}

i = length(ultimosx) + 1
j = 1
desfase = 0.01
marcar = c()
it = 1

repeat
{
  if (ultimosx[j] >= 0 && ultimosy[j]>= 0)
  {
    ultimosx[i] = ultimosx[j] - desfase
    ultimosy[i] = ultimosy[j] - desfase
    ultimosz[i] = ultimosz[j]
    
    if(ultimosx[i]< 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(ultimosy[i] < 0)
    {
      marcar[it] = i
      it = it + 1
    }
    
  }
  else if (ultimosx[j] <= 0 && ultimosy[j]>= 0)
  {
    ultimosx[i] = ultimosx[j] + desfase
    ultimosy[i] = ultimosy[j] - desfase
    ultimosz[i] = ultimosz[j]
    
    if(ultimosx[i]> 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(ultimosy[i] < 0)
    {
      marcar[it] = i
      it = it + 1
    }

  }
  else if(ultimosx[j] >= 0 && ultimosy[j]<= 0)
  {
    ultimosx[i] = ultimosx[j] - desfase
    ultimosy[i] = ultimosy[j] + desfase
    ultimosz[i] = ultimosz[j]
    
    if(ultimosx[i]< 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(ultimosy[i] > 0)
    {
      marcar[it] = i
      it = it + 1
    }
  }
  else if(ultimosx[j] <= 0 && ultimosy[j]<= 0)
  {
    ultimosx[i] = ultimosx[j] + desfase
    ultimosy[i] = ultimosy[j] + desfase
    ultimosz[i] = ultimosz[j]
    
    if(ultimosx[i]> 0)
    {
      marcar[it] = i
      it = it + 1
    }
    else if(ultimosy[i] > 0)
    {
      marcar[it] = i
      it = it + 1
    }
  }
  
  i = i + 1
  j = j + 1
  
  if(j == 169)
  {
    j = 1
    desfase = desfase + 0.01
  }
  if(i == 169*1000+1)
  {
    break;
  }
}

ultimosx=ultimosx[-marcar]
ultimosy=ultimosy[-marcar]
ultimosz=ultimosz[-marcar]

#-----------
# Gráfico 3D
#-----------
require(plot3D)
library(rgl)

plot3d(X1, Y, X2, type = "p", col = "red", xlim = c(-7,7), ylim = c(-7,7), zlim = c(-2,2))
plot3d(x, y, z, type = "p", col = "blue", xlim = c(-7,7), ylim = c(-7,7), zlim = c(-2,2))
plot3d(ultimosx, ultimosy, ultimosz, type = "p", col = "green", xlim = c(-7,7), ylim = c(-7,7), zlim = c(-2,2))

xx = X1
yy = Y
zz = X2

xxx = x
yyy = y
zzz = z

xxxx = ultimosx
yyyy = ultimosy
zzzz = ultimosz
