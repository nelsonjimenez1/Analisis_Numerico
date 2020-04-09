require(bezier)
require(gridBezier)
t <- seq(0, 5, length=100)

## BEZIER CURVES ##
p <- matrix(c(10,5,1, 
              14,9,1,
              18,5,1, 
              12,2,1,
              10,1,1,
              8,2,1,
              5,5,1,
              4,11,1,
              6,17,1,
              13,20,1,
              18,18,1,
              20,15,1,
              16,14,1,
              12,14,1,
              8,20,1,
              8,24,1), nrow=16, ncol=3, byrow=TRUE)
bezier_points <- bezier(t=t, p=p, deg = 3)
plot(bezier_points)

X1= bezier_points[,1]
Y= bezier_points[,2]
X2= bezier_points[,3]

attach(ex1)                
names(ex1)
dim(ex1)

#----------------------------------------------------------
# Gráfico com as 3 variáveis em estudo em 2D
#----------------------------------------------------------
require(scatterplot3d)
fig = scatterplot3d(X1,Y,X2, box=F, type='p', lwd=1, pch=19, 
                    xlim=c(0,30), ylim=c(0,30), zlim=c(0,30))
plano = lm(Y~X1+X2)
fig$plane3d(plano, lty.box = "solid")

#----------------------------------------------------------
# Gráfico com as 3 variáveis em estudo em 3D
#----------------------------------------------------------

require(Rcmdr)
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
          bg='black', sphere.size=1, revolutions=1, axis.col='white',
          xlab="X", ylab="Y", zlab="Z")


