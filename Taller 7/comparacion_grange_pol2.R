###
require(PolynomF)
#p(10,12)
#p(12,18)
#p(14,21)

#a_0 + 10a_1 + 100a_2 = 12
#a_0 + 12a_1 + 144a_2 = 18
#a_0 + 14a_1 + 196a_2 = 21

x = c(6,8,10,12,14,16,18,20)
y = c(7,9,12,18,21,19,15,10)

plot(x,y, pch=19, cex=0.3, col = "red", asp=1,xlab="X", ylab="Y", main="Diagrama")


#pol de grado 2
datos = c(1,10,100,1,12,144,1,14,196)
A = matrix( data = datos, nrow=3, byrow = T)
b = c(12,18,21)
solve(A,b)
f = function(x) -63 + 11.25*x -0.375*x^2
f(13)
curve(f, from=x[3],to=x[6],add=T, lwd=1,col="blue")

#pol de grado 2 + c
f = function(x) -63 + 11.25*(x+1) -0.375*(x+1)^2
f(13)
curve(f, from=x[3],to=x[6],add=T, lwd=1,col="orange")

#pol de lagrange
datx = x[3:6]; daty = y[3:6]
polyAjuste = poly_calc(datx, daty)
polyAjuste(13)
curve(polyAjuste, from=x[3],to=x[6],add=T, lwd=1,col="green")
