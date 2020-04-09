f = function(x) 1/(1+25*x^2)

datx = seq(-6,6,by =4/10); daty = c()
x = 1
repeat
{
  daty[x] = f(datx[x])
  x = x + 1
  if(x == length(datx)+1)
    break;
}
plot(datx, daty, pch=19, cex=0.5, col = "red", asp=1, xlim = c(-6,6), ylim = c(0,4))
polyAjuste = poly_calc(datx, daty)
curve(polyAjuste,from=-6,to=6,add=T, lwd=1,col="blue")

coseno = function(x) cos(x)
i = 1
x_i = c()
u_i = c()
y_i = c()
repeat
{
  x_i[i] = coseno(((2*i+1)/(2*10+2))*pi)
  u_i[i] = ((6+6)*(x_i[i]-1))/(2+6)
  y_i[i] = f(u_i[i])
  i = i + 1
  if(i == 11)
    break;
}
x_i
u_i
y_i
polyAjuste = poly_calc(u_i, y_i)
curve(polyAjuste,from=-6,to=6,add=T, lwd=1,col="green")

f = function(x) 9 + 2.25*(x-8) - 0.125*(x-8)*(x-12)-0.0375*(x-8)*(x-12)*(x-14)
f(16)
