simpson = function(fun, a,b, n) 
{
  if (n%%2 != 0) stop("En la regla de Simpson, n es par!")
  h = (b-a)/n
  i1 = seq(1, n-1, by = 2) # impares
  i2 = seq(2, n-2, by = 2) # pares
  h/3 * ( fun(a) + fun(b) + 4*sum( fun(a+i1*h) ) + 2*sum( fun(a+i2*h) ) )
}

f = function(x) sin(x^2)
result = simpson(f,0,1,10)

#integrales de Fresnel sen(t^2)
n = 0
num =  function(x, n) x^(4*n+3)
div = function(x, n) (4*n+3)*(factorial(2*n+1))
x = 1
sum = 0

repeat
{
  i = ((-1)^n)*(num(x,n)/div(x,n))
  sum = sum + i
  cat("I", sum, "|i-I|", abs(result - sum), "\n")
  n = n + 1
  if(n==12)
  {
    break;
  }
  
}


