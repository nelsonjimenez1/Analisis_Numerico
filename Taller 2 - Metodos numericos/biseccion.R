biseccion = function(f, xa, xb, tol)
{
  if ( sign( f(xa)  ) == sign ( f(xb) ) ) 
  {
    stop ( "MISMO SIGGNO F(A) F(B)" )
  }  
  errores = c()
  eje_x = c()
  eje_y = c()
  a = xa 
  b = xb
  k = 0
  cat( formatC ( c( "a","b", "m", "errorE"), width = 15, format = "f", flag = " "  ), "\n")
  cont = 0
  
  repeat 
  {
    cont = cont + 1
    m = a + 0.5 * (b-a)
    
    if( f( m ) == 0)
    {
      cat("cero de f en el [",xa," "," ",xb," ] es ", m)
    }
    if( sign ( f(a) ) != sign ( f(m) ))
    {
      b = m
      
    }else
    {
      a  = m
    }
    
    dx = (b-a)/2
    errores[cont] = dx
    
    
    #imprimir estado
    
    cat( formatC( c(cont, a,b,m,dx ), digits = 7, width = -15, format = "f", flag = "  "  ), "\n" )
    k = k + 1
    #until
    if( dx < tol )
    {
      cat ("cero de f en el [",xa," "," ",xb," ] es aprox", m, "con error <=", dx )
      iter = c(1:cont)
      cont_n = 0;
      cont_e = 0;
      repeat
      {
        eje_x[cont_n] = errores[cont_e]
        eje_y[cont_n] = errores[cont_e+1]
        cont_n = cont_n + 1
        cont_e = cont_e + 1;
        
        if (cont_n == cont)
        {
          break;
        }
      }
      
      plot(eje_x, eje_y)
      plot(iter, errores)
      break;
    }
  }
}

f = function(x) signif(exp(1),8)^x-signif(pi, 8)*x
par(mar = rep(2, 4))
curve(f, -2, 2); abline( h = 0, v = 0)
biseccion(f, 0, 1, 10^-8)
biseccion(f, 1, 2, 10^-8)
