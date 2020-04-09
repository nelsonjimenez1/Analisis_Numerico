steffensen = function(f, x0, tol, nmax)
{
    i = 0
    errores = c()
    eje_x  =  c()
    eje_y  =  c()

    repeat
    {
        i = i + 1
        a = f(x0)*f(x0)
        b = f(x0+f(x0))-f(x0)
        x1 = x0 - (a/b)
        error = (abs(x1-x0))/x1
        errores[i] = error
        temp = f(x0)
        x0 = x1
        
        cat("X=", x0,"\t","E=", error, "iteracion", i,"\n")

        if(abs(temp) <= tol)
        {
            iter = c(1:i)
            cont_n = 0;
            cont_e = 0;
            
            repeat
            {
                eje_x[cont_n] = errores[cont_e]
                eje_y[cont_n] = errores[cont_e+1]
                cont_n = cont_n + 1
                cont_e = cont_e + 1;
                
                if (cont_n == i)
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
steffensen(f, 0, 10^-8, 10)
steffensen(f, 1.5, 10^-8, 10)

