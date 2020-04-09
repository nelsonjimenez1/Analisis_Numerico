Xs = c(8,12,13,18)
Temperaturas = c (9,18,21,15)

#p_3 = a_0 + a_1(x-x_0) + a_2(x-x_0)(x-x_1) + a_3(x-x_0)(x-x_1)(x-x_2)

A = matrix(data = c(1,0,0,0,
                    1,4,0,0,
                    1,6,12,0,
                    1,10,60, (60*4)), nrow = 4, byrow =T )
b = c(9,18,21,15)
#redondear 0 cifras

solve(A, b)

Temperaturas_a_interpolar  = c(16, 10)
f = function(x) 9 + 2.25*(x-8) - 0.125*(x-8)*(x-12)-0.0375*(x-8)*(x-12)*(x-14)
f(16)
