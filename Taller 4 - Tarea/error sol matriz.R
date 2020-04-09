A = matrix(c(3.5,4.2, 1/2,1/3), nrow=2, byrow=TRUE) 
b = c(1.3, 7/4) 
B = matrix(c(3.8,4.2, 1/2,1/3), nrow=2, byrow=TRUE) 
cond = 1/rcond(A)
e= cond*((det(A)-det(B))/det(A))
cat("cota superior de error", e)

