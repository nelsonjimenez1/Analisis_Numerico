a <- matrix(c(1,2,4, 2,1,-4, 0,0,3), 3, 3, byrow = TRUE)
r <- eigen(a) 
p <- r$vectors; 
lam <- r$value
d = diag(lam)
A =p%*%d%*%solve(p)
d
a
A
