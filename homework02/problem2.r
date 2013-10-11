library(matrixcalc)

x = 1:20
y = x+rnorm(20)
A = vandermonde.matrix(x,20)

for (i in 2:20) { # This loop will solve the normal eqn. manually as long as it can
  print(solve(crossprod(A[,1:i],A[,1:i]),crossprod(A[,1:i],y)))
}

A = A[,-1] # Remove column of ones
for (i in 1:19) { # This will use lm( ... ) to find the parameters
  out = lm(y ~ A[,1:i])
  print(sprintf("--------------- i = %d -------------",i))
  print(summary(out))
}

