set.seed(8)
X <- matrix(rnorm(1e4), ncol = 100)
beta <- 100 * rnorm(10)
Y <- X[,1:10] %*% matrix(beta, ncol = 1) + rnorm(1e2)

beta1 <- select(X, Y, Iterations = 100, mu = 0.05)
beta2 <- select(X, Y, Iterations = 100)
beta3 <- select(X, Y, Iterations = 100)
sum(beta2==beta3)
