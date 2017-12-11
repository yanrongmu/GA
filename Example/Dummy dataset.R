set.seed(8)
X <- matrix(rnorm(3e3), ncol = 30)
beta <- 100 * rnorm(10)
select <- sample(1:ncol(X), size = 10, replace = F)
Y <- X[,select] %*% matrix(beta, ncol = 1) + rnorm(1e2)

beta1 <- select(X, Y, Iterations = 100)

sum(lm(Y ~X [,select])$residuals^2)
sum(lm(Y ~X [,1:ncol(X)*beta1])$residuals^2)
