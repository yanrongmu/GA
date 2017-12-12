# Set the seed to get the same results
set.seed(8)

# Build the dataset
# X is 100 by 30, beta is 10 by 1
X <- matrix(rnorm(30 * 100), ncol = 30)
beta <- 100 * rnorm(10)

# We randomly select 10 columns of X (out of 30) as covariates
select <- sample(1:ncol(X), size = 10, replace = F)

# Y is X*beta plus some noice
Y <- X[,select] %*% matrix(beta, ncol = 1) + rnorm(100)

# Apply the genetic algorithm with default coefficients
beta1 <- select(X, Y, IterationsMax = 200)

# Check the fit
sum(lm(Y ~X [,select])$residuals^2)
sum(lm(Y ~X [,1:ncol(X)*beta1$FittestInd])$residuals^2)

