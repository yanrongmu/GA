############ Those are the default functions #########

########### Last Updated by Hector 11/30 ###################

AIC <- function(X, Y, Gen, nCores){
  P <- nrow(Gen)

<<<<<<< HEAD
  if(nCores ==1){
=======
  if(nCores == 1){
>>>>>>> cfe050b16235eefca8a8ca0e9a7a67ff790f10f6
      AICPop <- rep(0, P)
    # Compute the AIC for each individual
    for (i in 1:P){
      # The AIC is the sum of the MSE plus the complexity penalty
      lm_fit <- lm(Y ~ X[, Gen[i,]] )
      AICPop[i] <- nrow(X) * log(sum(lm_fit$residuals^2)) + 2 * sum(Gen[i,])
    }

    # Return the AIC
    return(AICPop)
  }

  if(nCores > 1){
    library(doParallel)
    registerDoParallel(nCores)

    # Compute the AIC for each individual
<<<<<<< HEAD
    AICPop <-  foreach (i = 1:P) %dopar% {
=======
    AICPop <- foreach (i = 1:P) %dopar% {
>>>>>>> cfe050b16235eefca8a8ca0e9a7a67ff790f10f6
      # The AIC is the sum of the MSE plus the complexity penalty
      lm_fit <- lm(Y ~ X[, Gen[i,]] )
      AICPop[i] <- nrow(X) * log(sum(lm_fit$residuals^2)) + 2 * sum(Gen[i,])
    }
    return(unlist(AICPop))
  }
}

Ranking <- function(Fitness){
  P <- length(Fitness)
  return( 2* rank(Fitness) / (P * (P+1)) )
}

Stop <- function(FitnessLastGen, FitnessNewGen){
  # Compute the fitness of the best candidate in the previous generation
  BestOldGen <- max(FitnessLastGen)

  # Compute the fitness of the best candidate in the new generation
  BestNewGen <- max(FitnessNewGen)

  # Return whether the increase is smaller than 0.1%
  return( (abs((BestNewGen - BestOldGen) / BestNewGen)) < 0.001)
}
