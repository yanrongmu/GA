############ Those are the default functions #########

########### Last Updated by Hector 11/30 ###################

AIC <- function(X, Y, Gen){
  P <- nrow(Gen)
  
  AICPop <- rep(0, P)
  # Compute the AIC for each individual
  for (i in 1:P){
    # The AIC is the sum of the MSE plus the complexity penalty
    lm_fit <- lm(Y ~ X[, Gen[i,]] )
    AICPop[i] <- nrow(X) * log(lm_fit$residuals^2) + 2 * sum(Gen[i,])
  }
  
  # Return the AIC
  return(AICPop)
}

Ranking <- function(Fitness){
  P <- length(Fitness)
  return( 2* rank(Fitness) / (P * (P+1)) )
}
  
Stop <- function(X, Y, LastGen, NewGen, ObjectiveFunction){
  # Compute the fitness of the best candidate in the previous generation
  FitnessOldGen <- ObjectiveFunction(X, Y, Gen = LastGen)
  BestOldGen <- max(FitnessOldGen)
  
  # Compute the fitness of the best candidate in the new generation
  FitnessNewGen <- ObjectiveFunction(X, Y, NewGen)
  BestNewGen <- max(FitnessNewGen)
  
  # Return whether the increase is smaller than 0.1%
  return( (abs((BestNewGen - BestOldGen) / BestNewGen)) < 0.001)
}