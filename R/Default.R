############ Those are the default functions #########

AIC <- function(X, Y, Gen, nCores){
  P <- nrow(Gen)

  if(nCores ==1){
      AICPop <- rep(0, P)
    # Compute the AIC for each individual
    for (i in 1:P){
      # The AIC is the sum of the MSE plus the complexity penalty
      selected <- 1:ncol(X) * Gen[i,]
      if (sum(selected) == 0){
        # If no parameters are selected we fit the average
        AICPop[i] <- nrow(X) * log(sum( (Y- mean(Y))^2 )) + 2 * sum(Gen[i,])
      } else {
        # Else wo do the fit
        lm_fit <- lm(Y ~ X[, selected] )
        AICPop[i] <- nrow(X) * log(sum(lm_fit$residuals^2)) + 2 * sum(Gen[i,])
      }
    }

    # Return the AIC
    return(AICPop)
  }

  if(nCores > 1){
    library(doParallel)
    registerDoParallel(nCores)

    # Compute the AIC for each individual
    AICPop <- foreach (i = 1:P) %dopar% {
      selected <- 1:ncol(X) * Gen[i,]
      if (sum(selected == 0)){
        # If no parameters are selected we fit the average
        AICInd <- nrow(X) * log(sum( (Y- mean(Y))^2 )) + 2 * sum(Gen[i,])
      } else {
        # Else wo do the fit
        lm_fit <- lm(Y ~ X[, selected] )
        AICInd <- nrow(X) * log(sum(lm_fit$residuals^2)) + 2 * sum(Gen[i,])
      }
      AICInd
    }
    return(unlist(AICPop))
  }
}

Ranking <- function(Fitness){
  P <- length(Fitness)
  return( 2* rank(-Fitness) / (P * (P+1)) )
}

Stop <- function(FitnessLastGen, FitnessNewGen){
  # Compute the fitness of the best candidate in the previous generation
  BestOldGen <- min(FitnessLastGen)

  # Compute the fitness of the best candidate in the new generation
  BestNewGen <- min(FitnessNewGen)

  # Return whether the increase is smaller than 0.1%
  return( (abs((BestNewGen - BestOldGen) / BestNewGen)) < 0.001)
}
