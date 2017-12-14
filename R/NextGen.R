############ This is the NextGen function #########

NextGen <- function(LastGen, X, Y, FitnessLastGen, Probs, mu){
  p <- ncol(X)
  P <- nrow(LastGen)

  # Select the parents
  Parents <- ParentSelection(LastGen, X, Y, FitnessLastGen, Probs)

  # Do Crossover between the Parents
  Kids <- Crossover(Parents)

  # Mutate the kids
  Kids <- Mutation(Kids, mu)

  #Return the Next Generation
  return(Kids)
}
