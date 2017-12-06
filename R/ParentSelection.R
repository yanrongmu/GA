############ This is the ParentSelection function #########
# It selects individual from the population to be parents for the next generation
# Return a list of two P/2 by n matrices Each row correspond to a parent 
# that match the parent of the same row from the other dataframe

########### Last Updated by Hector 11/30 ############

ParentSelection <- function(LastGen, X, Y, ObjectiveFunction, Probs){
  P <- nrow(LastGen)
  
  # Compute the fitness of each individual
  Fitness <- ObjectiveFunction(X, Y, LastGen)
  
  # Compute the probability of choosing each individual
  Proba <- Probs(Fitness)
  
  #Initialize the parents each pair of parents
  Parent1 <- matrix(0, ncol = ncol(LastGen), nrow = P/2)
  Parent2 <- matrix(0, ncol = ncol(LastGen), nrow = P/2)
  
  # Sample P/2 pairs of parents
  for (i in 1:(P/2)){
    # Sample a pair of parent. Each individual has a probability of
    # being picked determined by probs
    Indices <- sample(x = 1:P, size = 2, replace = FALSE, prob = Proba)
    Parent1[i,] <- LastGen[Indices[1],]
    Parent2[i,] <- LastGen[Indices[2],]
  }
  
  # Return the pairs of parents
  return(list("Parent1" = Parent1, "Parent2" = Parent2))
}