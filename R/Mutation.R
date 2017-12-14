############ This is the mutation function #########
# It mutates the kids genome after crossover

Mutation <- function(Kids, mu){
  P <- nrow(Kids)
  p <- ncol(Kids)
  
  # Randomly choose the genes and kids where we have mutation
  MutationRegion <- matrix(rbinom(p * P, size = 1, prob = mu), nrow = P)
  
  # Randomly choose the new mutations
  Mutations <- matrix(rbinom(p * P, size = 1, prob = 0.5), nrow = P)
  
  # Mutate the genes
  Kids <- (1 - MutationRegion) * Kids + MutationRegion * Mutations * Kids
  
  #Return the mutated kids
  return(Kids)
}