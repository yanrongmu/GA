############ This is the Crossover function #########
# It performs the crossover that produces the kid population from 
# the parent population

########### Last Updated by Hector 11/27 ############


Crossover <- function(Parents){
  # Takes as input the pairs of parents
  Parent1 <- Parents$Parent1
  Parent2 <- Parents$Parent2
  P <- 2 * nrow(Parent1)
  p <- ncol(Parent1)
  
  # Find the place where the crossover happens, between 1 and p-1
  # We assume there is one and only one crossover event every time
  #(so the crossover region cannot be 0 or p).
  CrossoverRegions <- sample(x = 2:(p-1), size = P/2, replace = T)
  
  # Create a mnatrix that says wheter the gene has the allele from
  # parent1 (origin is 1) or parent2 (origin is 0)
  Origins <- matrix(0, nrow = P/2, ncol = p)
  for (i in 1:(P/2)){
    Origins[i,] <- c(rep(1, times = CrossoverRegions[i]),
                     rep(0, times = p - CrossoverRegions[i]))
  }
  
  # Do the crossovers for the first kid of each pair of parents
  Kids1 <- Parent1 * Origins + Parent2 * (1-Origins)
  # Do the crossovers for the second kid of each pair of parents
  Kids2 <- Parent2 * Origins + Parent1 * (1-Origins)
  
  # Return the crossover children
  return(rbind(Kids1, Kids2))
}
