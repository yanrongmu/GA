############ This is the select function #########
# It performs the genetic algorithm

########### Last Updated by Hector 11/30 ###################
library("stats")
source("Default.R")
source("Input.R")
source("Initialize.R")
source("NextGen.R", chdir = TRUE)

select <- function(X, Y, ObjectiveFunction = AIC, Probs = Ranking,
                   P = 2 * ncol(X), Initialize = Initialize(ncol(X), P),
                   mu = 1 / ncol(X), Stop = Stop, Iterations, ...){
  
  # Lots of checks on the variables
  Input(X, Y, ObjectiveFunction, Probs, P, Initialize, mu, Stop, Iterations)
  
  #Initialize the population
  Gen <- Initialize
  
  # Initialize the stopping criterions
  i <- 0
  Stopping <- FALSE
  
  # Run the algorithm
  while(i < Iterations & !Stopping){
    # Span the next generation
    NewGen <- NextGen(LastGen = Gen, X, Y, ObjectiveFunction, Probs, mu)
    
    # Check if the algorithm stops there
    Stopping <- Stop(X, Y, LastGen = Gen, NewGen, ObjectiveFunction)
    i <- i + 1
    Gen <- NewGen
  }
  
  # Return the fittest individual in the population
  Fitness <- ObjectiveFunction(X, Y, Gen)
  return( Gen[which.max(Fitness), ] )
}
