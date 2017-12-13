############ This is the select function #########
# It performs the genetic algorithm

########### Last Updated by Hector 11/30 ###################

#' select Function
#'
#' This function implement a genetic algorithm for variable selection in regression problems.
#'
#' @param X A n*p matrix of predictors.
#' @param Y A n*1 matrix of responses.
#' @param ObjectiveFunction An objective criterion/fitness function. Defaults to AIC.
#' @param Probs The probability of parents being selected. Defaults to Ranking.
#' @param P Population size for generation. Must be an even integer. Defaults to 2p.
#' @param Initialized A matrix initialized the population. Defaults to Initialize(p, P).
#' @param mu The mutation rate has to be a number between 0 and 1. Defaults to 1/p.
#' @param StopFunction A stop criterion. Defaults to Stop function.
#' @param IterationsMax The maximum number of iterations.
#' @param IterationsMin The minimum number of iterations. Default to half of IterationsMAx
#' @param nCores Number of cores used for parallelization. Defaults to 1.
#' @return  @return A list with the elements
#' \item{FinalGeneration}{The generation of the last iteration}
#' \item{FittestInd}{The fittest individual of the the last iteration}
#' @export
#' @examples
#' select()
#'

# library("stats")
# source("Default.R")
# source("Input.R")
# source("Initialize.R")
# source("NextGen.R", chdir = TRUE)

select <- function(X, Y, ObjectiveFunction = AIC, Probs = Ranking,
                   P = 2 * ncol(X), Initialized = Initialize(ncol(X), P),
                   mu = 1 / ncol(X), StopFunction = Stop, IterationsMax,
                   IterationsMin = round(IterationsMax / 2), nCores = 1, ...){

  # Adaptive, if Y is a vector instead of a matrix
  Y <- matrix(Y)

  # Lots of checks on the variables
  Input(X, Y, ObjectiveFunction, Probs, P,
        Initialize, mu, Stop, Iterations, nCores)

  #Initialize the population
  Gen <- Initialized
  FitnessGen <- ObjectiveFunction(X, Y, Gen, nCores)


  # Initialize the stopping criterions
  i <- 0
  Stopping <- FALSE

  # Run the algorithm
  while(i < IterationsMax & !Stopping){
    # Span the next generation
    NewGen <- NextGen(LastGen = Gen, X, Y,
                      FitnessLastGen = FitnessGen, Probs, mu)

    # Check if the algorithm stops there
    FitnessNewGen <- ObjectiveFunction(X, Y, NewGen, nCores)
    if(i > IterationsMin){
      Stopping <- Stop(FitnessLastGen = FitnessGen, FitnessNewGen)
    }
    i <- i + 1
    Gen <- NewGen
    FitnessGen <- FitnessNewGen
  }

  # Return the fittest individual in the population
  return( list("FinalGeneration" = Gen,
               "FittestInd" = Gen[which.min(FitnessGen), ] ))
}
