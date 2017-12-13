############ This is the Input function #########
# It checks wheter the input to the global function are of the correct form

########### Last Updated by Hector 11/30 ###################

Input <- function(X, Y, ObjectiveFunction, Probs, P, Initialized, mu, StopFunction, IterationsMax, nCores){
  # Test X
  if( !is.matrix(X) )  stop("X has to be a matrix of numbers", call. = F)
  if( !is.numeric(X) ) stop("X has to be a matrix of numbers", call. = F)
  if( ncol(X) < 3) stop("Why are you running a genetic algorithm with that few covariates?!", call. = F)

  # test Y
  if(!(is.matrix(Y))) stop("Y has to be a matrix of numbers", call. = F)
  if(!is.numeric(Y))  stop("Y has to be a matrix of numbers", call. = F)

  if( nrow(Y) != nrow(X) )
    stop("X and Y should have the same number of observations", call. = F)


  # Test ObjectiveFunction
  if( typeof(ObjectiveFunction) != "closure" ) stop("ObjectiveFunction has to be a function", call. = F)

  # Test Probs
  if( typeof(Probs) != "closure" ) stop("Probs has to be a function", call. = F)

  # Test P
  if( length(P) != 1)  stop("Only provides one population size", call. = F)
  if( !is.numeric(P) ) stop("P has to be an integer", call. = F)
  if( round(P) != P )  stop("P has to be an integer", call. = F)
  if( P %% 2 != 0) stop ("The population size has to be even", call. = F)

  # Test Initialized
  if( typeof(Initialized) != "closure" & !is.vector(Initialized) &
      !is.matrix(Initialized))
                       stop("InitializeFunction has to be a vector or matrix", call. = F)

  # Test mu
  if( length(mu) != 1) stop("Only provides one mutation rate", call. = F)
  if( mu > 1 | mu < 0) stop("The mutation rate has to be between 0 and 1", call. = F)

  # Test StopFunction
  if( typeof(StopFunction) != "closure" ) stop("StopFunction has to be a function", call. = F)

  # Test Iterations
  if( length(IterationsMax) != 1)
    stop("Only provides one maximum number of iterations", call. = F)
  if( !is.numeric(IterationsMax) )
    stop("Iterations has to be an integer", call. = F)
  if( round(IterationsMax) != IterationsMax )
    stop("Iterations has to be an integer", call. = F)

  if( !is.numeric(nCores))
    stop("nCores must be an integer", call. = F)
  if (round(nCores) != nCores )
    stop("nCores must be an integer", call. = F)
}
