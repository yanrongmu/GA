############ TEST: Input function #########
# It checks whether the input to the global function are of the correct form

########### Last Updated by Joanne 12/6 ###################

context("Input function")


test_that("Input checks whether the inputs are of the correct form", {
  expect_silent(Input(matrix(rbinom(200, 1, 0.5), nrow = 20), 
                      matrix(rbinom(20, 1, 0.5), nrow = 20), 
                      ObjectiveFunction = AIC, 
                      Probs = Ranking, 
                      P = 8, 
                      Initialize = Initialize, 
                      mu = 0.5, 
                      Stop = Stop, 
                      Iterations = 3))
  expect_error(Input(matrix(rbinom(200, 1, 0.5), nrow = 20), 
                     matrix(rbinom(20, 1, 0.5), nrow = 20), 
                     ObjectiveFunction = AIC, 
                     Probs = Ranking, 
                     P = 8, 
                     Initialize = Initialize, 
                     mu = 1.3, 
                     Stop = Stop, 
                     Iterations = 3), "The mutation rate has to be between 0 and 1")
  expect_error(Input(matrix(rbinom(200, 1, 0.5), nrow = 20), 
                     matrix(rbinom(20, 1, 0.5), nrow = 20), 
                     ObjectiveFunction = AIC, 
                     Probs = Ranking, 
                     P = 9, 
                     Initialize = Initialize, 
                     mu = 0.5, 
                     Stop = Stop, 
                     Iterations = 3), "The population size has to be even")
  expect_error(Input(matrix(rbinom(200, 1, 0.5), nrow = 20), 
                     matrix(rbinom(100, 1, 0.5), nrow = 10), 
                     ObjectiveFunction = AIC, 
                     Prob = Ranking, P = 8, 
                     Initialize = Initialize, 
                     mu = 0.5, 
                     Stop = Stop, 
                     Iterations = 3), "X and Y should have the same number of observations")
  
})
