############ TEST: Select function #########
# It performs the genetic algorithm

########### Last Updated by Joanne 12/8 ###################


context("Select function")

test_that("Select returns a vector of the fittest individual", {
  expect_true(is.vector(select(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                               Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                               IterationsMax = 8,
                               nCores = 1)))
  expect_error(select(X = c("a", "b", "c"),
                      Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                      IterationsMax = 8,
                      nCores = 1),
               "X has to be a matrix of numbers")
})
