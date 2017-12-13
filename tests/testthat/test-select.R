############ TEST: Select function #########
# It performs the genetic algorithm

########### Last Updated by Joanne 12/8 ###################


context("Select function")

test_that("Select returns a list of 2", {
  expect_equal(length(select(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                               Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                               IterationsMax = 8,
                               nCores = 1)),
               2)
})

test_that("Select returns the last generation and the fittest individual", {
  expect_true(is.matrix(select(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                               Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                               IterationsMax = 8,
                               nCores = 1)$FinalGeneration))
  expect_true(is.vector(select(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                               Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                               IterationsMax = 8,
                               nCores = 1)$FittestInd))
})

test_that("Select recognizes incorrect input", {
  expect_error(select(X = c("a", "b", "c"),
                      Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                      IterationsMax = 8,
                      nCores = 1),
               "X has to be a matrix of numbers")
})
