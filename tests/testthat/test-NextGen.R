############ TEST: NextGen function #########

########### Last Updated by Joanne 12/6 ###################

library(testthat)

context("NextGen function")

test_that("NextGen produces a matrix of the next generation", {
  expect_is(NextGen(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20), 
                              X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              ObjectiveFunction = AIC,
                              Probs = Ranking,
                              mu = 0.5),
              "matrix")
  expect_equal(dim(NextGen(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20), 
                    X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                    Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                    ObjectiveFunction = AIC,
                    Probs = Ranking,
                    mu = 0.5)),
            c(20,10))
})
