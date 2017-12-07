############ TEST: default functions #########

########### Last Updated by Joanne 12/6 ###################

library(testthat)

context("default functions")

test_that("AIC returns the AIC", {
  expect_length(AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                    Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                    Gen = matrix(rbinom(200, 1, 0.5), nrow = 20)), 
                20)
  expect_type(AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                  Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                  Gen = matrix(rbinom(200, 1, 0.5), nrow = 20)),
              "double")
})

test_that("Ranking returns ranking of fitness", {
  expect_is(Ranking(AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                        Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                        Gen = matrix(rbinom(200, 1, 0.5), nrow = 20))),
            "numeric")
  expect_length(Ranking(AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                            Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                            Gen = matrix(rbinom(200, 1, 0.5), nrow = 20))),
                20)
})

test_that("Stop returns TRUE or FALSE", {
  expect_is(Stop(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                 Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                 LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                 NewGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                 ObjectiveFunction = AIC
                 ),
            "logical")
})
