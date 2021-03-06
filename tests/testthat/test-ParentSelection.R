############ TESET: ParentSelection function #########
# It selects individual from the population to be parents for the next generation
# Return a list of two P/2 by n matrices Each row correspond to a parent
# that match the parent of the same row from the other dataframe

########### Last Updated by Joanne 12/6 ############

context("ParentSelection function")

test_that("ParentSelection creates a list of two P/2 by n matrices", {
  expect_is(ParentSelection(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                   Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                   Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                   nCores = 1),
                              Probs = Ranking),
              "list")
  expect_length(ParentSelection(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                     Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                     Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                     nCores = 1),
                                Probs = Ranking),
                2)
  expect_true(is.matrix(ParentSelection(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                     Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                     Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                     nCores = 1),
                                Probs = Ranking)$Parent1))
  expect_equal(dim(ParentSelection(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                   X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                   Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                   FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                        Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                        Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                        nCores = 1),
                                   Probs = Ranking)$Parent1),
               c(10, 10))
})
