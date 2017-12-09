############ TEST: Crossover function #########
# It performs the crossover that produces the kid population from
# the parent population

########### Last Updated by Joanne 12/6 ############


context("Crossover function")

test_that("Crossover function creates child population matrix from list of 2 parents", {
  expect_is(Crossover(ParentSelection(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                        X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                        Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                        FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                             Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                             Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                             nCores = 1),
                                        Probs = Ranking)),
              "matrix")
  expect_equal(dim(Crossover(ParentSelection(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                      X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                      Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                      FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                           Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                           Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                           nCores = 1),
                                      Probs = Ranking))),
            c(20,10))
})
