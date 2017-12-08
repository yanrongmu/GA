############ TEST: NextGen function #########

########### Last Updated by Joanne 12/6 ###################


context("NextGen function")

test_that("NextGen produces a matrix of the next generation", {
  expect_is(NextGen(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                              FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                   Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                   Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                   nCores = 1),
                              Probs = Ranking,
                              mu = 0.5),
              "matrix")
  expect_equal(dim(NextGen(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                    X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                    Y = matrix(rbinom(200, 1, 0.5), nrow = 20),
                    FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                         Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                         Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                         nCores = 1),
                    Probs = Ranking,
                    mu = 0.5)),
            c(20,10))
})
