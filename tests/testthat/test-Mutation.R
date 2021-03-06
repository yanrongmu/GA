############ TEST: Mutation function #########
# It mutates the kids genome after crossover

########### Last Updated by Joanne 12/6 ############

context("Mutation function")

test_that("Mutation function takes a matrix of children and mutates some genomes", {
  expect_is(Mutation(Crossover(ParentSelection(LastGen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                               X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                               Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                               FitnessLastGen = AIC(X = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                                    Y = matrix(rbinom(20, 1, 0.5), nrow = 20),
                                                                    Gen = matrix(rbinom(200, 1, 0.5), nrow = 20),
                                                                    nCores = 1),
                                               Probs = Ranking)),
                     0.5),
            "matrix")
})
