############ TEST: Initialize function #########
########### Last Updated by Joanne 12/6 ############

context("Initialize function")

test_that("Initialize creates a p by P matrix of 1s and 0s", {
  expect_is(Initialize(5,8), "matrix")
  expect_length(Initialize(5,8), 40)
  expect_equal(max(Initialize(5,8)), 1)
  expect_equal(min(Initialize(5,8)), 0)
})
