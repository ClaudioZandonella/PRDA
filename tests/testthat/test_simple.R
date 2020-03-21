###########################
####    Simple Test    ####
###########################

#----    hello    ----
context("Hello function")
library(PRDAbeta)

test_that("hello returns a string", {
  expect_true(is.character(capture.output(hello(),file = NULL)))
})


context("simple_sum function")

test_that("simple_sum is correct",{
  expect_equal(simple_sum(4,6), 10)
})


#----
