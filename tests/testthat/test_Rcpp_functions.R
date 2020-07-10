##################################
####    Test Rcpp Functions   ####
##################################

library(PRDAbeta)

#----    input checks    ----

context("Evaluate Rcpp functions")


x <- rnorm(3)
y <- rnorm(3)

x_bis <- rnorm(30)
y_bis <- rnorm(30)

#----    eval_meanC    ----

test_that("evaluate meanC", {
  expect_equal(mean(x), meanC(x))
  expect_equal(mean(y), meanC(y))
  expect_equal(mean(x_bis), meanC(x_bis))
  expect_equal(mean(y_bis), meanC(y_bis))
})

#----    eval_varC    ----

test_that("evaluate varC", {
  xm <- mean(x)
  ym <- mean(y)
  xm_bis <- mean(x_bis)
  ym_bis <- mean(y_bis)

  expect_equal(var(x), varC(x))
  expect_equal(var(y), varC(y))
  expect_equal(var(x_bis), varC(x_bis))
  expect_equal(var(y_bis), varC(y_bis))
})

#----    eval_corC    ----

test_that("evaluate corC", {
  expect_equal(cor(x,y), corC(x, y))
  expect_equal(cor(x_bis, y_bis), corC(x_bis, y_bis))
})
#----
