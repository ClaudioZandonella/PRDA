#######################################
####    Test sample observations   ####
#######################################

library(PRDAbeta)

#----    input checks    ----

context("Evaluate functions to sample observations")

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

error_truncation <- "'tl' has to be greater than 'tu'."
message_truncation <- "Truncation could require long computational time."
error_tol <- "Truncation requires too long computational time, consider possible misspecification."


#----    eval_my_mvrnorm    ----

test_that("evaluate my_mvrnorm", {


  Eigen_matrix <- compute_eigen_matrix(effect_target = .3)

  expect_equal(with_seed(2020, my_mvrnorm(30, Eigen_matrix =Eigen_matrix)$x),
               with_seed(2020, mvrnorm(n=30,mu=c(0,0),Sigma=matrix(c(1,.3,.3,1),ncol=2))[,1]))
  expect_equal(with_seed(2020, my_mvrnorm(30, Eigen_matrix =Eigen_matrix)$y),
               with_seed(2020, mvrnorm(n=30,mu=c(0,0),Sigma=matrix(c(1,.3,.3,1),ncol=2))[,2]))

})

#----    sample_effect    ----
test_that("evaluate sample_effect", {
  expect_equal(with_seed(2020, sample_effect(FUN = function(x) rnorm(x), B_effect = 100)$effect_samples),
               with_seed(2020, rnorm(100)))

  expect_error(sample_effect(FUN = function(x,y) rnorm(x,y), B_effect = 100))
  expect_error(sample_effect(FUN = "ciao", B_effect = 100))

  expect_message(sample_effect(FUN = function(x) rnorm(x), B_effect = 100, tl=0), message_truncation)
  expect_error(sample_effect(FUN = function(x) rnorm(x), B_effect = 100, tl=0, tu=-1), error_truncation)

  expect_error(sample_effect(function(x)runif(x), B_effect = 100, tl=2), error_tol)
  })
sample_effect
#----
