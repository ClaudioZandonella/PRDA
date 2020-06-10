#######################################
####    Test sample observations   ####
#######################################

library(PRDAbeta)

#----    input checks    ----

context("Evaluate functions to sample observations")


#----    eval_my_mvrnorm    ----

test_that("evaluate my_mvrnorm", {
  with_seed <- function(seed, code) {
    code <- substitute(code)
    orig.seed <- .Random.seed
    on.exit(.Random.seed <<- orig.seed)
    set.seed(seed)
    eval.parent(code)
  }

  Eigen_matrix <- compute_eigen_matrix(effect_size = .3)

  expect_equal(with_seed(2020, my_mvrnorm(30, Eigen_matrix =Eigen_matrix)$x),
               with_seed(2020, mvrnorm(n=30,mu=c(0,0),Sigma=matrix(c(1,.3,.3,1),ncol=2))[,1]))
  expect_equal(with_seed(2020, my_mvrnorm(30, Eigen_matrix =Eigen_matrix)$y),
               with_seed(2020, mvrnorm(n=30,mu=c(0,0),Sigma=matrix(c(1,.3,.3,1),ncol=2))[,2]))

})

#----
