######################################
####    Test evaluate arguments   ####
######################################

library(PRDA)

#----    input checks    ----

context("Evaluate arguments")

x <- c(-32, -49, 157, 116, 114, 170, 162, 220, -55, 97, 151, 160, 145, 78, 116)
y <- c(-30, 102, 184, 28, -89, -40, -43, -129, 53, 119, 44, 35, 189, 137, -68)
diff <- x-y
mx <- mean(x)
my <- mean(y)
nx <- length(x)
ny <- length(y)
ny2 <- ny+2
sig_level = .10
mu = .5
mu2 = -.5

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

#----    eval_test_method    ----

test_that("evaluate the correct test method", {
  expect_match(eval_test_method(effect_type = "cohen_d", effect_target = .3, sample_n1 = nx, sample_n2 = NULL), "one_sample")
  expect_match(eval_test_method(effect_type = "cohen_d", effect_target = .3, sample_n1 = nx, sample_n2 = ny, paired=TRUE), "paired")
  expect_match(eval_test_method(effect_type = "cohen_d", effect_target = .3, sample_n1 = nx, sample_n2 = ny, var.equal=TRUE), "two_samples")
  expect_match(eval_test_method(effect_type = "cohen_d", effect_target = .3, sample_n1 = nx, sample_n2 = ny), "welch")

  expect_match(eval_test_method(effect_type = "correlation", effect_target = .3, sample_n1 = nx, sample_n2 = ny), "pearson")
  expect_match(eval_test_method(effect_type = "correlation", effect_target = .3, sample_n1 = nx, method = "pearson"), "pearson")

  error_spearman <- "correlation with method = 'spearman' is not implemented. Only method = 'pearson' is available."
  expect_error(eval_test_method(effect_type = "correlation", effect_target = .3, sample_n1 = nx, method = "spearman"), error_spearman)
  error_kendall <- "correlation with method = 'kendall' is not implemented. Only method = 'pearson' is available."
  expect_error(eval_test_method(effect_type = "correlation", effect_target = .3, sample_n1 = nx, method = "kendall"), error_kendall)
})



#----    eval_effect_size    ----

test_that("evaluate the correct effect size", {
  expect_equal(eval_effect_size(effect_type = "correlation", effect_size = .3)$effect_samples, .3)
  expect_equal(eval_effect_size(effect_type = "correlation", effect_size = .3)$effect_summary[["Mean"]], .3)
  expect_equal(with_seed(2020,eval_effect_size(effect_type = "cohen_d", effect_size = function(x) rnorm(x),B_effect = 100)$effect_samples),
               with_seed(2020, rnorm(100)))

  error_effect_corr <- "If 'correlation' effect_size must be between -1 and 1"
  expect_error(eval_effect_size(effect_type = "correlation", effect_size = 2), error_effect_corr)
  message_trunc_cor <- "If 'correlation' effect_size distribution is truncated between -1 and 1\n"
  message_trunc <- "Truncation could require long computational time.\n"
  res <- evaluate_promise(eval_effect_size(effect_type = "correlation", effect_size = function(x) rnorm(x),B_effect = 100))
  expect_equal(res$messages[1], message_trunc_cor)
  expect_equal(res$messages[2], message_trunc)
})


#----
