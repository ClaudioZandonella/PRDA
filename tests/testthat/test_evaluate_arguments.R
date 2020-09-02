######################################
####    Test evaluate arguments   ####
######################################

library(PRDAbeta)

#----    input checks    ----

context("Evaluate arguments")

groups <- with_seed(2020, list(x = rnorm(15, .3),
                            y = rnorm(15)))
diff <- groups$x- groups$y
mx <- mean(groups$x)
my <- mean(groups$y)
nx <- length(groups$x)
ny <- length(groups$y)
ny2 <- ny+2
sig_level = .10
mu = .5
mu2 = -.5




#----    eval_test_method    ----

test_that("evaluate the correct test method", {

  # Redefine function to avoid specify arguments each the times
  test_eval_test_method <- function(effect_type = "cohen_d", effect_target = .3,
                                    test_method, sample_n1 = nx, sample_n2 = ny,
                                    sig_level = .05, alternative = "two_sided",
                                    ratio_sd = 1){
    with_seed(2020, eval_test_method(effect_type = effect_type, effect_target = effect_target,
                     test_method = test_method, sample_n1 = sample_n1,
                     sample_n2 = sample_n2, sig_level = sig_level,
                     alternative = alternative, ratio_sd = ratio_sd))
  }

  # Cohen's d
  expect_equal(test_eval_test_method(test_method = "one_sample", sample_n2 = NULL),
               t.test(groups$x))
  expect_equal(test_eval_test_method(test_method = "paired", sample_n2 = ny),
               t.test(groups$x, groups$y, paired = TRUE))
  expect_equal(test_eval_test_method(test_method = "two_sample", sample_n2 = ny),
               t.test(groups$x, groups$y, var.equal = TRUE))
  expect_equal(test_eval_test_method(test_method = "welch", sample_n2 = ny),
               t.test(groups$x, groups$y))

  # welch and ratio_n2
  groups = with_seed(2020, list(x = rnorm(15, .3, 1.5),
                                 y = rnorm(15, 0, 1)))
  expect_equal(test_eval_test_method(test_method = "welch", sample_n2 = ny, ratio_sd = 1.5),
               t.test(groups$x, groups$y))

  #Correlation
  groups = with_seed(2020, sample_obs_cor(nx, .3))
  expect_equal(test_eval_test_method(effect_type = "correlation", test_method = "pearson", sample_n2 = ny),
               cor.test(groups$x, groups$y))
})



#----    eval_effect_size    ----

test_that("evaluate the correct effect size", {
  expect_equal(eval_effect_size(effect_type = "correlation", effect_size = .3)$effect_samples, .3)
  expect_equal(eval_effect_size(effect_type = "correlation", effect_size = .3)$effect_summary[["Mean"]], .3)
  expect_equal(with_seed(2020,eval_effect_size(effect_type = "cohen_d", effect_size = function(x) rnorm(x),B_effect = 100)$effect_samples),
               with_seed(2020, rnorm(100)))

  error_effect_corr <- "If 'effect_type = correlation', argument 'effect_size' must be between -1 and 1"
  expect_error(eval_effect_size(effect_type = "correlation", effect_size = 2), error_effect_corr)
  message_trunc_cor <- "If 'effect_type = correlation', effect_size distribution is truncated between -1 and 1\n"
  message_trunc <- "Truncation could require long computational time.\n"
  res <- evaluate_promise(eval_effect_size(effect_type = "correlation", effect_size = function(x) rnorm(x),B_effect = 100))
  expect_equal(res$messages[1], message_trunc_cor)
  expect_equal(res$messages[2], message_trunc)
})


#----
