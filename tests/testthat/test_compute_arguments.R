#####################################
####    Test compute arguments   ####
#####################################

library(PRDAbeta)

#----    input checks    ----

context("Evaluate my test results")

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


#----    compute_df    ----

test_that("evaluate the correct df", {
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2, test_method = "one_sample"), nx-1L)
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny, test_method = "paired"), nx-1L)
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2, test_method = "two_samples"), nx+ny2-2L)
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2, test_method = "welch"),
               ((nx+ny2)^2*(nx-1)*(ny2-1))/(nx^2*(nx-1)+ ny2^2*(ny2-1)))

  expect_equal(compute_df(effect_type = "correlation", sample_n1 = nx), nx-2L)
  })

#----    compute_critical_effect    ----

test_that("evaluate the correct critical effect", {
  expect_equal(compute_critical_effect(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2,
                                       test_method = "one_sample", sig_level = .10, alternative = "two.sided"),
               list(df = nx-1, critical_effect = qt(1-sig_level/2, df = nx-1)/sqrt(nx)))

  expect_equal(compute_critical_effect(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny,
                                       test_method = "paired", sig_level = .10, alternative = "less"),
               list(df = nx-1, critical_effect = qt(sig_level, df = nx-1)/sqrt(nx)))
  expect_equal(compute_critical_effect(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny,
                                       test_method = "paired", sig_level = .10, alternative = "greater", mu = mu),
               list(df = nx-1, critical_effect = qt(1-sig_level, df = nx-1)/sqrt(nx)+mu))

  expect_equal(compute_critical_effect(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2,
                                       test_method = "two_samples", sig_level = .10, alternative = "two.sided"),
               list(df = nx+ny2-2, critical_effect = qt(1-sig_level/2, df = nx+ny2-2)*sqrt((nx+ny2)/(nx*ny2))))
  expect_equal(compute_critical_effect(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2,
                                       test_method = "two_samples", sig_level = .10, alternative = "less", mu = mu2),
               list(df = nx+ny2-2, critical_effect = qt(sig_level, df = nx+ny2-2)*sqrt((nx+ny2)/(nx*ny2)) + mu2))

  expect_equal(compute_critical_effect(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2,
                                       test_method = "welch", sig_level = .10, alternative = "greater"),
               list(df = ((nx+ny2)^2 * (nx-1) * (ny2-1))/(nx^2*(nx-1) + ny2^2*(ny2-1)),
                    critical_effect = qt(1-sig_level, df = ((nx+ny2)^2 * (nx-1) * (ny2-1))/(nx^2*(nx-1) + ny2^2*(ny2-1))
                                                        )*sqrt((nx+ny2)/(nx*ny2))))
  expect_equal(compute_critical_effect(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2,
                                       test_method = "welch", sig_level = .10, alternative = "two.sided", mu = mu2),
               list(df = ((nx+ny2)^2 * (nx-1) * (ny2-1))/(nx^2*(nx-1) + ny2^2*(ny2-1)),
                    critical_effect = qt(1-sig_level/2, df = ((nx+ny2)^2 * (nx-1) * (ny2-1))/(nx^2*(nx-1) + ny2^2*(ny2-1))
                                         )*sqrt((nx+ny2)/(nx*ny2)) + mu2))

  expect_equal(compute_critical_effect(effect_type = "correlation", sample_n1 = nx, sig_level = .10, alternative = "two.sided"),
               list(df = nx-2, critical_effect = qt(1-sig_level/2, df = nx-2)/sqrt(nx-2+qt(1-sig_level/2, df = nx-2)^2)))
  expect_equal(compute_critical_effect(effect_type = "correlation", sample_n1 = nx, sig_level = .10, alternative = "greater"),
               list(df = nx-2, critical_effect = qt(1-sig_level, df = nx-2)/sqrt(nx-2+qt(1-sig_level, df = nx-2)^2)))
  expect_equal(compute_critical_effect(effect_type = "correlation", sample_n1 = nx, sig_level = .10, alternative = "less"),
               list(df = nx-2, critical_effect = qt(sig_level, df = nx-2)/sqrt(nx-2+qt(sig_level, df = nx-2)^2)))

  })
#----    eval_effect_size    ----

test_that("evaluate the correct critical effect", {
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
