#####################################
####    Test compute arguments   ####
#####################################

library(PRDAbeta)

#----    input checks    ----

context("Compute arguments")

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

#----
