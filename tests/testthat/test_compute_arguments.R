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

#----    eval_test_method    ----

test_that("evaluate the correct test method", {
  expect_match(eval_test_method(effect_type = "cohen_d", effect_size = .3, sample_n1 = nx, sample_n2 = NULL), "one_sample")
  expect_match(eval_test_method(effect_type = "cohen_d", effect_size = .3, sample_n1 = nx, sample_n2 = ny, paired=TRUE), "paired")
  expect_match(eval_test_method(effect_type = "cohen_d", effect_size = .3, sample_n1 = nx, sample_n2 = ny, var.equal=TRUE), "two_samples")
  expect_match(eval_test_method(effect_type = "cohen_d", effect_size = .3, sample_n1 = nx, sample_n2 = ny), "welch")
})


#-----

test_that("evaluate the correct df", {
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2, method = "one_sample"), nx-1)
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny, method = "paired"), nx-1)
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2, method = "two_samples"), nx+ny2-2)
  expect_equal(compute_df(effect_type = "cohen_d", sample_n1 = nx, sample_n2 = ny2, method = "welch"),
               ((nx+ny2)^2*(nx-1)*(ny2-1))/(nx^2*(nx-1)+ ny2^2*(ny2-1)))
  })
#----
