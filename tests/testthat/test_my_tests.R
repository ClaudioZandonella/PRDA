############################
####    Test my_tests   ####
############################

library(PRDAbeta)

#----    evaluate my tests    ----

context("Evaluate my test results")

x <- c(-32, -49, 157, 116, 114, 170, 162, 220, -55, 97, 151, 160, 145, 78, 116)
y <- c(-30, 102, 184, 28, -89, -40, -43, -129, 53, 119, 44, 35, 189, 137, -68)
diff <- x-y
mx <- mean(x)
my <- mean(y)
nx <- length(x)
ny <- length(y)

#----    my_t_test    ----

test_that("my_t_test gives the same p-value as t.test", {
  expect_equal(my_t_test(x,y, test_method = "welch", df = NULL, alternative = "two.sided")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x,y, test_method = "welch", df = NULL, alternative = "greater")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "greater")$p.value)
  expect_equal(my_t_test(x,y, test_method = "welch", df = NULL, alternative = "less")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "less")$p.value)
  expect_equal(my_t_test(x,y, test_method = "welch", df = NULL, mu = 30)$p.value,
               t.test(x,y, paired=F, var.equal=F, mu = 30)$p.value)


  expect_equal(my_t_test(x,y, test_method = "two_samples", df = nx+ny-2, alternative = "two.sided")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x,y, test_method = "two_samples", df = nx+ny-2, alternative = "greater")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "greater")$p.value)
  expect_equal(my_t_test(x,y, test_method = "two_samples", df = nx+ny-2, alternative = "less")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "less")$p.value)
  expect_equal(my_t_test(x,y, test_method = "two_samples", df = nx+ny-2, mu = 30)$p.value,
               t.test(x,y, paired=F, var.equal=T, mu = 30)$p.value)

  expect_equal(my_t_test(x,y,  test_method = "paired", df = nx-1, alternative = "two.sided")$p.value,
               t.test(x,y, paired=T, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x,y, test_method = "paired", df = nx-1, alternative = "greater")$p.value,
               t.test(x,y, paired=T, var.equal=F, alternative = "greater")$p.value)
  expect_equal(my_t_test(x,y, test_method = "paired", df = nx-1, alternative = "less")$p.value,
               t.test(x,y, paired=T, var.equal=F, alternative = "less")$p.value)
  expect_equal(my_t_test(x,y, test_method = "paired", df = nx-1, mu = 30)$p.value,
               t.test(x,y, paired=T, var.equal=F, mu = 30)$p.value)

  expect_equal(my_t_test(x, test_method = "one_sample", df = nx-1, alternative = "two.sided")$p.value,
               t.test(x, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x, test_method = "one_sample", df = nx-1, alternative = "greater")$p.value,
               t.test(x, alternative = "greater")$p.value)
  expect_equal(my_t_test(x, test_method = "one_sample", df = nx-1, alternative = "less")$p.value,
               t.test(x, alternative = "less")$p.value)
  expect_equal(my_t_test(x, test_method = "one_sample", df = nx-1, mu = 30)$p.value,
               t.test(x, mu = 30)$p.value)
})


test_that("my_t_test gives the correct estimate", {
  # One sample t-test
  expect_equal(my_t_test(x, test_method = "one_sample", df = nx-1)$estimate, mx/sd(x))
  expect_equal(my_t_test(x, test_method = "one_sample", df = nx-1, mu = 10)$estimate, (mx-10)/sd(x))

  # Paired t-test (Hedge's correction)
  expect_equal(my_t_test(x,y, test_method = "paired", df = nx-1)$estimate,
               (nx-2)/(nx-1.25)*mean(diff)/sd(diff))

  # Two samples t-test (Hedge's correction)
  expect_equal(my_t_test(x,y, test_method = "two_samples", df = nx+ny-2)$estimate,
               (1 - (3/(4*(nx+ny)-9)))*(mx-my)/pool_sd(x,y))
  # Welch t-test
  expect_equal(my_t_test(x,y, test_method = "welch", df = NULL)$estimate,
               (mx-my)/sqrt((var(x)+var(y))/2))
})

#----    my_cor_test    ----

test_that("my_cor_test gives the same p-value as cor.test", {
  expect_equal(my_cor_test(x,y, alternative = "two.sided")$p.value,
               cor.test(x,y, alternative = "two.sided")$p.value)
  expect_equal(my_cor_test(x,y, alternative = "less")$p.value,
               cor.test(x,y, alternative = "less")$p.value)
  expect_equal(my_cor_test(x,y, alternative = "greater")$p.value,
               cor.test(x,y, alternative = "greater")$p.value)
})

test_that("my_cor_test gives the correct estimate", {
  expect_equal(my_cor_test(x,y, alternative = "two.sided")$estimate,
               cor.test(x,y, alternative = "two.sided")$estimate[[1]])
  })


#----
