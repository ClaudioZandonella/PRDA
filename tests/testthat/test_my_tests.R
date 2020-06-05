#############################
####    Test my_t_test   ####
#############################

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

test_that("my_t_test gives the same p-value as t.test", {
  expect_equal(my_t_test(x,y, paired=F, var.equal=F, alternative = "two.sided")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x,y, paired=F, var.equal=F, alternative = "greater")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "greater")$p.value)
  expect_equal(my_t_test(x,y, paired=F, var.equal=F, alternative = "less")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "less")$p.value)

  expect_equal(my_t_test(x,y, paired=F, var.equal=T, alternative = "two.sided")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x,y, paired=F, var.equal=T, alternative = "greater")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "greater")$p.value)
  expect_equal(my_t_test(x,y, paired=F, var.equal=T, alternative = "less")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "less")$p.value)

  expect_equal(my_t_test(x,y, paired=T, var.equal=F, alternative = "two.sided")$p.value,
               t.test(x,y, paired=T, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x,y, paired=T, var.equal=F, alternative = "greater")$p.value,
               t.test(x,y, paired=T, var.equal=F, alternative = "greater")$p.value)
  expect_equal(my_t_test(x,y, paired=T, var.equal=F, alternative = "less")$p.value,
               t.test(x,y, paired=T, var.equal=F, alternative = "less")$p.value)

  expect_equal(my_t_test(x, alternative = "two.sided")$p.value,
               t.test(x, alternative = "two.sided")$p.value)
  expect_equal(my_t_test(x, alternative = "greater")$p.value,
               t.test(x, alternative = "greater")$p.value)
  expect_equal(my_t_test(x, alternative = "less")$p.value,
               t.test(x, alternative = "less")$p.value)
})


test_that("my_t_test gives the correct estimate", {
  # One sample t-test
  expect_equal(my_t_test(x)$estimate, mx/sd(x))
  expect_equal(my_t_test(x, mu = 10)$estimate, (mx-10)/sd(x))

  # Paired t-test (Hedge's correction)
  expect_equal(my_t_test(x,y, paired=T, var.equal=T)$estimate,
               (nx-2)/(nx-1.25)*mean(diff)/sd(diff))

  # Two samples t-test (Hedge's correction)
  expect_equal(my_t_test(x,y, paired=F, var.equal=T)$estimate,
               (1 - (3/(4*(nx+ny)-9)))*(mx-my)/pool_sd(x,y))
  # Welch t-test
  expect_equal(my_t_test(x,y, paired=F, var.equal=F)$estimate,
               (mx-my)/sqrt((var(x)+var(y))/2))
})

#----


#----
