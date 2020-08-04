##############################
####    Test Rcpp loops   ####
##############################

library(PRDAbeta)

#----    input checks    ----

context("Evaluate Rcpp loops")



pool_sd <- function(x, y){
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)

  sqrt((sum((x-mx)^2)+sum((y-my)^2))/(nx + ny -2))
}

obs <- with_seed(2020, list(x = rnorm(20, .3, 1),
                            y = rnorm(20, 0, 1)))

x <- obs$x
y <- obs$y
diff <- x-y
mx <- mean(x)
my <- mean(y)
nx <- length(x)
ny <- length(y)


#----    eval_cohen_loop    ----

test_that("cohen_loop gives the same p-value as t.test", {

  # Welch t-test
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "welch",alternative = "two.sided", mu = 0, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "welch",alternative = "greater", mu = 0, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "greater")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "welch",alternative = "less", mu = 0, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "less")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "welch",alternative = "two.sided", mu = 1.5, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=F, mu = 1.5)$p.value)

  # Two sample t-test
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "two_samples",alternative = "two.sided", mu = 0, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "two.sided")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "two_samples",alternative = "greater", mu = 0, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "greater")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "two_samples",alternative = "less", mu = 0, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "less")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "two_samples",alternative = "two.sided", mu = 1.5, B = 1 ))$p.value,
               t.test(x,y, paired=F, var.equal=T, mu = 1.5)$p.value)

  # Paired t-test
  # Note that for paired t.test the Cohen's d is calculated by dividing the mean difference
  # by the standard deviation of the difference. Thus, given the effect we directly sample
  # the difference as x and ignore y. Thus, cohen_loop is compared to t.test one sample
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "paired",alternative = "two.sided", mu = 0, B = 1 ))$p.value,
               t.test(x, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "paired",alternative = "greater", mu = 0, B = 1 ))$p.value,
               t.test(x, var.equal=F, alternative = "greater")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "paired",alternative = "less", mu = 0, B = 1 ))$p.value,
               t.test(x, var.equal=F, alternative = "less")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "paired",alternative = "two.sided", mu = 1.5, B = 1 ))$p.value,
               t.test(x, var.equal=F, mu = 1.5)$p.value)

  # One sample t-test
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 0,
                        test_method = "one_sample",alternative = "two.sided", mu = 0, B = 1 ))$p.value,
               t.test(x, alternative = "two.sided")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 0,
                        test_method = "one_sample",alternative = "greater", mu = 0, B = 1 ))$p.value,
               t.test(x, alternative = "greater")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 0,
                        test_method = "one_sample",alternative = "less", mu = 0, B = 1 ))$p.value,
               t.test(x, alternative = "less")$p.value)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 0,
                        test_method = "one_sample",alternative = "two.sided", mu = 1.5, B = 1 ))$p.value,
               t.test(x, mu = 1.5)$p.value)
})

test_that("cohen_loop gives the correct estimate", {

  # One sample t-test
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "one_sample",alternative = "two.sided", mu = 0, B = 1 ))$estimate,
               mx/sd(x))
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "one_sample",alternative = "two.sided", mu = 10, B = 1 ))$estimate,
               (mx-10)/sd(x))

  # Paired t-test (Hedge's correction)
  # Note that for paired t.test the Cohen's d is calculated by dividing the mean difference
  # by the standard deviation of the difference. Thus, given the effect we directly sample
  # the difference as x and ignore y.
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "paired",alternative = "two.sided", mu = 0, B = 1 ))$estimate,
               (nx-2)/(nx-1.25)*mean(x)/sd(x))

  # Two samples t-test (Hedge's correction)
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "two_samples",alternative = "two.sided", mu = 0, B = 1 ))$estimate,
               (1 - (3/(4*(nx+ny)-9)))*(mx-my)/pool_sd(x,y))

  # Welch t-test
  expect_equal(with_seed(2020, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 20,
                        test_method = "welch",alternative = "two.sided", mu = 0, B = 1 ))$estimate,
               (mx-my)/sqrt((var(x)+var(y))/2))
})


test_that("cohen_loop works for different sample_n2", {

  obs_bis <- with_seed(2021, list(x = rnorm(20, .3, 1),
                                  y = rnorm(30, 0, 1)))
  x_bis <- obs_bis$x
  y_bis <- obs_bis$y
  mx_bis <- mean(x_bis)
  my_bis <- mean(y_bis)
  nx_bis <- length(x_bis)
  ny_bis <- length(y_bis)

  # p.value
  expect_equal(with_seed(2021, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 30,
                        test_method = "two_samples",alternative = "two.sided", mu = 0, B = 1 ))$p.value,
               t.test(x_bis, y_bis, paired=F, var.equal=T, alternative = "two.sided")$p.value)

  # effect
  expect_equal(with_seed(2021, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 30,
                        test_method = "two_samples",alternative = "two.sided", mu = 0, B = 1 ))$estimate,
               (1 - (3/(4*(nx_bis+ny_bis)-9)))*(mx_bis-my_bis)/pool_sd(x_bis,y_bis))

})


#----    eval_cor_loop   ----

Eigen_matrix <- compute_eigen_matrix(.3)
obs_cor <- with_seed(2020, mvrnorm(20, mu=c(0,0), Sigma=matrix(c(1,.3,.3,1),ncol=2)))

test_that("cor_loop gives the same p-value as cor.test", {

  expect_equal(with_seed(2020, cor_loop(n = 20, alternative = "two.sided",
                                        B = 1, Eigen_matrix = Eigen_matrix))$p.value,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "two.sided")$p.value)
  expect_equal(with_seed(2020, cor_loop(n = 20, alternative = "less",
                                        B = 1, Eigen_matrix = Eigen_matrix))$p.value,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "less")$p.value)
  expect_equal(with_seed(2020, cor_loop(n = 20, alternative = "greater",
                                        B = 1, Eigen_matrix = Eigen_matrix))$p.value,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "greater")$p.value)
})

test_that("my_cor_test gives the correct estimate", {
  expect_equal(with_seed(2020, cor_loop(n = 20, alternative = "two.sided",
                                        B = 1, Eigen_matrix = Eigen_matrix))$estimate,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "two.sided")$estimate[[1]])
})


#----

