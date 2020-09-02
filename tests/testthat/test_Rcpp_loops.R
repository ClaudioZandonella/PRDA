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

# for case ratio_sd = 2
obs2 <- with_seed(2020, list(x = rnorm(20, .3, 2),
                             y = rnorm(20, 0, 1)))


#----    eval_cohen_loop    ----

# Redefine function to avoid specify arguments each the times
test_cohen_loop <- function(sample_n1 =20 ,effect_target = .3, sample_n2 = 20,
                            test_method, alternative, ratio_sd = 1, mu = 0, B = 1){
  with_seed(2020, cohen_loop(sample_n1 = sample_n1, effect_target = effect_target,
                             sample_n2 = sample_n2, test_method = test_method,
                             alternative = alternative, ratio_sd = ratio_sd, mu = mu, B = B))
}

test_that("cohen_loop gives the same p-value as t.test", {

  # Two sample t-test
  expect_equal(test_cohen_loop(test_method = "two_sample", alternative = "two_sided")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "two.sided")$p.value)
  expect_equal(test_cohen_loop(test_method = "two_sample", alternative = "greater")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "greater")$p.value)
  expect_equal(test_cohen_loop(test_method = "two_sample",alternative = "less")$p.value,
               t.test(x,y, paired=F, var.equal=T, alternative = "less")$p.value)
  expect_equal(test_cohen_loop(test_method = "two_sample",alternative = "two_sided", mu = 1.5)$p.value,
               t.test(x,y, paired=F, var.equal=T, mu = 1.5)$p.value)

  # Paired t-test
  # Note that for paired t.test the Cohen's d is calculated by dividing the mean difference
  # by the standard deviation of the difference. Thus, given the effect we directly sample
  # the difference as x and ignore y. Thus, cohen_loop is compared to t.test one sample
  expect_equal(test_cohen_loop(test_method = "paired", alternative = "two_sided")$p.value,
               t.test(x, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(test_cohen_loop(test_method = "paired", alternative = "greater")$p.value,
               t.test(x, var.equal=F, alternative = "greater")$p.value)
  expect_equal(test_cohen_loop(test_method = "paired", alternative = "less")$p.value,
               t.test(x, var.equal=F, alternative = "less")$p.value)
  expect_equal(test_cohen_loop(test_method = "paired", alternative = "two_sided", mu = 1.5)$p.value,
               t.test(x, var.equal=F, mu = 1.5)$p.value)

  # One sample t-test
  expect_equal(test_cohen_loop(test_method = "one_sample", alternative = "two_sided")$p.value,
               t.test(x, alternative = "two.sided")$p.value)
  expect_equal(test_cohen_loop(test_method = "one_sample", alternative = "greater")$p.value,
               t.test(x, alternative = "greater")$p.value)
  expect_equal(test_cohen_loop(test_method = "one_sample", alternative = "less")$p.value,
               t.test(x, alternative = "less")$p.value)
  expect_equal(test_cohen_loop(test_method = "one_sample", alternative = "two_sided", mu = 1.5)$p.value,
               t.test(x, mu = 1.5)$p.value)

  # Welch t-test case ratio_sd = 1
  expect_equal(test_cohen_loop(test_method = "welch", alternative = "two_sided")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "greater")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "greater")$p.value)
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "less")$p.value,
               t.test(x,y, paired=F, var.equal=F, alternative = "less")$p.value)
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "two_sided", mu = 1.5)$p.value,
               t.test(x,y, paired=F, var.equal=F, mu = 1.5)$p.value)
  # case ratio_sd = 2
  expect_equal(test_cohen_loop(test_method = "welch", alternative = "two_sided", ratio_sd = 2)$p.value,
               t.test(obs2$x,obs2$y, paired=F, var.equal=F, alternative = "two.sided")$p.value)
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "greater", ratio_sd = 2)$p.value,
               t.test(obs2$x,obs2$y, paired=F, var.equal=F, alternative = "greater")$p.value)
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "less", ratio_sd = 2)$p.value,
               t.test(obs2$x,obs2$y, paired=F, var.equal=F, alternative = "less")$p.value)
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "two_sided", ratio_sd = 2, mu = 1.5)$p.value,
               t.test(obs2$x,obs2$y, paired=F, var.equal=F, mu = 1.5)$p.value)
})

test_that("cohen_loop gives the correct estimate", {

  # One sample t-test
  expect_equal(test_cohen_loop(test_method = "one_sample", alternative = "two_sided")$estimate,
               mx/sd(x))
  expect_equal(test_cohen_loop(test_method = "one_sample",alternative = "two_sided", mu = 10)$estimate,
               (mx-10)/sd(x))

  # Paired t-test (Hedge's correction)
  # Note that for paired t.test the Cohen's d is calculated by dividing the mean difference
  # by the standard deviation of the difference. Thus, given the effect we directly sample
  # the difference as x and ignore y.
  expect_equal(test_cohen_loop(test_method = "paired",alternative = "two_sided")$estimate,
               (nx-2)/(nx-1.25)*mean(x)/sd(x))

  # Two samples t-test (Hedge's correction)
  expect_equal(test_cohen_loop(test_method = "two_sample",alternative = "two_sided")$estimate,
               (1 - (3/(4*(nx+ny)-9)))*(mx-my)/pool_sd(x,y))

  # Welch t-test case ratio_sd = 1
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "two_sided")$estimate,
               (mx-my)/sqrt((var(x)+var(y))/2))
  # case ratio_sd = 2
  expect_equal(test_cohen_loop(test_method = "welch",alternative = "two_sided", ratio_sd = 2)$estimate,
               (mean(obs2$x)-mean(obs2$y))/sqrt((var(obs2$x)+var(obs2$y))/2))
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
                        test_method = "two_sample",alternative = "two_sided", mu = 0, B = 1 ))$p.value,
               t.test(x_bis, y_bis, paired=F, var.equal=T, alternative = "two.sided")$p.value)

  # effect
  expect_equal(with_seed(2021, cohen_loop(sample_n1 = 20, effect_target = .3, sample_n2 = 30,
                        test_method = "two_sample",alternative = "two_sided", mu = 0, B = 1 ))$estimate,
               (1 - (3/(4*(nx_bis+ny_bis)-9)))*(mx_bis-my_bis)/pool_sd(x_bis,y_bis))

})


#----    eval_cor_loop   ----

Eigen_matrix <- compute_eigen_matrix(.3)
obs_cor <- with_seed(2020, mvrnorm(20, mu=c(0,0), Sigma=matrix(c(1,.3,.3,1),ncol=2)))


# Redefine function to avoid specify arguments each the times
test_cor_loop <- function(n = 20 , alternative, B = 1, Eigen_matrix = compute_eigen_matrix(.3)){
  with_seed(2020, cor_loop(n = n , alternative = alternative,
                           B = B, Eigen_matrix = Eigen_matrix))
}

test_that("cor_loop gives the same p-value as cor.test", {

  expect_equal(test_cor_loop(alternative = "two_sided")$p.value,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "two.sided")$p.value)
  expect_equal(test_cor_loop(alternative = "less")$p.value,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "less")$p.value)
  expect_equal(test_cor_loop(alternative = "greater")$p.value,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "greater")$p.value)
})

test_that("my_cor_test gives the correct estimate", {
  expect_equal(test_cor_loop(alternative = "two_sided")$estimate,
               cor.test(obs_cor[,1], obs_cor[,2], alternative = "two.sided")$estimate[[1]])
})


#----

