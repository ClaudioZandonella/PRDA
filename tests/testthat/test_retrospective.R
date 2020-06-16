#################################
####    Test retrospective   ####
#################################

library(PRDAbeta)

#----    input checks    ----

context("retrospective inputs specification")

test_that("inputs are correctly specified", {

  sample_n1_text <- "sample_n1 has to be a single integer value grater than 1."
  effect_size_text <- "effect_size has to be a single numeric value or a function."
  sample_n2_text <- "If specified, sample_n2 has to be a single integer value grater than 1."
  sig_level_text <- "sig_level has to be a single value between 0 and 1."
  B_text <- "B has to be a single integer value grater than 1."
  seed_text <- "If specified, seed has to be a single finite number."
  correlation_text <- "If effect_type is set to 'correlation', sample_n2 is ignored."
  conf.level_text <- "conf.level is set according to sig_level."
  paired_t.test <- "If paired = TRUE sample_n1 and sample_n2 must be equal."
  tl_text <- "tl has to be a single numeric value."
  tu_text <- "tu has to be a single numeric value."
  B_effect_text <- "B_effect has to be a single integer value grater than 1."
  mu_text <- "Desing Analysis is allowed only for  Null Hypothesis mu = 0."


  expect_error(retrospective(sample_n1 = 1, effect_size = .3), sample_n1_text)
  expect_error(retrospective(sample_n1 = Inf, effect_size = .3), sample_n1_text)
  expect_error(retrospective(sample_n1 = -3, effect_size = .3), sample_n1_text)
  expect_error(retrospective(sample_n1 = "ciao", effect_size = .3), sample_n1_text)
  expect_error(retrospective(sample_n1 = c(10,20), effect_size = .3), sample_n1_text)

  expect_error(retrospective(sample_n1 = 20, effect_size = Inf), effect_size_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = "ciao"), effect_size_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = c(.5,.7)), effect_size_text)

  expect_error(retrospective(sample_n1 = 20, sample_n2 = 1, effect_size = .3), sample_n2_text)
  expect_error(retrospective(sample_n1 = 20, sample_n2 = Inf, effect_size = .3), sample_n2_text)
  expect_error(retrospective(sample_n1 = 20, sample_n2 = -3, effect_size = .3), sample_n2_text)
  expect_error(retrospective(sample_n1 = 20, sample_n2 = "ciao", effect_size = .3), sample_n2_text)
  expect_error(retrospective(sample_n1 = 20, sample_n2 = c(10,20), effect_size = .3), sample_n2_text)

  expect_error(retrospective(sample_n1 = 20, effect_size = .3, sig_level = 1), sig_level_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, sig_level = Inf), sig_level_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, sig_level = 0), sig_level_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, sig_level = "ciao"), sig_level_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, sig_level = c(.1,.2)), sig_level_text)

  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B = 1), B_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B = Inf), B_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B = "ciao"), B_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B = c(10,20)), B_text)

  expect_error(retrospective(sample_n1 = 20, effect_size = .3, seed = Inf), seed_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, seed = "ciao"), seed_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, seed = c(10,20)), seed_text)

  expect_error(retrospective(sample_n1 = 20, effect_size = .3, tl = "ciao"), tl_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, tl = c(10,20)), tl_text)

  expect_error(retrospective(sample_n1 = 20, effect_size = .3, tu = "ciao"), tu_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, tu = c(10,20)), tu_text)

  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B_effect = 1), B_effect_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B_effect = Inf), B_effect_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B_effect = "ciao"), B_effect_text)
  expect_error(retrospective(sample_n1 = 20, effect_size = .3, B_effect = c(10,20)), B_effect_text)

  expect_warning(retrospective(sample_n1 = 20, sample_n2 = 30, effect_size = .3, effect_type = "correlation", B=10),
                 correlation_text)

  # conf.level test
  expect_warning(retrospective(sample_n1 = 20, sample_n2 = 30, effect_size = .3, effect_type = "cohen_d", conf.level=.8, B=10),
                 conf.level_text)
  expect_warning(retrospective(sample_n1 = 20, sample_n2 = 30, effect_size = .3, effect_type = "correlation", conf.level=.8, B=10),
                 conf.level_text)

  # paired_t.test
  expect_error(retrospective(sample_n1 = 20, sample_n2 = 30, effect_size = .3, effect_type = "cohen_d", paired=T),
                 paired_t.test)
  expect_error(retrospective(sample_n1 = 20, sample_n2 = NULL, effect_size = .3, effect_type = "cohen_d", paired=T),
                 paired_t.test)

  # mu
  expect_error(retrospective(effect_size = .3, sample_n1 = 20, mu = .2), mu_text)
})


#----    obtain same results    ----

test_that("same results as previous run", {
  expect_known_value(retrospective(sample_n1 = 10,effect_size = .3,effect_type = "correlation",seed = 2020)$effect_info,
                            file = "test_cache/effect_info_single", update= FALSE)
  expect_known_value(retrospective(sample_n1 = 10,effect_size = .3,effect_type = "correlation",seed = 2020)$retrospective_res,
                            file="test_cache/res_corr_single", update= FALSE)
  expect_known_value(retrospective(sample_n1 = 10,effect_size = .3,effect_type = "cohen_d",seed = 2020)$retrospective_res,
                            file="test_cache/res_cohen_single", update= FALSE)

  expect_known_value(retrospective(sample_n1 = 10, effect_size = function(x) rnorm(x), effect_type = "correlation",
                                   seed = 2020,B = 10, B_effect = 10)$effect_info, file = "test_cache/effect_info_dist", update= FALSE)
  expect_known_value(retrospective(sample_n1 = 10, effect_size = function(x) rnorm(x), effect_type = "correlation",
                                   seed = 2020, B=100, B_effect = 10)$retrospective_res, file = "test_cache/res_corr_dist", update= FALSE)
  expect_known_value(retrospective(sample_n1 = 10, effect_size = function(x) rnorm(x), effect_type = "cohen_d",
                                   seed = 2020, B=100, B_effect = 10)$retrospective_res, file = "test_cache/res_cohen_dist", update= FALSE)

  })


#----
