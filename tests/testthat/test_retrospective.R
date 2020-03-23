#################################
####    Test retrospective   ####
#################################

library(PRDAbeta)

#----    input checks    ----

context("retrospective inputs specification")

sample_group1_text <- "sample_group1 has to be a single integer value grater than 1."
effect_size_text <- "effect_size has to be a single integer value."
sample_group2_text <- "If specified, sample_group2 has to be a single integer value grater than 1."
sig_level_text <- "sig_level has to be a single value between 0 and 1."
B_text <- "B has to be a single integer value grater than 1."
seed_text <- "If specified, seed has to be a single finite number."
correlation_text <- "If effect_type is set to 'correlation', sample_group2 is ignored."

test_that("inputs are correctly specified", {
  expect_error(retrospective(sample_group1 = 1, effect_size = .3), sample_group1_text)
  expect_error(retrospective(sample_group1 = Inf, effect_size = .3), sample_group1_text)
  expect_error(retrospective(sample_group1 = -3, effect_size = .3), sample_group1_text)
  expect_error(retrospective(sample_group1 = "ciao", effect_size = .3), sample_group1_text)
  # expect_error(retrospective(sample_group1 = c(10,20), effect_size = .3), sample_group1_text)

  expect_error(retrospective(sample_group1 = 20, effect_size = Inf), effect_size_text)
  expect_error(retrospective(sample_group1 = 20, effect_size = "ciao"), effect_size_text)
  # expect_error(retrospective(sample_group1 = 20, effect_size = c(.5,.7)), effect_size_text)

  expect_error(retrospective(sample_group1 = 20, sample_group2 = 1, effect_size = .3), sample_group2_text)
  expect_error(retrospective(sample_group1 = 20, sample_group2 = Inf, effect_size = .3), sample_group2_text)
  expect_error(retrospective(sample_group1 = 20, sample_group2 = -3, effect_size = .3), sample_group2_text)
  expect_error(retrospective(sample_group1 = 20, sample_group2 = "ciao", effect_size = .3), sample_group2_text)
  # expect_error(retrospective(sample_group1 = 20, sample_group2 = c(10,20), effect_size = .3), sample_group2_text)

  expect_error(retrospective(sample_group1 = 20, effect_size = .3, sig_level = 1), sig_level_text)
  expect_error(retrospective(sample_group1 = 20, effect_size = .3, sig_level = Inf), sig_level_text)
  expect_error(retrospective(sample_group1 = 20, effect_size = .3, sig_level = 0), sig_level_text)
  expect_error(retrospective(sample_group1 = 20, effect_size = .3, sig_level = "ciao"), sig_level_text)
  # expect_error(retrospective(sample_group1 = 20, effect_size = .3, sig_level = c(.1,.2)), sig_level_text)

  expect_error(retrospective(sample_group1 = 20, effect_size = .3, B = 1), B_text)
  expect_error(retrospective(sample_group1 = 20, effect_size = .3, B = Inf), B_text)
  expect_error(retrospective(sample_group1 = 20, effect_size = .3, B = "ciao"), B_text)
  # expect_error(retrospective(sample_group1 = 20, effect_size = .3, B = c(10,20)), B_text)

  expect_error(retrospective(sample_group1 = 20, effect_size = .3, seed = Inf), seed_text)
  expect_error(retrospective(sample_group1 = 20, effect_size = .3, seed = "ciao"), seed_text)
  # expect_error(retrospective(sample_group1 = 20, effect_size = .3, seed = c(10,20)), seed_text)

  expect_warning(retrospective(sample_group1 = 20, sample_group2 = 30, effect_size = .3, effect_type = "correlation"),
                 correlation_text)

})


#----


#----
