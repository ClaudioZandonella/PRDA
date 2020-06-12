###############################
####    Test prospective   ####
###############################

library(PRDAbeta)

#----    input checks    ----

context("prospective inputs specification")




test_that("inputs are correctly specified", {

  effect_size_text <- "effect_size has to be a single numeric value or a function."
  power_text <- "power has to be a single value between 0 and 1."
  ratio_n2_text <- "ratio_n2 has to be a single integer value grater or equal than 1."
  sig_level_text <- "sig_level has to be a single value between 0 and 1."
  B_text <- "B has to be a single integer value grater than 1."
  seed_text <- "If specified, seed has to be a single finite number."
  tl_text <- "tl has to be a single numeric value."
  tu_text <- "tu has to be a single numeric value."
  B_effect_text <- "B_effect has to be a single integer value grater than 1."
  sample_range_text1 <- "sample_range has to be a two length numeric vector."
  sample_range_text2 <- "sample_range minimum has to be grater than 1 and less than sample range maximum."
  tol_text <- "tol has to be a single value between 0 and 1."
  display_text <- "display_message has to be logical."

  correlation_text <- "If effect_type is set to 'correlation', sample_n2 is ignored."
  conf.level_text <- "conf.level is set according to sig_level."
  paired_t.test <- "If paired = TRUE sample_n1 and sample_n2 must be equal."


  expect_error(prospective(effect_size = Inf, power = .8), effect_size_text)
  expect_error(prospective(effect_size = "ciao", power = .8), effect_size_text)
  expect_error(prospective(effect_size = c(1,2), power = .8), effect_size_text)

  expect_error(prospective(effect_size = .3, power = Inf), power_text)
  expect_error(prospective(effect_size = .3, power = "ciao"), power_text)
  expect_error(prospective(effect_size = .3, power = c(.5,.6)), power_text)
  expect_error(prospective(effect_size = .3, power = 1), power_text)
  expect_error(prospective(effect_size = .3, power = -1), power_text)

  expect_error(prospective(effect_size = .3, power = .8, ratio_n2 = Inf), ratio_n2_text)
  expect_error(prospective(effect_size = .3, power = .8, ratio_n2 = "ciao"), ratio_n2_text)
  expect_error(prospective(effect_size = .3, power = .8, ratio_n2 = c(.5,.6)), ratio_n2_text)
  expect_error(prospective(effect_size = .3, power = .8, ratio_n2 = .5), ratio_n2_text)

  expect_error(prospective(effect_size = .3, power = .8, sig_level = 1), sig_level_text)
  expect_error(prospective(effect_size = .3, power = .8, sig_level = Inf), sig_level_text)
  expect_error(prospective(effect_size = .3, power = .8, sig_level = 0), sig_level_text)
  expect_error(prospective(effect_size = .3, power = .8, sig_level = "ciao"), sig_level_text)
  expect_error(prospective(effect_size = .3, power = .8, sig_level = c(.1,.2)), sig_level_text)

  expect_error(prospective(effect_size = .3, power = .8, B = 1), B_text)
  expect_error(prospective(effect_size = .3, power = .8, B = Inf), B_text)
  expect_error(prospective(effect_size = .3, power = .8, B = "ciao"), B_text)
  expect_error(prospective(effect_size = .3, power = .8, B = c(10,20)), B_text)

  expect_error(prospective(effect_size = .3, power = .8, seed = Inf), seed_text)
  expect_error(prospective(effect_size = .3, power = .8, seed = "ciao"), seed_text)
  expect_error(prospective(effect_size = .3, power = .8, seed = c(10,20)), seed_text)

  expect_error(prospective(effect_size = .3, power = .8, tl = "ciao"), tl_text)
  expect_error(prospective(effect_size = .3, power = .8, tl = c(10,20)), tl_text)

  expect_error(prospective(effect_size = .3, power = .8, tu = "ciao"), tu_text)
  expect_error(prospective(effect_size = .3, power = .8, tu = c(10,20)), tu_text)

  expect_error(prospective(effect_size = .3, power = .8, B_effect = 1), B_effect_text)
  expect_error(prospective(effect_size = .3, power = .8, B_effect = Inf), B_effect_text)
  expect_error(prospective(effect_size = .3, power = .8, B_effect = "ciao"), B_effect_text)
  expect_error(prospective(effect_size = .3, power = .8, B_effect = c(10,20)), B_effect_text)

  expect_error(prospective(effect_size = .3, power = .8, sample_range = 1), sample_range_text1)
  expect_error(prospective(effect_size = .3, power = .8, sample_range = c(10,20,20)), sample_range_text1)
  expect_error(prospective(effect_size = .3, power = .8, sample_range = c(10,"20")), sample_range_text1)
  expect_error(prospective(effect_size = .3, power = .8, sample_range = c(10,Inf)), sample_range_text1)
  expect_error(prospective(effect_size = .3, power = .8, sample_range = c(10,8)), sample_range_text2)
  expect_error(prospective(effect_size = .3, power = .8, sample_range = c(1,8)), sample_range_text2)

  expect_error(prospective(effect_size = .3, power = .8, tol = 1), tol_text)
  expect_error(prospective(effect_size = .3, power = .8, tol = Inf), tol_text)
  expect_error(prospective(effect_size = .3, power = .8, tol = 0), tol_text)
  expect_error(prospective(effect_size = .3, power = .8, tol = "ciao"), tol_text)
  expect_error(prospective(effect_size = .3, power = .8, tol = c(.1,.2)), tol_text)

  expect_error(prospective(effect_size = .3, power = .8, display_message = 1), display_text)
  expect_error(prospective(effect_size = .3, power = .8, display_message = Inf), display_text)
  expect_error(prospective(effect_size = .3, power = .8, display_message = NULL), display_text)
  expect_error(prospective(effect_size = .3, power = .8, display_message = "ciao"), display_text)
  expect_error(prospective(effect_size = .3, power = .8, display_message = c(.1,.2)), display_text)





   # expect_warning(retrospective(sample_n1 = 20, sample_n2 = 30, effect_size = .3, effect_type = "correlation", B=10),
   #               correlation_text)

  # conf.level test
  expect_warning(prospective(effect_size = .3, power = .8, effect_type = "cohen_d", conf.level=.8, B=10),
                 conf.level_text)
  expect_warning(prospective(effect_size = .3, power = .8, effect_type = "correlation", conf.level=.8, B=10),
                 conf.level_text)

  # # paired_t.test
  # expect_error(retrospective(sample_n1 = 20, sample_n2 = 30, effect_size = .3, effect_type = "cohen_d", paired=T),
  #              paired_t.test)
  # expect_error(retrospective(sample_n1 = 20, sample_n2 = NULL, effect_size = .3, effect_type = "cohen_d", paired=T),
  #              paired_t.test)
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
                                   seed = 2020,B = 10, B_effect = 10)$effect_info, file = "test_cache/effect_info_dist")
  expect_known_value(retrospective(sample_n1 = 10, effect_size = function(x) rnorm(x), effect_type = "correlation",
                                   seed = 2020, B=100, B_effect = 10)$retrospective_res, file = "test_cache/res_corr_dist")
  expect_known_value(retrospective(sample_n1 = 10, effect_size = function(x) rnorm(x), effect_type = "cohen_d",
                                   seed = 2020, B=100, B_effect = 10)$retrospective_res, file = "test_cache/res_cohen_dist")

})


#----
