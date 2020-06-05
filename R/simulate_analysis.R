#################################
####    simulate_analysis    ####
#################################

#----    simulate_analysis    ----

# simulate_analysis <- function(sample_n1, effect_size, sample_n2, effect_type, alternative, B, ...){
#
#
#   if (effect_type == "cohen_d"){
#     analysis_simulated <- analysis_cohen(sample_n1 = sample_n1,
#                                          sample_n2 = sample_n2,
#                                          effect_size = effect_size,
#                                          alternative = alternative,
#                                          B = B, ...)
#     } else if (effect_type == "correlation"){
#
#     analysis_simulated <- analysis_correlation(sample_n1 = sample_n1,
#                                                effect_size = effect_size,
#                                                alternative = alternative,
#                                                B = B, ...)
#     }
#
#   return(analysis_simulated)
# }

#----    retrospective_cohen    ----

#' Title
#'
#' @param sample_n1 numeric value
#' @param sample_n2 numeric value
#' @param effect_size numeric value
#' @param alternative character value
#' @param sig_level numeric value.
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#' @importFrom stats rnorm t.test
#'
retrospective_cohen <- function(sample_n1, sample_n2, effect_size, alternative, sig_level, B, ...){


  arguments <- as.list(match.call()[-1])

  # Check and get test method
  method <- do.call(check_test_method, arguments)

  # Select arguments for t.test (remove)
  arguments <- select_arguments(arguments, c("sample_n1", "sample_n2", "effect_type",
                                "effect_size", "B","sig_level", "seed"), remove = T)

  sim_res <- replicate(B,{
    groups <- sample_groups(sample_n1, effect_size, sample_n2)

    sim <- do.call(my_t_test,c(groups,arguments))
  })


  sim_res <- list2data(sim_res)

  return(sim_res)
  }
#----    retrospective_correlation    ----

#' Title
#'
#' @param sample_n1 numeric value
#' @param effect_size numeric value
#' @param alternative character value
#' @param sig_level numeric value.
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#' @importFrom MASS mvrnorm
#' @importFrom stats cor.test
#'
retrospective_correlation <- function(sample_n1, effect_size, alternative, sig_level, B, ...){

  arguments <- as.list(match.call()[-1])

  # Select arguments for cor.test (remove)
  arguments <- select_arguments(arguments, c("sample_n1", "sample_n2",
                                "effect_size", "B", "sig_level", "seed"), remove = T)

  res <- replicate(B,{
    obs<-mvrnorm(n = sample_n1, mu=c(0,0), Sigma=matrix(c(1, effect_size, effect_size, 1), ncol=2))
    arguments$x <- obs[,1]
    arguments$y <- obs[,2]

    do.call(cor.test,arguments)
  })

  return(res)
}

#----

