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
#' @param effect_target numeric value
#' @param test_method character value
#' @param alternative character value
#' @param sig_level numeric value.
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#' @importFrom stats rnorm t.test
#'
retrospective_cohen <- function(sample_n1, sample_n2, effect_target, test_method,
                                alternative, sig_level, B, ...){


  arguments <- as.list(match.call()[-1])

  sim_res <- replicate(B,{
    groups <- sample_groups(sample_n1, effect_target, sample_n2)

    sim <- do.call(my_t_test,c(groups,
                               arguments))
  })


  sim_res <- list2data(sim_res)

  res_errors <- compute_errors(p.values = sim_res$p.value,
                        estimates = sim_res$estimate,
                        true_value = effect_target,
                        sig_level = sig_level, alternative = alternative, B = B)

  return(res_errors)
  }
#----    retrospective_correlation    ----

#' Title
#'
#' @param sample_n1 numeric value
#' @param effect_target numeric value
#' @param test_method character value
#' @param alternative character value
#' @param sig_level numeric value.
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#'
#' @importFrom stats cor.test
#'
retrospective_correlation <- function(sample_n1, effect_target, test_method,
                                      alternative, sig_level, B, ...){

  arguments <- as.list(match.call()[-1])

  Eigen_matrix <- compute_eigen_matrix(effect_target = effect_target)

  sim_res <- replicate(B,{
    groups <- my_mvrnorm(sample_n1, Eigen_matrix =Eigen_matrix)

    sim <- do.call(my_cor_test,c(groups, arguments))
  })

  sim_res <- list2data(sim_res)

  res_errors <- compute_errors(p.values = sim_res$p.value,
                               estimates = sim_res$estimate,
                               true_value = effect_target,
                               sig_level = sig_level, alternative = alternative, B = B)


  return(res_errors)
}

#----

