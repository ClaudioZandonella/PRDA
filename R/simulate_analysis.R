#################################
####    simulate_analysis    ####
#################################

#----    simulate_analysis    ----

simulate_analysis <- function(effect_type, effect_samples, test_method,
                              sample_n1, sample_n2, alternative, sig_level,
                              ratio_sd, B, ...){

  arguments <- as.list(match.call()[-1])

  if(effect_type == "cohen_d"){
    # Cohen's d
    analysis_res <- vapply(effect_samples,
                          FUN = function(effect_target)
                            do.call(retrospective_cohen,
                                    c(arguments,
                                      effect_target = effect_target)),
                          FUN.VALUE = list(power = 0, typeM = 0, typeS = 0))

  } else if (effect_type == "correlation"){
    # Correlation
    analysis_res <- vapply(effect_samples,
                          FUN = function(effect_target)
                            do.call(retrospective_correlation,
                                    c(arguments,
                                      effect_target = effect_target)),
                          FUN.VALUE = list(power = 0, typeM = 0, typeS = 0))
  }

  analysis_res <- list2data(analysis_res)

  return(analysis_res)

}


#----    retrospective_cohen    ----

retrospective_cohen <- function(sample_n1, sample_n2, effect_target,
                                test_method, alternative, sig_level, ratio_sd,
                                B, mu = 0, ...){

  # Get the correct mean difference from the effect value
  correct_diff <- compute_correct_diff(effect_target, test_method, ratio_sd)

  # C++ do not use NULL so set to 0
  sample_n2_new <- ifelse(is.null(sample_n2), 0, sample_n2)

  sim_res <- cohen_loop(sample_n1 = sample_n1, mean_diff = correct_diff,
                        sample_n2 = sample_n2_new, test_method = test_method,
                        alternative = alternative, ratio_sd = ratio_sd,
                        mu = mu,  B = B)
  sim_res <- list2data(sim_res)

  res_errors <- compute_errors(p_values = sim_res$p_value,
                               estimates = sim_res$estimate,
                               true_value = effect_target,
                               sig_level = sig_level, B = B)

  return(res_errors)
}


#----    retrospective_correlation    ----

retrospective_correlation <- function(sample_n1, effect_target, test_method,
                                      alternative, sig_level, B, ...){

  Eigen_matrix <- compute_eigen_matrix(effect_target = effect_target)

  sim_res <- cor_loop(n = sample_n1, alternative = alternative, B = B,
                      Eigen_matrix = Eigen_matrix)

  sim_res <- list2data(sim_res)

  res_errors <- compute_errors(p_values = sim_res$p_value,
                               estimates = sim_res$estimate,
                               true_value = effect_target,
                               sig_level = sig_level, B = B)

  return(res_errors)
}


#----

