#################################
####    simulate_analysis    ####
#################################

#----    simulate_analysis    ----



simulate_analysis <- function(effect_type, effect_samples, test_method, sample_n1, sample_n2, alternative, sig_level, B, ...){

  arguments <-  as.list(match.call()[-1])

  if(effect_type == "cohen_d"){
    # Cohen's d
    analysis_res <- sapply(effect_samples,
                           FUN = function(effect_target) do.call(retrospective_cohen,
                                                                 c(arguments,
                                                                   effect_target = effect_target)))

  } else if (effect_type == "correlation"){
    # Correlation
    analysis_res <- sapply(effect_samples,
                           FUN = function(effect_target) do.call(retrospective_correlation,
                                                                 c(arguments,
                                                                   effect_target = effect_target)))
  }

  analysis_res <- list2data(analysis_res)

  return(analysis_res)

}

#----    retrospective_cohen    ----

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

