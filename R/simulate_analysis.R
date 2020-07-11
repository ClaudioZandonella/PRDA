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
                                alternative, sig_level, B, paired = FALSE, var.equal = FALSE, mu = 0, ...){


  arguments <- as.list(match.call()[-1])

  # sim_res <- replicate(B,{
  #   groups <- sample_groups(sample_n1, effect_target, sample_n2)
  #
  #   sim <- my_t_test(x = groups$x, y = groups$y,
  #                    test_method = test_method, alternative = alternative,
  #                    paired = paired, var.equal = var.equal)
  # })
  sample_n2_new <- ifelse(is.null(sample_n2), 0, sample_n2)
  sim_res <- cohen_loop(sample_n1 = sample_n1, effect_target = effect_target, sample_n2 = sample_n2_new,
                        test_method = test_method, alternative = alternative, mu = mu,  B = B)
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

  # sim_res <- replicate(B,{
  #   groups <- my_mvrnorm(sample_n1, Eigen_matrix =Eigen_matrix)
  #
  #   sim <- my_cor_test(x = groups$x, y = groups$y, alternative = alternative)
  # })

  sim_res <- cor_loop(n = sample_n1, alternative = alternative, B = B, Eigen_matrix = Eigen_matrix)

  sim_res <- list2data(sim_res)

  res_errors <- compute_errors(p.values = sim_res$p.value,
                               estimates = sim_res$estimate,
                               true_value = effect_target,
                               sig_level = sig_level, alternative = alternative, B = B)


  return(res_errors)
}

#----

