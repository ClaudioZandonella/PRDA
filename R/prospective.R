###########################
####    Prospective    ####
###########################

#----    Prospective    ----


prospective <- function(effect_size,
                        power,
                        ratio_n2 = 1,
                        effect_type = c("cohen_d","correlation"),
                        alternative = c("two.sided","less","greater"),
                        sig_level = .05,
                        B = 1e4,
                        seed = NULL,
                        tl = -Inf,
                        tu = Inf,
                        B_effect = 250,
                        sample_range = c(2, 1000),
                        tol = .01,
                        display_message = FALSE,
                        ...){



  #----    Save call    ----

  # Match arguments
  effect_type <- match.arg(effect_type)
  alternative <- match.arg(alternative)

  # Save call
  design_analysis = "prospective"
  call_arguments = as.list(match_call(default = TRUE))[-1]

  # eval possible errors
  do.call(eval_arguments_prospective,
          call_arguments)

  # Define conf.level according to sig_level
  call_arguments$conf.level <- define_conf_level(call_arguments)

  #----    Set seed    ----

  # Set seed
  if(!is.null(seed)){
    old_seed <- .Random.seed
    on.exit( { .Random.seed <<- old_seed })
    set.seed(seed = seed)
  }

  #----    Evaluate effect size    ----

  effect_info <- eval_effect_size(effect_type = effect_type,
                                  effect_size = effect_size,
                                  tl = tl,
                                  tu = tu,
                                  B_effect = B_effect)
  effect_target = effect_info$effect_summary[["Mean"]]

  #----    Evaluate samples    ----

  if(effect_type == "correlation" && ratio_n2 != 1){
    call_arguments["ratio_n2"] <- list(1)
    warning("If effect_type is set to 'correlation', ratio_n2 is set to 1")
  }

  sample_info <- eval_samples(ratio_n2 = ratio_n2,
                              current_n = sample_range[2])

  # #----    Get test info    ----
  #
  # # Evaluate test test_method
  # test_method <- do.call(eval_test_method, c(call_arguments,
  #                                            effect_target = effect_target))
  # #Compute df and critical value
  # crit_values <- do.call(compute_critical_effect,
  #                        c(call_arguments,
  #                          test_method = test_method))
  #
  # test_info <- c(test_method = test_method,
  #                crit_values)
  #
  # #----    Retrospective analysis    ----
  #
  # if(effect_type == "cohen_d"){
  #   # Cohen's d
  #   retrospective_res <- sapply(effect_info$effect_samples,
  #                               FUN = function(effect_target) do.call(retrospective_cohen,
  #                                                                     c(call_arguments,
  #                                                                       effect_target = effect_target,
  #                                                                       test_method = test_method)))
  #
  # } else if (effect_type == "correlation"){
  #   # Correlation
  #
  #   # Check sample_n2
  #   if(!is.null(call_arguments$sample_n2)){
  #     call_arguments["sample_n2"] <- list(NULL)
  #     warning("If effect_type is set to 'correlation', sample_n2 is ignored.")
  #   }
  #
  #   retrospective_res <- sapply(effect_info$effect_samples,
  #                               FUN = function(effect_target) do.call(retrospective_correlation,
  #                                                                     c(call_arguments,
  #                                                                       effect_target = effect_target,
  #                                                                       test_method = test_method)))
  # }
  #
  # retrospective_res <- list2data(retrospective_res)
  #
  # #----    save results    ----
  # design_fit <- list(design_analysis = design_analysis,
  #                    call_arguments = call_arguments,
  #                    effect_info = effect_info,
  #                    test_info = test_info,
  #                    retrospective_res = retrospective_res)
  #
  return(list(call_arguments,sample_info))

}


#-----

