###########################
####    Prospective    ####
###########################

#----    Prospective    ----


#' Title
#'
#' @param effect_size numeric value or function
#' @param power numeric value
#' @param ratio_n2 numeric value
#' @param effect_type character value.
#' @param alternative character value.
#' @param sig_level numeric value
#' @param B numeric value
#' @param seed numeric value
#' @param tl numeric value
#' @param tu numeric value
#' @param B_effect numeric value
#' @param sample_range two value numeric vector
#' @param tol numeric value
#' @param display_message logical
#' @param ... optional argumets
#'
#' @return a list
#' @export
#'
#' @importFrom stats median
#'
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

  sample_info <- do.call(eval_samples,
                         c(call_arguments,
                           current_n = sample_range[2]))

  #----    Get test method    ----

  # Evaluate test test_method
  test_method <- do.call(eval_test_method, c(call_arguments,
                                             sample_n1 = sample_info$sample_n1,
                                             sample_n2 = sample_info$sample_n2,
                                             effect_target = effect_target))

  #----    Prospective ananlysis    ----

  # Check with maximum N
  prospective_res <- do.call(simulate_analysis,
                             c(call_arguments,
                               effect_info["effect_samples"],
                               test_method = test_method,
                               sample_info))

  est_power <- mean(prospective_res$power)

  if ( est_power < power ) {
    cat(paste0("Actual power = ", est_power, " with n = ", sample_range[2]),"\n")
    cat(paste0("   try to increase maximum of sample_range > ", sample_range[2],"."),"\n")
  } else {

    find_power <- FALSE
    n_seq <- seq( sample_range[1], sample_range[2], by = 1 )
    n_target <- round(median(n_seq))

    while( (!find_power) ) {
      sample_info <- do.call(eval_samples,
                             c(call_arguments,
                               current_n = n_target))

      prospective_res <- do.call(simulate_analysis,
                                 c(call_arguments,
                                   effect_info["effect_samples"],
                                   test_method = test_method,
                                   sample_info))

      est_power <- mean(prospective_res$power)

      if (display_message == TRUE){
        cat("Evaluate n =", n_target, fill=TRUE)
        cat("Estimated power is", round(est_power,2), fill=TRUE)
        cat("\n")
      }

      # Evaluate if power was obtained according to tolerance value
      if ( (est_power<=(power+tol)) && (est_power>=(power-tol)) ) {
        find_power <- TRUE
      } else {
        if (length(n_seq)==1) { stop("Increase tolerance value")
        } else if (est_power > (power+tol)) {
          (n_seq <- seq( min(n_seq), n_target-1, by = 1))
          (n_target <- round(median(n_seq)))
        } else {
          (n_seq <- seq(n_target+1, max(n_seq), by = 1))
          (n_target <- round(median(n_seq)))
        }
      }
    }
  }

  #----    Get test_info    ----
  #Compute df and critical value
  crit_values <- do.call(compute_critical_effect,
                         c(call_arguments,
                           sample_n1 = sample_info$sample_n1,
                           sample_n2 = sample_info$sample_n2,
                           test_method = test_method))

  test_info <- c(test_method = test_method,
                 crit_values)

  #----    save results    ----
  design_fit <- list(design_analysis = design_analysis,
                     call_arguments = call_arguments,
                     effect_info = effect_info,
                     sample_info = sample_info,
                     test_info = test_info,
                     prospective_res = prospective_res)


  return(design_fit)
}


#-----

