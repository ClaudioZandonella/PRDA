#############################
####    Retrospective    ####
#############################

#----    Retrospective    ----

#' Retrospective
#'
#' @param sample_n1 numeric value.
#' @param effect_size numeric value.
#' @param sample_n2 numeric value.
#' @param effect_type character value.
#' @param alternative character value.
#' @param sig_level numeric value.
#' @param B numeric value.
#' @param seed numeric value.
#' @param ... optional argument.
#'
#' @return a list
#' @export
#'
retrospective <- function(sample_n1,
                          effect_size,
                          sample_n2 = NULL,
                          effect_type = c("cohen_d","correlation"),
                          alternative = c("two.sided","less","greater"),
                          sig_level = .05,
                          B = 1e4,
                          seed = NULL,
                          ...){



  #----    Save call    ----

  # Check inputs arguments
  if(!is_single_numeric(sample_n1) || sample_n1 <= 1 )
    stop("sample_n1 has to be a single integer value grater than 1.")

  if(!is_single_numeric(effect_size))
    stop("effect_size has to be a single numeric value.")

  if(!is.null(sample_n2) && (!is_single_numeric(sample_n2) || sample_n2 <= 1))
    stop("If specified, sample_n2 has to be a single integer value grater than 1.")

  if(!is_single_numeric(sig_level) || sig_level >= 1 || sig_level <= 0)
    stop("sig_level has to be a single value between 0 and 1.")

  if(!is_single_numeric(B) || B <= 1)
    stop("B has to be a single integer value grater than 1.")

  if(!is.null(seed) && (!is_single_numeric(seed)))
    stop("If specified, seed has to be a single finite number.")

  # Match arguments
  effect_type <- match.arg(effect_type)
  alternative <- match.arg(alternative)

  # Save call
  design_analysis = "retrospective"
  call_arguments = as.list(match_call(default = TRUE))[-1]

  # Define conf.level according to sig_level
  call_arguments$conf.level <- define_conf_level(call_arguments)

  #----    Set seed    ----

  # Set seed
  if(!is.null(seed)){
    old_seed <- .Random.seed
    on.exit( { .Random.seed <<- old_seed })
    set.seed(seed = seed)
  }

  #----    Get test info    ----

  # Evaluate test test_method
  test_method <- do.call(eval_test_method, call_arguments)
  #Compute df and critical value
  crit_values <- compute_critical_effect(effect_type, sample_n1, sample_n2, test_method,
                                         sig_level, alternative, ...)

  test_info <- c(test_method = test_method,
                    crit_values)
  #----    Retrospective analysis    ----

  if(effect_type == "cohen_d"){
    # Cohen's d
    retrospective_res <- do.call(retrospective_cohen,
                                 c(call_arguments,
                                   test_method = test_method))

  } else if (effect_type == "correlation"){
    # Correlation

    # Check sample_n2
    if(!is.null(call_arguments$sample_n2)){
      call_arguments["sample_n2"] <- list(NULL)
      warning("If effect_type is set to 'correlation', sample_n2 is ignored.")
    }

    retrospective_res <- do.call(retrospective_correlation,
                                 c(call_arguments,
                                   test_method = test_method))
  }


  #----    save results    ----
  design_fit <- list(design_analysis = design_analysis,
                     call_arguments = call_arguments,
                     test_info = test_info,
                     retrospective_res = retrospective_res)

  return(design_fit)

}


#-----

