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
                          B = 1000,
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
  z <- list(call_arguments = as.list(match_call(default = TRUE))[-1])

  #----    Set seed    ----

  # Set seed
  if(!is.null(seed)){
    old_seed <- .Random.seed
    on.exit( { .Random.seed <<- old_seed })
    set.seed(seed = seed)
  }

  #----    Retrospective analysis    ----



  #
  # analysis_simulated <- simulate_analysis(sample_n1 = sample_n1,
  #                                         effect_size = effect_size,
  #                                         sample_n2 = sample_n2,
  #                                         effect_type = effect_type,
  #                                         alternative = alternative,
  #                                         B = B, ...)

  if(effect_type == "cohen_d"){
    analysis_simulated <- do.call("analysis_cohen", z$call_arguments)
  } else if (effect_type == "correlation"){
    analysis_simulated <- do.call("analysis_correlation", z$call_arguments)
  }


  #----
  return(analysis_simulated)

}


#-----

