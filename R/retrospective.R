#############################
####    Retrospective    ####
#############################

#----    Retrospective    ----

#' Retrospective
#'
#' @param sample_group1 numeric value.
#' @param effect_size numeric value.
#' @param sample_group2 numeric value.
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
retrospective <- function(sample_group1,
                          effect_size,
                          sample_group2 = NULL,
                          effect_type = c("cohen_d","correlation"),
                          alternative = c("two.sided","less","greater"),
                          sig_level = .05,
                          B = 1000,
                          seed = NULL,
                          ...){

  #----    Check inputs arguments    ----

  if(!is.finite(sample_group1) || sample_group1 <= 1 || length(sample_group1) > 1)
    stop("sample_group1 has to be a single integer value grater than 1.")

  if(!is.finite(effect_size) || length(effect_size) != 1)
    stop("effect_size has to be a single integer value.")

  if(!is.null(sample_group2) && (!is.finite(sample_group2) || sample_group2 <= 1 || length(sample_group2) != 1))
    stop("If specified, sample_group2 has to be a single integer value grater than 1.")

  if(!is.finite(sig_level) || sig_level >= 1 || sig_level <= 0 || length(sig_level) != 1)
    stop("sig_level has to be a single value between 0 and 1.")

  if(!is.finite(B) || B <= 1 || length(B) != 1)
    stop("B has to be a single integer value grater than 1.")

  if(!is.null(seed) && (!is.finite(seed) || length(seed) != 1))
    stop("If specified, seed has to be a single finite number.")

  effect_type <- match.arg(effect_type)

  alternative <- match.arg(alternative)

  # Check sample_group2
  if(effect_type=="correlation" && !is.null(sample_group2)){
    warning("If effect_type is set to 'correlation', sample_group2 is ignored.")
    sample_group2 <- NULL
  }

  if(effect_type=="cohen_d" && is.null(sample_group2)){
    sample_group2 <- sample_group1
  }

  # Save call

  call_arguments <- as.list(match.call()[-1])

  #----    Simulate data    ----

  # Set seed
  if(!is.null(seed)){
    old_seed <- .Random.seed
    on.exit( { .Random.seed <<- old_seed })
    set.seed(seed = seed)
    }

  analysis_simulated <- simulate_analysis(sample_group1 = sample_group1,
                                          effect_size = effect_size,
                                          sample_group2 = sample_group2,
                                          effect_type = effect_type,
                                          alternative = alternative,
                                          B = B, ...)


  #----
  return(analysis_simulated)

}


#-----

