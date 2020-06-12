##################################
####    evaluate arguments    ####
##################################


#----    eval_arguments_retrospective    ----

eval_arguments_retrospective <- function(sample_n1, effect_size, sample_n2, effect_type,
                                         alternative, sig_level, B, seed, tl, tu, B_effect,...){
  # Check inputs arguments
  if(!is_single_numeric(sample_n1) || sample_n1 <= 1 )
    stop("sample_n1 has to be a single integer value grater than 1.")

  if(!is.function(effect_size) && !is_single_numeric(effect_size))
    stop("effect_size has to be a single numeric value or a function.")

  if(!is.null(sample_n2) && (!is_single_numeric(sample_n2) || sample_n2 <= 1))
    stop("If specified, sample_n2 has to be a single integer value grater than 1.")

  if(!is_single_numeric(sig_level) || sig_level >= 1 || sig_level <= 0)
    stop("sig_level has to be a single value between 0 and 1.")

  if(!is_single_numeric(B) || B <= 1)
    stop("B has to be a single integer value grater than 1.")

  if(!is.null(seed) && (!is_single_numeric(seed)))
    stop("If specified, seed has to be a single finite number.")

  if(!is_single_numeric(tl, infinite = TRUE ))
    stop("tl has to be a single numeric value.")

  if(!is_single_numeric(tu, infinite = TRUE))
    stop("tu has to be a single numeric value.")

  if(!is_single_numeric(B_effect) || B_effect <= 1)
    stop("B_effect has to be a single integer value grater than 1.")
}

#----    eval_arguments_prospective    ----

eval_arguments_prospective <- function(effect_size, power, ratio_n2, effect_type, alternative,
                                       sig_level, B, seed, tl, tu, B_effect, sample_range,
                                       tol, display_message, ...){
  # Check inputs arguments
  if(!is.function(effect_size) && !is_single_numeric(effect_size))
    stop("effect_size has to be a single numeric value or a function.")

  if(!is_single_numeric(power) || power >= 1 || power <= 0)
    stop("power has to be a single value between 0 and 1.")

  if(!is_single_numeric(ratio_n2) || ratio_n2 < 1)
    stop("ratio_n2 has to be a single integer value grater or equal than 1.")

  if(!is_single_numeric(sig_level) || sig_level >= 1 || sig_level <= 0)
    stop("sig_level has to be a single value between 0 and 1.")

  if(!is_single_numeric(B) || B <= 1)
    stop("B has to be a single integer value grater than 1.")

  if(!is.null(seed) && (!is_single_numeric(seed)))
    stop("If specified, seed has to be a single finite number.")

  if(!is_single_numeric(tl, infinite = TRUE ))
    stop("tl has to be a single numeric value.")

  if(!is_single_numeric(tu, infinite = TRUE))
    stop("tu has to be a single numeric value.")

  if(!is_single_numeric(B_effect) || B_effect <= 1)
    stop("B_effect has to be a single integer value grater than 1.")

  if(length(sample_range)!=2L || sum(!is.finite(sample_range)))
    stop("sample_range has to be a two length numeric vector.")
  if(sample_range[1] <= 1 || sample_range[1] > sample_range[2])
    stop("sample_range minimum has to be grater than 1 and less than sample range maximum.")

  if(!is_single_numeric(tol) || tol <= 0 || tol >= 1)
    stop("tol has to be a single value between 0 and 1.")

  if(!is.logical(display_message))
    stop("display_message has to be logical.")
}

#----    eval_effect_size    ----

eval_effect_size <- function(effect_type, effect_size,
                             tl = -Inf, tu = Inf, B_effect = 250){
  correlation <- effect_type == "correlation"
  sample_fun <- is.function(effect_size)

  if(!sample_fun){
    res <- list(effect_function = "single_value",
                effect_summary = summary(effect_size),
                effect_samples = effect_size)

    if(correlation && (effect_size < -1 || effect_size > 1))
      stop("If 'correlation' effect_size must be between -1 and 1")
  } else {

    if(correlation && (tl < -1 || tu > 1)){
      tl <- max(-1, tl)
      tu <- min(tu, 1)
      message(paste("If 'correlation' effect_size distribution is truncated between",
                    tl,"and", tu))
    }

    res <- sample_effect(FUN = effect_size, B_effect = B_effect, tl = tl, tu = tu)
  }

  return(res)
}

#----    eval_samples    ----

eval_samples <- function(ratio_n2, current_n){
  sample_n1 <- current_n
  sample_n2 <- round(sample_n1 * ratio_n2,0)

  return(list(sample_n1 = sample_n1, sample_n2 = sample_n2))
}

#----    eval_test_method    ----

eval_test_method <- function(effect_type, effect_target, sample_n1, sample_n2 = NULL,
                             paired=FALSE, var.equal = FALSE,
                             method = c("pearson", "kendall", "spearman"), ...){
  test_method <- " "

  # Cohen d
  if(effect_type == "cohen_d"){
    if(paired && ((is.null(sample_n2) || sample_n1!=sample_n2))){
      stop("If paired = TRUE sample_n1 and sample_n2 must be equal.")
    }

    groups <- sample_groups(sample_n1, effect_target, sample_n2)
    t.test(groups$x, groups$y, paired=paired,...)

    if(is.null(sample_n2)){
      test_method <- "one_sample"
    } else if(paired){
      test_method <- "paired"
    } else if(var.equal) {
      test_method <- "two_samples"
    } else {
      test_method <- "welch"
    }

  } else if (effect_type == "correlation"){
    method <- match.arg(method)

    groups <- sample_obs_cor(sample_n1, effect_target)
    cor.test(groups$x, groups$y, ...)

    if(method == "pearson"){
      test_method <- "pearson"
    } else if(method == "spearman"){
      stop("correlation with method = 'spearman' is not implemented. Only method = 'pearson' is available.")
    } else if(method == "kendall") {
      stop("correlation with method = 'kendall' is not implemented. Only method = 'pearson' is available.")
    }
  }

  return(test_method)
}
