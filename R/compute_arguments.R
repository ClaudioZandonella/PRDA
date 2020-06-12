#################################
####    Compute arguments    ####
#################################


#----    eval_arguments    ----

eval_arguments <- function(sample_n1, effect_size, sample_n2, effect_type,
                           alternative, sig_level, B, seed, tl, tu, B_effect, ...){
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

  if(!is_single_numeric(tl) && !is.infinite(tl))
    stop("tl has to be a single numeric value.")

  if(!is_single_numeric(tu) && !is.infinite(tu))
    stop("tu has to be a single numeric value.")

  if(!is_single_numeric(B_effect) || B_effect <= 1)
    stop("B_effect has to be a single integer value grater than 1.")
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
#----    eval_test_method    ----

eval_test_method <- function(effect_type, effect_size, sample_n1, sample_n2 = NULL,
                             paired=FALSE, var.equal = FALSE,
                             method = c("pearson", "kendall", "spearman"), ...){
  test_method <- " "

  # Cohen d
  if(effect_type == "cohen_d"){
    if(paired && ((is.null(sample_n2) || sample_n1!=sample_n2))){
      stop("If paired = TRUE sample_n1 and sample_n2 must be equal.")
    }

    groups <- sample_groups(sample_n1, effect_size, sample_n2)
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

    groups <- sample_obs_cor(sample_n1, effect_size)
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


#----    pool_sd    ----

pool_sd <- function(x, y){
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)

  sqrt((sum((x-mx)^2)+sum((y-my)^2))/(nx + ny -2))
}

#----    compute_eigen_matrix    ----

compute_eigen_matrix <- function(effect_size){

  Sigma = matrix(c(1,effect_size,effect_size,1),ncol=2)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  Eign_matrix <- eS$vectors %*% diag(sqrt(pmax(ev, 0)), 2)

  return(Eign_matrix)
}

#----    compute_errors    ----

compute_errors <- function(p.values, estimates, true_value, sig_level, alternative,B){

  sig_p.value <- p.values < sig_level
  sum_sig_p <- sum(sig_p.value)

  power = sum_sig_p / B
  typeS = 0
  if(alternative == "two.sided") {
    if(true_value >= 0) {
      typeS <- sum(sig_p.value & (estimates < 0)) / sum_sig_p
    } else {
      typeS <- sum(sig_p.value & (estimates > 0)) / sum_sig_p}
  }
  typeM <- mean(abs(estimates[sig_p.value])) / abs(true_value)

  res <- list(power = power, typeM = typeM, typeS = typeS)

  return(res)
}

#----    compute_df    ----

compute_df <- function(effect_type, sample_n1, sample_n2 = NULL, test_method){
  df <- NULL
  if(effect_type == "cohen_d"){
    if (test_method == "two_samples") {
      df <- sample_n1 + sample_n2 - 2L
    } else if (test_method == "welch"){
      df1 <- sample_n1-1L
      df2 <- sample_n2-1L
      df <- ((sample_n1+sample_n2)^2 * df1 * df2)/(sample_n1^2*df1 + sample_n2^2*df2)
    } else if (test_method %in% c("one_sample","paired")){
      df <- sample_n1-1L
    }
  } else if(effect_type == "correlation"){
    df <- sample_n1 - 2L
  }

  return(df)
}

#----    compute_critical_t    ----

#' Title
#'
#' @param df numeric value
#' @param sig_level numeric value
#' @param alternative character value
#'
#' @return a numeric value
#' @importFrom stats qt
#'
compute_critical_t <- function(df, sig_level, alternative = "two.sided"){

  critical_t <-  NULL

  if(alternative == "two.sided"){
    critical_t <- qt(1-sig_level/2, df)
  } else if(alternative == "greater"){
    critical_t <- qt(1-sig_level, df)
  } else if(alternative == "less"){
    critical_t <- qt(sig_level, df)
  }

  return(critical_t)
}
#----    compute_critical_effect    ----

compute_critical_effect <- function(effect_type, sample_n1, sample_n2 = NULL, test_method,
                                    sig_level, alternative, mu = 0, ...){

  df <- compute_df(effect_type = effect_type, sample_n1 = sample_n1,
                   sample_n2 = sample_n2, test_method = test_method)
  critical_t <- compute_critical_t(df, sig_level, alternative)

  critical_effect <- NULL

  if(effect_type == "cohen_d"){
    if (test_method == "one_sample") {
      critical_effect <- critical_t /sqrt(sample_n1)
    } else if (test_method == "paired"){
      critical_effect <- critical_t /sqrt(sample_n1) + mu
    } else if (test_method %in% c("two_samples", "welch")) {
      critical_effect <- critical_t * sqrt((sample_n1+sample_n2)/(sample_n1*sample_n2)) + mu
    }
  } else if(effect_type == "correlation"){
    critical_effect <- critical_t /sqrt(sample_n1-2+critical_t^2)
  }

  res = list(df = df, critical_effect = critical_effect)

  return(res)
}

#----
