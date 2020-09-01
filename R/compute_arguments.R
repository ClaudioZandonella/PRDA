#################################
####    Compute arguments    ####
#################################


#----    compute_eigen_matrix    ----

compute_eigen_matrix <- function(effect_target){

  Sigma = matrix(c(1,effect_target,effect_target,1),ncol=2)
  eS = eigen(Sigma, symmetric = TRUE)
  ev = eS$values
  Eign_matrix = eS$vectors %*% diag(sqrt(pmax(ev, 0)), 2)

  return(Eign_matrix)
}


#----    compute_errors    ----

compute_errors <- function(p.values, estimates, true_value, sig_level,B){

  sig_p.value = p.values < sig_level
  sum_sig_p = sum(sig_p.value)

  power = sum_sig_p / B
  typeS = 0

  if(true_value >= 0) {
    typeS = sum(sig_p.value & (estimates < 0)) / sum_sig_p
  } else {
    typeS = sum(sig_p.value & (estimates > 0)) / sum_sig_p}

  typeM = mean(abs(estimates[sig_p.value])) / abs(true_value)

  res = list(power = power, typeM = typeM, typeS = typeS)

  return(res)
}


#----    compute_df    ----

compute_df <- function(effect_type, sample_n1, sample_n2 = NULL, test_method){
  df = NULL
  if(effect_type == "cohen_d"){
    if (test_method == "two_samples") {
      df = sample_n1 + sample_n2 - 2L
    } else if (test_method == "welch"){
      df1 = sample_n1-1L
      df2 = sample_n2-1L
      df = ((sample_n1+sample_n2)^2 * df1 * df2)/(sample_n1^2*df1 + sample_n2^2*df2)
    } else if (test_method %in% c("one_sample","paired")){
      df = sample_n1-1L
    }
  } else if(effect_type == "correlation"){
    df = sample_n1 - 2L
  }

  return(df)
}


#----    compute_critical_t    ----

compute_critical_t <- function(df, sig_level, alternative = "two_sided"){

  critical_t =  NULL

  if(alternative == "two_sided"){
    critical_t = qt(1-sig_level/2, df)
  } else if(alternative == "greater"){
    critical_t = qt(1-sig_level, df)
  } else if(alternative == "less"){
    critical_t = qt(sig_level, df)
  }

  return(critical_t)
}


#----    compute_critical_effect    ----

compute_critical_effect <- function(effect_type, sample_n1, sample_n2 = NULL, test_method,
                                    sig_level, alternative, mu = 0, ...){


  df = compute_df(effect_type = effect_type,
                  sample_n1 = sample_n1,
                  sample_n2 = sample_n2,
                  test_method = test_method)

  critical_t = compute_critical_t(df, sig_level, alternative)

  critical_effect = NULL

  if(effect_type == "cohen_d"){
    if (test_method == "one_sample") {
      critical_effect = critical_t /sqrt(sample_n1)
    } else if (test_method == "paired"){
      critical_effect = critical_t /sqrt(sample_n1) + mu
    } else if (test_method %in% c("two_samples", "welch")) {
      critical_effect = critical_t * sqrt((sample_n1+sample_n2)/(sample_n1*sample_n2)) + mu
    }
  } else if(effect_type == "correlation"){
    critical_effect = critical_t /sqrt(sample_n1-2+critical_t^2)
  }

  res = list(df = df, critical_effect = critical_effect)

  return(res)
}

#----
