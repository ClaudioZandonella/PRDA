##### It performs prospective design analysis [Internal Function]
######################################################
#' @title Design Analysis: required sample size
#' @description Internal function. In a comparison between two independent groups,
#' given a plausible value of effect size (Cohen's \eqn{d}),
#' the function estimates the sample size (per group) needed to reach a required \code{power} level.
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @note Internal Function, called from function \code{\link{design_analysis}}.
#' @param d = plausible effect size (Cohen's \eqn{d})
#' @param sig.level = significance level
#' @param power = required power of the test
#' @param rangen = range of \eqn{n} (per group)
#' @param B = number of replicates
#' @param tol = tolerance
#' @export
design_analysis_n <- function( d, power, sig.level = 0.05, rangen = c(2,1000), B = 1e4, tol = .005 ){

  (n_seq <- seq( rangen[1], rangen[2], by = 1 ))
  (n_target <- round(median(n_seq)))
  find_power <- FALSE

  ## check with maximum N
  cat("Estimating power with n =",rangen[2],"\n")
  (est_P <- sim_estimate( n1 = rangen[2], n2 = rangen[2], sim_d = d, target_d=d, sig.level = sig.level, B = B ))
  (est_power <- est_P[1,]$power)

  if ( est_power < power ) {
    cat(paste0("Actual power = ", est_power, " with n = ", rangen[2], " (per group); " ),"\n")
    cat(paste0("   try to increase maximum of rangen > ", rangen[2],"."),"\n")
    out <- NULL
  } else {

    ## estimating power
    while( (!find_power) ) {
      cat("Estimating power with n =",n_target,"\n")
      (est_P <- sim_estimate( n1 = n_target, n2 = n_target, sim_d = d, target_d=d, sig.level = sig.level, B = B))
      (est_power <- est_P[1,]$power)

      if ( (est_power<=(power+tol)) & (est_power>(power-tol)) ) {
        find_power <- TRUE
      } else {

        if (length(n_seq)==1) {
          print(n_seq)
          stop(" ")
        }

        if ( est_power > (power-tol) ) {
          (n_seq <- seq( min(n_seq), n_target, by = 1))
          (n_target <- round(median(n_seq)))
        } else {
          (n_seq <- seq( n_target, max(n_seq), by = 1))
          (n_target <- round(median(n_seq)))
        }
      }
    }
    out <- list( d = d, power = power, n = n_target, typeS=est_P[1,"typeS"],typeM=est_P[1,"typeM"])
  }
  if (!is.null(out))  return(out)
}
