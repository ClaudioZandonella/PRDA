#' @title Design Analysis: estimated power
#' @description Internal function. In a comparison between two independent groups,
#' the function estimates the power level, type M and type S errors given the specified
#' sample size per group (\code{n}) and a plausible value of effect size (Cohen's \eqn{d}).
#' @note Called from function \code{\link{design_analysis}}.
#' @author Massimiliano Pastore, Gianmarco Altoe
#' @param n = number of observations (per group)
#' @param d = plausible effect size (Cohen's \eqn{d})
#' @param sig.level = significance level
#' @param B = number of replicates
#' @param tol = tolerance
design_analysis_power <- function( n, d, sig.level = 0.05, B = 1e4 ,tol=.005 ) {
  out <- sim_estimate(sim_d=d,target_d=d,n1=n,sig.level=sig.level,B=B)
  out=list(d=d,n=n,power=out[1,"power"],typeS=out[1,"typeS"],typeM=out[1,"typeM"])
  return(out)
}
