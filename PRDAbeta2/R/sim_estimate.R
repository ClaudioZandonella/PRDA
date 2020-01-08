##### It calculates power, typeM, typeS based on B replicates [Internal Function]
# Note for design_analysis:  target_d is equal to sim_d
##############################################################
#' @title Internal function
#' @description Internal function. In a comparison between two independent groups,
#' the function computes through \code{B} simulations the expected power,
#' Type-S and Type-M errors, given a plausible effect size (\code{target_d}) and the sample size of each group.
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @note Called from \code{\link{design_est}}. For \code{design_analysis}: \code{target_d} is equal to \code{sim_d}.
#' @param B = number of replicates
#' @param n1 = sample size group 1
#' @param n2 = sample size group 2
#' @param sim_d = simulated effect size (Cohen's \eqn{d})
#' @param target_d = target effect size (Cohen's \eqn{d})
#' @param sig.level = significance level
#' @return Returns a \code{data.frame} with the results of the simulation and the computed values of
#' \code{power}, \code{typeS}, and \code{typeM} errors.
sim_estimate <- function( sim_d, target_d = sim_d, n1, n2 = n1, sig.level = .05, B = 1e4 ) {
  outsim <- t(replicate( B, {
    x1 <- rnorm(n1)
    x2 <- rnorm(n2,sim_d)
    d <- cohen_d(x1,x2)
    sim <- c(d$d,d$tstat,d$pval,sim_d)
  }))
  colnames(outsim) <- c("est_d","tstat","pval","sim_d")
  (outsim <- data.frame(outsim))
  (outsim$power <- sum( outsim$pval < sig.level ) / nrow(outsim))
  (outsim$typeS <- with(outsim, sum( (pval < sig.level) & (tstat<0)) / sum(pval < sig.level)  ) )
  (outsim$typeM <- with(outsim, mean(abs(est_d[pval< sig.level]))) / target_d)
  return(outsim)
}
#
