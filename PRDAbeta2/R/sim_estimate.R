##### It calculates power, typeM, typeS, typeN and typeI based on B replicates [Internal Function]
# Note for design_analysis:  target_d is equal to sim_d
##############################################################
#' @title Simulation of Power and Errors
#' @description Internal function. In a comparison between two independent groups,
#' the function computes through \code{B} simulations the expected power,
#' Type-S, Type-M, Type-N and Type-I errors, given a plausible effect size (\code{target_eff}) and the sample size of each group.
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @note Called from \code{\link{design_est}}. For \code{design_analysis}: \code{target_eff} is equal to \code{sim_eff}.
#' @param sim_eff = simulated effect size
#' @param target_eff = target effect size
#' @param type = a character string specifying the effect type, must be one of “\code{cohen}”,
#' "\code{hedges}", “\code{glass}” or “\code{bayes}”.
#' @param n1 = sample size of the first group.
#' @param n2 = sample size of the second group.
#' @param sig.level = significance level.
#' @param prior_limits = vector with lower and upper bounds of the prior distribution of the mean.
#' @param prior_density = function, density of the prior distribution of the mean.
#' @param B = number of replicates.
#' @return Returns the computed values of
#' \code{power}, \code{typeS}, \code{typeM}, \code{typeN} and \code{typeI}  errors.
#' @details Type-S error is the probabiliyy that a significant effect is estimated in the wrong direction.
#' @details Type-M error is the average overestimation of an effect that emerges as significant.
#' @details Type-N error is the probability of false negatives (1-power in a non-Bayesian framework).
#' @details Type-I error is the probability of an indecisive outcome when the effect is significative (zero
#' in a non-Bayesian framework).
#' @examples
#' sim_estimate(sim_eff=0.9, type="cohen", n1=10, n2=15, sig.level=0.05, B=10)
#' @export
#'
sim_estimate <- function(sim_eff, target_eff=sim_eff, type=c("cohen","hedges","glass","bayes"), n1, n2=n1,
                         sig.level=0.05, prior_limits=c(0,1), prior_density=function(y) dcauchy(y, location=0, scale=1/sqrt(2)),
                         B=1e4){
  type <- match.arg(type)

  if(type %in% c("cohen","hedges","glass")){
    outsim <- t(replicate(B, {
      x1 <- rnorm(n1, mean=0, sd=1)
      x2 <- rnorm(n2, mean=sim_eff, sd=1)
      e <- effect_mdiff(x1, x2, type, sign.level)
      sim <- c(e$effect, e$tstat, e$pval, sim_eff)
    }))
    colnames(outsim) <- c("est_eff","tstat","pval","sim_eff")
    outsim <- data.frame(outsim)
    power <- sum(outsim$pval < sig.level)/B
    typeS <- with(outsim, sum((pval < sig.level) & (tstat < 0))/sum(pval < sig.level))
    typeM <- with(outsim, mean(abs(est_eff[pval < sig.level])))/target_eff
    typeN <- 1 - power
    typeI <- 0
  }

  else if(type == "bayes"){
    outsim <- t(replicate(B, {
      x1 <- rnorm(n1, mean=0, sd=1)
      x2 <- rnorm(n2, mean=sim_eff, sd=1)
      e <- effect_bayes(x1, x2, prior_limits, prior_density)
      sim <- c(e$BF, e$difference, sim_eff)
    }))
    colnames(outsim) <- c("BF","est_eff","sim_eff")
    outsim <- data.frame(outsim)
    power <- with(outsim, sum(BF > thr)/B)
    typeS <- with(outsim, sum((BF > thr) & (sign(est_eff) != sign(target_eff)))/sum(BF > thr))
    typeM <- with(outsim, mean(abs(est_eff[BF > thr])))/target_eff
    typeN <- with(outsim, sum(BF < 1/thr)/B)
    typeI <- with(outsim, sum((BF >= 1/thr) & (BF <= thr))/B)
  }

  out <- list("power"=power, "typeS"=typeS, "typeM"=typeM, "typeN"=typeN, "typeI"=typeI)
  return(out)
}
