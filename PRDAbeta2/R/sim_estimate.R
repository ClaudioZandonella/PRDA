##### It calculates power, typeM, typeS, typeN and typeI based on B replicates [Internal Function]
# Note for design_analysis:  target_d is equal to sim_d
##############################################################
#' @title Simulation of Power and Errors
#'
#' @description Internal function. In a comparison between two independent groups,
#' it computes through \code{B} simulations the expected power,
#' Type-S, Type-M, Type-N and Type-I errors, given a plausible effect size (\code{target_e})
#' and the sample size of each group.
#'
#' @param sim_e simulated effect size
#' @param target_e target effect size
#' @param type a character string specifying the effect type, must be one of \code{"cohen"},
#' \code{"hedges"}, \code{"glass"} or \code{"bayes"}.
#' @param n1 sample size of the first group.
#' @param n2 sample size of the second group.
#' @param sig.level significance level (used if \code{type} is \code{"cohen"},
#' \code{"hedges"} or \code{"glass"}).
#' @param prior_limits vector with lower and upper bounds of the prior distribution of the mean
#' (used if \code{type} is \code{"bayes"}).
#' @param prior_density function, density of the prior distribution of the mean
#' (used if \code{type} is \code{"bayes"}).
#' @param k threshold for the Bayes factor (used if \code{type} is \code{"bayes"}).
#' @param B number of replicates.
#' @param return_data logical, if TRUE the function returns the simulated data.
#'
#' @details The Bayes factor \eqn{BF} is computed as the ratio of the posterior probabilities of the alternative
#' model and the null model. The threshold \eqn{k} is such that
#' the evidence supports the null model if \eqn{BF < 1/k}, and
#' it supports the alternative model if \eqn{BF > k}.
#' @details Type-S error is the probability that a significant effect is estimated in the wrong direction.
#' @details Type-M error is the average overestimation of an effect that emerges as significant.
#' @details Type-N error is the probability of false negatives (1 - power in a non-Bayesian framework).
#' @details Type-I error is the probability of an indecisive outcome when the effect is significative (zero
#' in a non-Bayesian framework).
#'
#' @return If \code{return_data} is \code{FALSE} (default), the function returns a list with the following objects:
#' \code{power} (power computed on the simulated data), \code{typeS}, \code{typeM}, \code{typeN} and \code{typeI}
#' (errors computed on the simulated data).
#' @return If \code{return_data} is \code{TRUE}, the function returns a list with the above-mentioned objects
#' and \code{return_data}, a data frame containing \code{est_e} (effect computed
#' on the simulated data), \code{tstat} (\eqn{t} value), \code{pval} (\eqn{p}-value) and
#' \code{sim_e} (the effect used to simulate data).
#'
#' @note For \code{design_analysis}: \code{target_e} is equal to \code{sim_e}.
#'
#' @author Massimiliano Pastore, Gianmarco Altoè
#'
#' @examples
#' sim_estimate(sim_e=0.9, type="cohen", n1=10, n2=15, sig.level=0.05, B=10, return.data=FALSE)
#' @export

sim_estimate <- function(sim_e, target_e=sim_e, type=c("cohen","hedges","glass","bayes"), n1, n2=n1,
                         sig.level=0.05, prior_limits=c(0,1), prior_density=function(y) dcauchy(y, location=0, scale=1/sqrt(2)),
                         k=3, B=1e4, return_data=FALSE){
  type <- match.arg(type)

  if(type %in% c("cohen","hedges","glass")){
    outsim <- t(replicate(B, {
      x1 <- rnorm(n1, mean=0, sd=1)
      x2 <- rnorm(n2, mean=sim_e, sd=1)
      e <- effect_mdiff(x1, x2, type, sign.level)
      sim <- c(e$effect, e$tstat, e$pval, sim_e)
    }))
    colnames(outsim) <- c("est_e","tstat","pval","sim_e")
    outsim <- data.frame(outsim)
    power <- with(outsim, sum(pval < sig.level)/B)
    typeS <- with(outsim, sum((pval < sig.level) & (tstat < 0))/sum(pval < sig.level))
    typeM <- with(outsim, mean(abs(est_e[pval < sig.level])))/target_e
    typeN <- 1 - power
    typeI <- 0
  }

  else if(type == "bayes"){
    outsim <- t(replicate(B, {
      x1 <- rnorm(n1, mean=0, sd=1)
      x2 <- rnorm(n2, mean=sim_e, sd=1)
      e <- effect_bayes(x1, x2, prior_limits, prior_density)
      sim <- c(e$BF, e$difference, sim_e)
    }))
    colnames(outsim) <- c("BF","est_e","sim_e")
    outsim <- data.frame(outsim)
    power <- with(outsim, sum(BF > k)/B)
    typeS <- with(outsim, sum((BF > k) & (sign(est_e) != sign(target_e)))/sum(BF > k))
    typeM <- with(outsim, mean(abs(est_e[BF > k])))/target_e
    typeN <- with(outsim, sum(BF < 1/k)/B)
    typeI <- with(outsim, sum((BF >= 1/k) & (BF <= k))/B)
  }

  out <- list("power"=power, "typeS"=typeS, "typeM"=typeM, "typeN"=typeN, "typeI"=typeI)

  if(return_data){
    out$sim_data <- outsim
  }

  return(out)
}
