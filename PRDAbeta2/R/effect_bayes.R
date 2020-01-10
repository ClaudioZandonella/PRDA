#' @import truncnorm
#' @importFrom stats na.omit pt runif sd rnorm median
############################################
#' @title Compute the Bayes Factor for Mean Differences
#'
#' @description The function computes the Bayes factor to compare two models for the difference between means:
#' the null model where the difference is zero, and the alternative model where the difference
#' follows a given prior distribution.
#'
#' @param x1 numerical vector with the observed values of the first group.
#' @param x2 numerical vector with the observed values of the second group.
#' @param prior_limits numerical vector with lower and upper bounds of the prior distribution of the mean.
#' @param prior_density function, density of the prior distribution of the effect.
#'
#' @details The Bayes factor is computed as the ratio of the posterior probabilities of the alternative
#' model and the null model.
#' @details The domain of the prior distribution should contain zero.
#'
#' @return The function returns a list with the following objects: \code{BF} (Bayes factor) and
#' \code{difference} (difference between group means).
#'
#' @author Massimiliano Pastore
#'
#' @examples
#' x1 <- c(0.43, -0.56, -0.39, -0.21, -0.58,  0.65)
#' x2 <- c(2.38, -0.96, -0.82,  0.28, -0.40,  0.34,  1.82)
#' effect_bayes(x1, x2, prior_limits=c(0,1), prior_density=function(y) dcauchy(y, location=0, scale=1/sqrt(2)))
#' @export



effect_bayes <- function(x1, x2, prior_limits=c(0,1), prior_density=function(y) dcauchy(y, location=0, scale=1/sqrt(2))){
  m1 <- mean(x1, na.rm=TRUE)
  m2 <- mean(x2, na.rm=TRUE)
  z <- m2-m1
  n1 <- length(na.omit(x1))
  n2 <- length(na.omit(x2))
  N <- (n1*n2)/(n1+n2)

  integrand <- function(v) exp(N*v*(z -(v/2))) * prior_density(v)
  eff <- integrate(integrand,lower=prior_limits[1], upper = prior_limits[2])$value
  out <- list("BF"=eff, "difference"=z)
  return(out)
}
