######## Sampling an effect given a plausible bounded distribution [Internal Function]
##############################################################
#' @title Sampling Effect
#'
#' @description Internal function. It samples \code{B0} values of effect  size from a given bounded
#' distribution (uniform or doubly truncated normal).
#'
#' @param limits vector with lower and upper bounds of the distribution.
#' @param distribution a character string specifying the sampling distribution, must be one of \code{"uniform"} or \code{"truncnorm"}.
#' @param h constant defining the standard deviation of the truncated normal distribution.
#' @param B0 number of replicates.
#'
#' @details The uniform distribution is defined considering minimum and maximum according to
#' \code{limits} values. The doubly truncated normal distribution is defined with mean
#' \eqn{(a+b)/2} and standard deviation \eqn{(b-a)h},
#' where \eqn{a} and \eqn{b} are respectively the lower and upper values
#' defined in \code{limits}, and \eqn{h} is a chosen constant.
#'
#' @return The function returns a list with the following objrcts:
#' \code{mean} (mean of the distribution), \code{sd} (standard deviation of the distribution)
#' and \code{effect} (vector containing the sampled effect values).
#'
#' @note The \pkg{truncnorm} package is required.
#'
#' @author Massimiliano Pastore, Gianmarco Altoè
#'
#' @examples
#' e <- sampling_e(limits=c(0.2,0.3), distribution="truncnorm", h=1/10, B0=100)
#' hist(e$effect)
#' @export



sampling_e <- function(limits=c(0,1), distribution=c("uniform","truncnorm"), h=1/6, B0=1e4){
  distribution <- match.arg(distribution)
  a <- limits[1]
  b <- limits[2]

  if(distribution == "uniform"){
    m <- (a+b)/2
    s <- (b-a)/(2*sqrt(3))
    e <- runif(B0, a, b)
  }
  else if(distribution == "truncnorm"){
    m <- (a+b)/2
    s <- (b-a)*h
    e <- rtruncnorm(B0, a, b, mean=m, sd=s)
  }

  out <- list("mean"=m, "sd"=s, "effect"=e)
  return(out)
}
