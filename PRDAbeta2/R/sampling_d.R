######## Sampling Cohen's d given a bounded distribution [Internal Function]
##############################################################
#' @title Sampling Cohen's d
#' @description The function samples \code{B0} values of effect  size (Cohen's \eqn{d}) from a given bounded
#' distribution. Boundaries are set using the argument \code{target_limits}. Probability distributions are
#' specified using  the argument \code{distribution}; \code{uniform}, default options, or (doubly truncated)
#' \code{normal} are  available.
#' @details The \code{uniform} distribution is defined considering minimum and maximum according to
#' \code{target_limits} values. The doubly truncated \code{normal} distribution is defined with mean
#' \eqn{(d_{l}+d_{u})/2} and standard deviation \eqn{(d_u - d_l)k},
#' where \eqn{d_l} and \eqn{d_u} are respectively the lower and upper values
#' defined in \code{target_limits}, and \eqn{k} a choosed constant (by default, \eqn{k=1/6}).
#' @note Required \pkg{truncnorm} package.
#' @return Returns a list with three objrcts: \item{y }{simulated values of effect size (Cohen's \eqn{d})}
#' \item{my }{mean of the distribution} \item{sy }{standard deviation of the distribution}
#' @param target_limits = vector with lower and upper bounds of the statistic distribution
#' @param distribution = a character string specifying the used sampling distribution, must be one of “\code{uniform}” or “\code{normal}”.
#' @param B0 = number of replicates
#' @param k = constant defining the standard deviation of truncated normal
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @examples
#' sampling_d()
#' sampling_d( c( .2, .3 ) )
#' d <- sampling_d( c( .2, .3 ) )
#' hist( d$y )
#' d <- sampling_d( c(.2,.3), "normal" )
#' hist( d$y )
#' @export
sampling_d <- function( target_limits = c(0,1), distribution = c("uniform","normal"), k = 1/6, B0 = 1e4 ) {

  distribution <- match.arg(distribution)
  my <- (sum(target_limits)/2)
  sy <- diff(target_limits)*k
  if (distribution=="uniform") {
    y <- runif(B0,target_limits[1], target_limits[2])
  } else {
    y <- rtruncnorm(B0,target_limits[1], target_limits[2], mean = my, sd = sy)

  }
  return(list(y=y,my=my,sy=sy))
}
