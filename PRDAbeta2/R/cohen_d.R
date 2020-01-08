#' @import truncnorm
#' @importFrom stats na.omit pt runif sd rnorm median
############################################
#' @title Compute Cohen's d
#' @description Computes Cohen's \eqn{d} effect size statistics with the relative \eqn{t}-statistics and \eqn{p}-value.
#' @param x1 = numerical vector with the observed values of the first group
#' @param x2 = numerical vector with the observed values of the second group
#' @return Returns a list with the following objects: \code{d} (estimated Cohen's \eqn{d}),
#' \code{tstat} (\eqn{t} value), \code{pval} (\eqn{p}-value).
#' @details Cohen's \eqn{d} is computed considering the pooled standard deviation.
#' @details \eqn{t}-statistics and relative \eqn{p}-value are computed considering two-tailed \eqn{t}-test for independent samples with equal variance.
#' @author Massimiliano Pastore
#' @examples
#' x1 <- c( 30.02, 29.99, 30.11, 29.97, 30.01, 20.99 )
#' x2 <- c( 29.89, 29.93, 29.72, 29.98, 30.02, 29.98, 50 )
#' cohen_d( x1, x2 )
#' @export
cohen_d <- function(x1,x2) {

  (m1 <- mean(x1, na.rm = TRUE))
  (m2 <- mean(x2, na.rm = TRUE))
  S1 <- sum((x1-m1)^2, na.rm=TRUE)
  S2 <- sum((x2-m2)^2, na.rm=TRUE)
  (n1 <- length(na.omit(x1)))
  (n2 <- length(na.omit(x2)))
  (df <- n1+n2-2)

  (pool.s <- sqrt( (S1 + S2) / df ) )
  (d <- (m2 - m1)/pool.s)

  s1 <- sd(x1, na.rm = TRUE)
  s2 <- sd(x2, na.rm = TRUE)
  (tstat <- (m2-m1) / (pool.s * sqrt((n1+n2)/(n1*n2))))
  (pval <- pt(abs(tstat),df,lower.tail=FALSE)*2)

  return(list(d=d,tstat=tstat,pval=pval))
}
