######## Effect size based on the difference between means
##############################################################
#' @import truncnorm
#' @importFrom stats na.omit pt runif sd rnorm median
############################################
#' @title Compute Effect Sizes Based on Mean Differences
#'
#' @description The function computes effect size statistics based on differences between means
#' (Cohen's \eqn{d}, Hedges' \eqn{g} and Glass' \eqn{\Delta}) with the
#' corresponding \eqn{t} statistic and \eqn{p}-value.
#'
#' @param x1 numerical vector with the observed values of the first group.
#' @param x2 numerical vector with the observed values of the second group.
#' @param type a character string specifying the effect type, must be one of \code{"cohen"},
#' \code{"hedges"} or \code{"glass"}.
#'
#' @details Cohen's \eqn{d} is computed as
#' \deqn{d = \frac{m_2 - m_1}{s_{pool}}}{d = (m2 - m1) / s_pool}
#' where \eqn{m_1}{m1} and \eqn{m_2}{m2} are the group means and
#' \eqn{s_{pool}}{s_pool} is the pooled standard deviation.
#' @details Hedges' \eqn{g} is computed considering the approximate correction, as
#' \deqn{g = \left(1 - \frac{3}{4(n_1 + n_2 -2) -1}\right)\frac{m_2 - m_1}{s_{pool}}}{g = [1 - 3 / {4 * (n1 + n2 - 2) - 1}]  *  (m2 - m1) / s_pool}
#' @details Glass' \eqn{\Delta} is computed as
#' \deqn{\Delta = \frac{ m_2 - m_1}{s_1}}{\Delta = (m2 - m1) / s1}
#' where \eqn{s_1}{s1} is the standard deviation of the first group.
#' @details The \eqn{t} statistic and the corresponding \eqn{p}-value
#' are computed considering a two-tailed \eqn{t}-test for independent samples
#' with equal variance.
#'
#' @return The function returns a list with the following objects: \code{effect} (estimated effect size),
#' \code{tstat} (\eqn{t} value), \code{pval} (\eqn{p}-value).
#'
#' @author Massimiliano Pastore
#'
#' @examples
#' x1 <- c(0.43, -0.56, -0.39, -0.21, -0.58,  0.65)
#' x2 <- c(2.38, -0.96, -0.82,  0.28, -0.40,  0.34,  1.82)
#' effect_mdiff(x1, x2, type="cohen")
#' @export



effect_mdiff <- function(x1, x2, type=c("cohen","hedges","glass")){
  type <- match.arg(type)

  m1 <- mean(x1, na.rm=TRUE)
  m2 <- mean(x2, na.rm=TRUE)
  z <- m2-m1
  n1 <- length(na.omit(x1))
  n2 <- length(na.omit(x2))
  N <- (n1*n2)/(n1+n2)

  if(type=="cohen"){
    df <- n1+n2-2
    s_pool <- pool_sd(x1, x2, m1, m2, df)
    e <- z/s_pool
  }

  else if(type=="hedges"){
    df <- n1+n2-2
    s_pool <- pool_sd(x1, x2, m1, m2, df)
    correction <- 1 - (3/((4*df)-1))
    e <- correction*z/s_pool
  }

  else if(type=="glass"){
    df <- n1-1
    s1 <- sd(x1, na.rm=TRUE)
    e <- z/s1
  }

  tstat <- e*sqrt(N)
  pval <- pt(abs(tstat), df, lower.tail=FALSE)*2
  out <- list("effect"=e, "tstat"=tstat, "pval"=pval)
  return(out)
}
