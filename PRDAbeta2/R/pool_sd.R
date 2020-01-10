######## Pooled standard deviation [Internal Function]
##############################################################
#' @import truncnorm
#' @importFrom stats na.omit pt runif sd rnorm median
############################################
#' @title Pooled Standard Deviation
#' @description Internal function. It computes the pooled standard deviation of two groups.
#' @note Called from \code{\link{effect_mdiff}}.
#' @param x1 = numerical vector with the observed values of the first group.
#' @param x2 = numerical vector with the observed values of the second group.
#' @param m1 = mean of the first group.
#' @param m2 = mean of the second group.
#' @param df = degrees of freedom.
#' @return Returns the pooled standard deviation.
#' @details The degrees of freedom \code{df} are given by the sum of the two group sizes minus 2.
#' @author Massimiliano Pastore
#' @examples
#' x1 <- c(0.43, -0.56, -0.39, -0.21, -0.58,  0.65)
#' x2 <- c(2.38, -0.96, -0.82,  0.28, -0.40,  0.34,  1.82)
#' m1 <- mean(x1, na.rm=TRUE)
#' m2 <- mean(x2, na.rm=TRUE)
#' df <- length(na.omit(x1)) + length(na.omit(x2)) - 2
#' pool_sd(x1, x2, m1, m2, df)
#' @export



pool_sd <- function(x1, x2, m1, m2, df){
  S1 <- sum((x1-m1)^2, na.rm=TRUE)
  S2 <- sum((x2-m2)^2, na.rm=TRUE)
  s_pool <- sqrt((S1+S2)/df)
  return(s_pool)
}
