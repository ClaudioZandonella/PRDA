# Computes effects size based on differences between means
# 
# Cohen's d
#Cohen, J. (1969). Statistical power analysis of the behavioral sciences. San
#Diego, CA: Academic Press.
#
# Glass' Δ
#Glass, G.V. (1976). Primary, secondary, and meta-analysis of research.
#Educational Researcher, 5, 3–8
#
# Hedges' g
#Hedges, L.V. (1981). Distribution theory for Glass’s estimator of effect size
#and related estimators. Journal of Educational Statistics, 6, 107–128
# Ψ root-mean-square standardized effect
#
# See: https://en.wikipedia.org/wiki/Effect_size


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

hedge_g <- function(x1,x2) {
  
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
  (pval <- pt(abs(tstat),df,lower.tail=FALSE)*2, ncp = sqrt((n1*n2)/(n1+n2))*d)
  
  return(list(d=d,tstat=tstat,pval=pval))
}


glass_delta <- function(x1,x2) {
  
  (m1 <- mean(x1, na.rm = TRUE))
  (m2 <- mean(x2, na.rm = TRUE))
  S1 <- sum((x1-m1)^2, na.rm=TRUE)
  S2 <- sum((x2-m2)^2, na.rm=TRUE)
  (n1 <- length(na.omit(x1)))
  (n2 <- length(na.omit(x2)))
  (df <- n2-1)
  
  pool.s <- sqrt(S2/n2-1)
  d <- (m2 - m1)/pool.s
  
  (tstat <- (m2-m1) / (pool.s * sqrt((n1+n2)/(n1*n2))))
  (pval <- pt(abs(tstat),df,lower.tail=FALSE)*2)
  
  return(list(d=d,tstat=tstat,pval=pval))
}


#psi <- function(x1,x2) {
  
  #to do
  
#  return(list(d=d,tstat=tstat,pval=pval))
#}

#' @analysisPrior prior distribution 
#' @D range difference mean 
#' @n1 number of observation of the first group 
#' @n2 number of observation of the second group 
#' @d mean difference

BF <- function(n1,n2= n1,analysisPrior= NULL, D,d) {
  
  x1 <- rnorm(n1)
  x2 <- rnorm(n2, d)
  (m1 <- mean(x1, na.rm = TRUE))
  (m2 <- mean(x2, na.rm = TRUE))
  
  z <- m2 - m1
  
  if(n1> 1000 & n2 > 1000){
    n1 = 1000
    n2 = 1000
  }
  f <- function(x) { exp(((n1*n2)/(n1 + n2))*x*(z - (x/2))) * dcauchy(x,location = 0, scale = 1/sqrt(2))}
  B <- integrate(f,lower = D[1], upper = D[2])$value
  
  return(cbind(B = B, z = z))
  
}

#TEST

#x1 <- rnorm(1000, mean = 3, sd = 1)
#x2 <- rnorm(1000, mean = 3, sd = 1)
#D <- c(-3,3)
#BF(x1 = x1, x2 = x2, analysisPrior = NULL, D = D)


