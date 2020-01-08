
#' @D range difference mean 
#' @d range difference mean 

library(truncnorm)

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

#' @n1
#' @n2
#' @k threshold Bayes Factor
#' 

BayesError <- function(n1, n2 = n1, B = 1e4, D, distribution = c("uniform", "normal"), thr = 3){
  
  d <- sampling_d(target_limits = c(-1,1), distribution = distribution, k = 1/6, B0 = B)$y 
  BFv <- Vectorize(BF, vectorize.args = "d")
  int <- lapply(d,function(x) BFv(n1,n2= n1,analysisPrior= NULL, D,x))
  bf <- sapply(c(1:B),function(x) int[[x]][1])
  z <- sapply(c(1:B),function(x) int[[x]][2])

  #Compute errors
  power <- sum(bf > thr) / B
  eN <- sum(bf < 1/thr) / B
  eI <- sum( 1/thr <= bf & bf <= thr) / B
  eS <- sum(bf > thr & sign(z) != sign(d))/sum(bf > thr)
  eM <- mean((abs(z)/d)[which(bf > thr)]) 
  out <- cbind(power, eN, eI, eS, eM)
  colnames(out) <- c("Power", "False Negative", "Indecisive under H1", "error Sign", "Overestimation")
  
  return(out = out)
}

#BayesError(n1=10000,n2=10000,B=1e4,distribution = "normal", D = c(-20,20), thr = 3)
