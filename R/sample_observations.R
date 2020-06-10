###################################
####    sample observations    ####
###################################

#----    sample_groups    ----

sample_groups <- function(sample_n1, effect_size, sample_n2=NULL){

  if(is.null(sample_n2)){
    res <- list(x = rnorm(sample_n1, mean=effect_size, sd=1),
                y = NULL)
  }else{
    res <- list(x = rnorm(sample_n1, mean=effect_size, sd=1),
                y = rnorm(sample_n1, mean=0, sd=1))
  }

  return(res)
}

#----    sample_obs_cor    ----

#' Title
#'
#' @param sample_n1 numeric value
#' @param effect_size numeric value
#'
#' @return matrix of observations
#'
#' @importFrom MASS mvrnorm
#'
sample_obs_cor <- function(sample_n1, effect_size){

  obs <- mvrnorm(n=sample_n1,mu=c(0,0),Sigma=matrix(c(1,effect_size,effect_size,1),ncol=2))

  return(list(x = obs[,1], y = obs[,2]))
}

#----    my_mvrnorn    ----

my_mvrnorm <-function(n = 1, Eigen_matrix){

  X <- matrix(rnorm(2 * n), n)
  X <- Eigen_matrix %*% t(X)

  return(list(x = X[1,], y = X[2,]))
}
#----

