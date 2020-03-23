#################################
####    simulate_analysis    ####
#################################

#----    simulate_analysis    ----

simulate_analysis <- function(sample_group1, effect_size, sample_group2, effect_type, alternative, B, ...){


  if (effect_type == "cohen_d"){
    analysis_simulated <- analysis_cohen(sample_group1 = sample_group1,
                                         sample_group2 = sample_group2,
                                         effect_size = effect_size,
                                         alternative = alternative,
                                         B = B, ...)
    } else if (effect_type == "correlation"){

    analysis_simulated <- analysis_correlation(sample_group1 = sample_group1,
                                               effect_size = effect_size,
                                               alternative = alternative,
                                               B = B, ...)
    }

  return(analysis_simulated)
}

#----    analysis_cohen    ----

#' Title
#'
#' @param sample_group1 numeric value
#' @param sample_group2 numeric value
#' @param effect_size numeric value
#' @param alternative character value
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#' @importFrom stats rnorm t.test
#'
analysis_cohen <- function(sample_group1, sample_group2, effect_size, alternative, B, ...){

  arguments <- eval_args(...)
  arguments$alternative <- alternative

  res <- replicate(B,{
    x1 <- rnorm(sample_group1, mean=0, sd=1)
    x2 <- rnorm(sample_group2, mean=effect_size, sd=1)

    arguments$x <- x1
    arguments$y <- x2

    do.call(t.test,arguments)
  })


  return(res)
  }
#----    analysis_correlation    ----

#' Title
#'
#' @param sample_group1 numeric value
#' @param effect_size numeric value
#' @param alternative character value
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#' @importFrom MASS mvrnorm
#' @importFrom stats cor.test
#'
analysis_correlation <- function(sample_group1, effect_size, alternative, B, ...){

  arguments <- eval_args(...)
  arguments$alternative <- alternative

  res <- replicate(B,{
    d<-mvrnorm(n = sample_group1, mu=c(0,0), Sigma=matrix(c(1, effect_size, effect_size, 1), ncol=2))
    arguments$x <- d[,1]
    arguments$y <- d[,2]

    do.call(cor.test,arguments)
  })

  return(res)
}

#----

