#################################
####    simulate_analysis    ####
#################################

#----    simulate_analysis    ----

# simulate_analysis <- function(sample_n1, effect_size, sample_n2, effect_type, alternative, B, ...){
#
#
#   if (effect_type == "cohen_d"){
#     analysis_simulated <- analysis_cohen(sample_n1 = sample_n1,
#                                          sample_n2 = sample_n2,
#                                          effect_size = effect_size,
#                                          alternative = alternative,
#                                          B = B, ...)
#     } else if (effect_type == "correlation"){
#
#     analysis_simulated <- analysis_correlation(sample_n1 = sample_n1,
#                                                effect_size = effect_size,
#                                                alternative = alternative,
#                                                B = B, ...)
#     }
#
#   return(analysis_simulated)
# }

#----    retrospective_cohen    ----

#' Title
#'
#' @param sample_n1 numeric value
#' @param sample_n2 numeric value
#' @param effect_size numeric value
#' @param alternative character value
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#' @importFrom stats rnorm t.test
#'
retrospective_cohen <- function(sample_n1, sample_n2, effect_size, alternative, sig_level, B, ...){


  arguments <- as.list(match.call()[-1])
  arguments$conf.level <- 1-sig_level

  # Remove arguments
  arguments <- select_arguments(arguments, c("sample_n1", "sample_n2",
                                "effect_size", "B","sig_level", "seed"), remove = T)

  res <- replicate(B,{
    x1 <- rnorm(sample_n1, mean=0, sd=1)
    arguments$x <- x1

    if(!is.null(sample_n2)){
      x2 <- rnorm(sample_n2, mean=effect_size, sd=1)
      arguments$y <- x2
    }

    do.call(t.test,arguments)
  })

  return(res)
  }
#----    retrospective_correlation    ----

#' Title
#'
#' @param sample_n1 numeric value
#' @param effect_size numeric value
#' @param alternative character value
#' @param B numeric value
#' @param ... other variables passed to
#'
#' @return a matrix
#' @importFrom MASS mvrnorm
#' @importFrom stats cor.test
#'
retrospective_correlation <- function(sample_n1, effect_size, alternative, sig_level, B, ...){

  arguments <- as.list(match.call()[-1])
  arguments$conf.level <- 1-sig_level

  # Remove arguments
  arguments <- select_arguments(arguments, c("sample_n1", "sample_n2",
                                "effect_size", "B", "sig_level", "seed"), remove = T)

  res <- replicate(B,{
    obs<-mvrnorm(n = sample_n1, mu=c(0,0), Sigma=matrix(c(1, effect_size, effect_size, 1), ncol=2))
    arguments$x <- obs[,1]
    arguments$y <- obs[,2]

    do.call(cor.test,arguments)
  })

  return(res)
}

#----

