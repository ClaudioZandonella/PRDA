######## It performs retrospective design analysis given a plausible interval and a distribution for d.
########  It works also for different sample size per group.
########  It works also given a fixed value of d.
#########################################################
#' @title Retrospective Design Analysis
#' @description Performs retrospective design analysis for the comparison between two independent groups.
#' It estimates power, type S and type M errors given the sample size of each group (\code{n1} and
#' \code{n2}) and a plausible effect size (Cohen's \eqn{d}). The plausible effect size can be defined as a single value
#' (\code{target_d}) or as a range of possible values. In the second case, the interval of possible values
#' (\code{target_d_limits}) as well as the type of distribution (\code{distribution}) can be specified.
#' Distributions available are uniform (default) or doubly truncated normal.
#' @details Default settings consider equal size for the two groups (\code{n1} = \code{n2}).
#' @details When a plausible effect size is defined as a range of possible values, the  function \code{\link{sampling_d}}
#'  is used to sample \code{B0} values of \eqn{d} from the specified distribution, “\code{uniform}”, by default,
#'  or (doubly truncated) “\code{normal}”. Power, type S and type M are computed for each sampled value of \eqn{d} but only
#'  mean values are reported in the output. To obtain results for each sampled value of \eqn{d}, set option
#'  \code{return_data = TRUE}.
#' @return It returns two lists:
#' @return \item{call }{reporting input values: \code{n1}, \code{n2}, \code{target_d}, \code{target_d_limits} e \code{distribution}.}
#' @return \item{results }{reporting output values: \code{sim_d} (mean of simulated \eqn{d}, see details), \code{est_d} (estimated \eqn{d}), \code{power}, \code{typeS} e \code{typeM}.}
#' @return If \code{return_data = TRUE}, a \code{data.frame} (\code{B0}  rows and 3 columns)
#' is returned with complete information for each sampled value of \eqn{d}:
#'  \code{power}, \code{typeS} and \code{typeM}.
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @param n1 = group 1 sample size
#' @param n2 = group 2 sample size (default = n1)
#' @param target_d = plausible effect size (Cohen's \eqn{d})
#' @param target_d_limits = vector with lower and upper bounds of plausible effect size (Cohen's \eqn{d})
#' @param distribution = probability distribution function \code{uniform} or (truncated) \code{normal}
#' @param sig.level = significance level
#' @param return_data = logical, by default is FALSE
#' @param B = number of replicates
#' @param B0 = number of replicates for simulating \eqn{d} calling \code{\link{sim_estimate}}
#' @param k = constant for defining the standard deviation of truncated normal
#' @examples
#' design_est( target_d = .25 ) # default sample size is 5 for both groups
#' design_est( n1 = 100, target_d = .25 )
#' design_est( n1 = 100, n2 = 50, target_d = .25 ) # different sample sizes
#'
#' ### Dealing with uncertainty on d ###
#' design_est( n1 = 100, target_d_limits = c( .2, .3 ) ) # uniform distribution
#' design_est( n1 = 100, target_d_limits = c( .2, .3 ), distribution = "normal" ) # normal distribution
#'
#' ### Get complete information for each sampled value of d  ###
#' out <- design_est(n1=50, n2=48, distribution="normal",
#'                   target_d_limits = c(.20,.30), return_data=TRUE)
#' out
#'
#' #### To draw the distribution of simulated power, Type S and Type M errors:  ####
#' hist(out$results$data$power,main="",xlab="power") # power
#' hist(out$results$data$typeS,main="",xlab="typeS") # Type S error
#' hist(out$results$data$typeM,main="",xlab="typeM") # Type M error
#' @export
design_est<- function( n1 = 5, n2 = n1, target_d = NULL, target_d_limits = NULL, distribution = c("uniform","normal"),
                       k = 1/6, sig.level = 0.05, B = 500, B0 = 500, return_data = FALSE ) {

  distribution <- match.arg(distribution)

  if (sum(sapply(list(target_d,target_d_limits), is.null)) != 1) {
    stop("Exactly one of 'target_d' and 'target_d_limits' must be NULL")
  }

  ########### fixed d
  if (!is.null(target_d)) {
    if (target_d<=0) stop("Target_d must be greater than 0")
    temp <- sim_estimate(sim_d=target_d, target_d=target_d, n1 = n1, n2 = n2, B = B )
    results <- list(power=temp[1,"power"],typeS=temp[1,"typeS"],typeM=temp[1,"typeM"])
    outList <- list(call=list(n1=n1, n2=n2, target_d=target_d, B=B),results=results)
    return(outList)
  }

  ############## plausible interval and distribution for d
  if (!is.null(target_d_limits)) {
    if (min(target_d_limits)<0) stop("Minimum of target_d_limits must be greater than 0")
    (call_est <- list(n1=n1, n2=n2, target_d_limits=target_d_limits, distribution=distribution,k=k,B=B,B0=B0))
    (my <- (sum(target_d_limits)/2))
    (sy <- diff(target_d_limits)*k)
    y <- sampling_e(target_d_limits,distribution = distribution,B0=B0, k = k) ###
    sim_cohen <- y$y

    #data <- NULL
    #for (i in 1:B0){
    #  temp <- sim_estimate(sim_cohen[i], target_d=my, n1 = n1, n2 = n2, B = B )
    #  data <- rbind(data,c(temp[1,"power"],temp[1,"typeS"],temp[1,"typeM"]))
    #}

    data <- t(sapply(1:B0, function(b){
      temp <- sim_estimate(sim_cohen[b], target_d=my, n1 = n1, n2 = n2, B = B )
      c(temp[1,"power"],temp[1,"typeS"],temp[1,"typeM"])
    }))
    data <- data.frame(data)
    colnames(data) <- c("power","typeS","typeM")

    (power <- mean(data$power))
    (typeS <- mean(data$typeS,na.rm = TRUE))
    (typeM <- mean(data$typeM,na.rm = TRUE))

    results <- list(power=power,typeS=typeS,typeM=typeM)
    if (return_data) {
      results$data <- data

      outList <-  list(call=call_est,results=results)
    } else {
      outList <-  list(call=call_est,results=results)
    }
    return(outList)
  }
}
