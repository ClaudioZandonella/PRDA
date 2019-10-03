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
# 
######################################################
#' @title Design Analysis
#' @description Performs a prospective or retrospective design analysis for the comparison between two independent groups,
#' given a plausible value of effect size (Cohen's \eqn{d}). If \code{power} is defined,
#' the function estimates the sample size (per group) needed to reach that power. Instead, if 
#' the number of observations (per group) is defined (\code{n}), the function estimates 
#' the power level, type M and type S errors given that sample size. 
#' @references Altoè, G., Bertoldo, G., Zandonella Callegher, C., Toffalini E., Calcagnì, A., Finos, L., Pastore, M. (2019).
#' Enhancing statistical inference in psychological research via prospective and retrospective design analysis. \url{https://arxiv.org/abs/1909.13773} (Submitted to \emph{Frontiers in Psychology}).
#' @references Cohen, J. (1988). \emph{Statistical power analysis}. Academic Press, San Diego, CA. 
#' @references Gelman, A., Carlin, J. (2014). Beyond power calculations: Assessing type S (sign) and type M
#' (magnitude) errors. \emph{Perspectives on Psychological Science}, 9(6), 641--651. \url{https://doi.org/10.1177/1745691614551642}  
#' @param n = number of observations (per group)
#' @param d = plausible effect size (Cohen's \eqn{d})
#' @param sig.level = significance level
#' @param power = required power of the test
#' @param rangen = range of \eqn{n} (per group), only if \code{n=NULL}
#' @param B = number of replicates
#' @param tol = tolerance in estimation
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @details The function can be used to compute the power (\code{power})
#' or the sample size needed (\code{n}). In the first case, users have to set \code{power=NULL} 
#' and provide values for \code{d} and \code{n}. In the second case, users have to set 
#' \code{n=NULL} and provide values for \code{d} and \code{power}.
#' @details Users can also define through \code{rangen}, the minimum and maximum values
#' for the range number of observations (per group). When the function evaluates the sample size 
#' (per group) needed to reach a given power, only values within this range are considered. Default
#' settings are \code{rangen = c( 2, 1000 )}.
#' @note Plausible \eqn{d} is a fixed value, \code{n} is the sample size per group.
#' @seealso \code{\link{power.t.test}}
#' @examples
#' design_analysis( n = 10, d = .5 )
#' design_analysis( d = .5, power = .10 )
#' design_analysis( d = .2, power = .90, rangen = c(5,50) )
#' design_analysis( d = .2, power = .90, rangen = c(50,100) )
#' @return Returns a list with the following objects: \code{d} (effect size),
#' \code{power}, \code{n} (sample size), \code{typeS} (type S error), 
#' \code{typeM} (type M error).
#' @export
design_analysis <- function( d, n = NULL, power = NULL, sig.level = 0.05,
                             B = 1e4 , rangen = c( 2, 1000 ),  tol=.005) {
  if (d <= 0) {
    stop("A d greater than 0 must be entered")
  }
  
  if (sum(sapply(list(n, power), is.null)) != 1) {
    stop("Exactly one of 'n' or 'power' must be NULL")
  }
  
  if (is.null(n)) {
    out <- design_analysis_n( d=d, power=power, sig.level=sig.level, rangen=rangen, B=B )
  } else {
    if (is.null(power)) {
      out <- design_analysis_power( n=n, d=d, sig.level=sig.level, B=B )
    } 
  }
  return(out)
}

##### It performs prospective design analysis [Internal Function]
######################################################
#' @title Design Analysis: required sample size
#' @description Internal function. In a comparison between two independent groups, 
#' given a plausible value of effect size (Cohen's \eqn{d}), 
#' the function estimates the sample size (per group) needed to reach a required \code{power} level.
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @note Internal Function, called from function \code{\link{design_analysis}}.
#' @param d = plausible effect size (Cohen's \eqn{d})
#' @param sig.level = significance level
#' @param power = required power of the test
#' @param rangen = range of \eqn{n} (per group)
#' @param B = number of replicates
#' @param tol = tolerance
#' @export 
design_analysis_n <- function( d, power, sig.level = 0.05, rangen = c(2,1000), B = 1e4, tol = .005 ){
  
  (n_seq <- seq( rangen[1], rangen[2], by = 1 ))
  (n_target <- round(median(n_seq)))
  find_power <- FALSE
  
  ## check with maximum N
  cat("Estimating power with n =",rangen[2],"\n")
  (est_P <- sim_estimate( n1 = rangen[2], n2 = rangen[2], sim_d = d, target_d=d, sig.level = sig.level, B = B ))
  (est_power <- est_P[1,]$power)
  
  if ( est_power < power ) {
    cat(paste0("Actual power = ", est_power, " with n = ", rangen[2], " (per group); " ),"\n")
    cat(paste0("   try to increase maximum of rangen > ", rangen[2],"."),"\n")
    out <- NULL
  } else {
    
    ## estimating power
    while( (!find_power) ) {
      cat("Estimating power with n =",n_target,"\n")
      (est_P <- sim_estimate( n1 = n_target, n2 = n_target, sim_d = d, target_d=d, sig.level = sig.level, B = B))
      (est_power <- est_P[1,]$power)
      
      if ( (est_power<=(power+tol)) & (est_power>(power-tol)) ) {
        find_power <- TRUE
      } else {
        
        if (length(n_seq)==1) {
          print(n_seq)
          stop(" ")
        }
        
        if ( est_power > (power-tol) ) {
          (n_seq <- seq( min(n_seq), n_target, by = 1))
          (n_target <- round(median(n_seq)))
        } else {
          (n_seq <- seq( n_target, max(n_seq), by = 1))
          (n_target <- round(median(n_seq)))
        }
      }
    }
    out <- list( d = d, power = power, n = n_target, typeS=est_P[1,"typeS"],typeM=est_P[1,"typeM"])
  }
  if (!is.null(out))  return(out)
}


##### It calculates power, typeM, typeS based on B replicates [Internal Function]
# Note for design_analysis:  target_d is equal to sim_d
##############################################################
#' @title Internal function
#' @description Internal function. In a comparison between two independent groups,
#' the function computes through \code{B} simulations the expected power, 
#' Type-S and Type-M errors, given a plausible effect size (\code{target_d}) and the sample size of each group.
#' @author Massimiliano Pastore, Gianmarco Altoè
#' @note Called from \code{\link{design_est}}. For \code{design_analysis}: \code{target_d} is equal to \code{sim_d}.
#' @param B = number of replicates
#' @param n1 = sample size group 1
#' @param n2 = sample size group 2
#' @param sim_d = simulated effect size (Cohen's \eqn{d})
#' @param target_d = target effect size (Cohen's \eqn{d})
#' @param sig.level = significance level 
#' @return Returns a \code{data.frame} with the results of the simulation and the computed values of 
#' \code{power}, \code{typeS}, and \code{typeM} errors.
sim_estimate <- function( sim_d, target_d = sim_d, n1, n2 = n1, sig.level = .05, B = 1e4 ) {
  outsim <- t(replicate( B, {
    x1 <- rnorm(n1)
    x2 <- rnorm(n2,sim_d)
    d <- cohen_d(x1,x2)
    sim <- c(d$d,d$tstat,d$pval,sim_d)
  }))
  colnames(outsim) <- c("est_d","tstat","pval","sim_d")
  (outsim <- data.frame(outsim))
  (outsim$power <- sum( outsim$pval < sig.level ) / nrow(outsim))
  (outsim$typeS <- with(outsim, sum( (pval < sig.level) & (tstat<0)) / sum(pval < sig.level)  ) )
  (outsim$typeM <- with(outsim, mean(abs(est_d[pval< sig.level]))) / target_d)
  return(outsim)
}

######################################################
#' @title Design Analysis: estimated power 
#' @description Internal function. In a comparison between two independent groups,
#' the function estimates the power level, type M and type S errors given the specified 
#' sample size per group (\code{n}) and a plausible value of effect size (Cohen's \eqn{d}). 
#' @note Called from function \code{\link{design_analysis}}.
#' @author Massimiliano Pastore, Gianmarco Altoe
#' @param n = number of observations (per group)
#' @param d = plausible effect size (Cohen's \eqn{d})
#' @param sig.level = significance level
#' @param B = number of replicates
#' @param tol = tolerance
design_analysis_power <- function( n, d, sig.level = 0.05, B = 1e4 ,tol=.005 ) {
  out <- sim_estimate(sim_d=d,target_d=d,n1=n,sig.level=sig.level,B=B)
  out=list(d=d,n=n,power=out[1,"power"],typeS=out[1,"typeS"],typeM=out[1,"typeM"])
  return(out)
}

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
    y <- sampling_d(target_d_limits,distribution = distribution,B0=B0, k = k) ###
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

