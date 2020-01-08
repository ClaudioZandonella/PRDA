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
