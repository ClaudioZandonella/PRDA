#################################
####    PRDA beta version    ####
#################################

#----    Package documentation    ----

#' PRDAbeta: A Package for Prospective and Retrospective Design Analysis.
#'
#' The PRDAbeta package provides two main important functions:
#' \code{prospective()} and \code{retrospective()}.
#'
#' @section PRDAbeta functions:
#' The PRDAbeta functions ...
#'
#' @importFrom stats rnorm t.test cor.test qt pt sd var cor
#' @importFrom MASS mvrnorm
#' @importFrom utils capture.output
#'
#' @docType package
#' @name PRDAbeta
#'
NULL


#----    use_rcpp and rcppArmadillo    ----

## usethis namespace: start
#' @useDynLib PRDAbeta, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @importFrom Rcpp evalCpp
## usethis namespace: end
NULL

#----    onAttach    ----

#' Initial message
#'
#' @param libname used to define the libname
#' @param pkgname used to define the pkgname
#'
#' @return Initial message with suggestion to loook at vignettas
#'
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to PRDA (beta version) \nTo get started look at vignettas")
}

#----
