#################################
####    PRDA beta version    ####
#################################

#----    Package documentation    ----

#' PRDA: A Package for Prospective and Retrospective Design Analysis.
#'
#' The PRDA package provides two main important functions:
#' \code{prospective()} and \code{retrospective()}.
#'
#' @section PRDA functions:
#' The PRDA functions ...
#'
#' @importFrom stats rnorm t.test cor.test qt pt sd var cor
#' @importFrom MASS mvrnorm
#' @importFrom utils capture.output
#'
#' @docType package
#' @name PRDA
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
