#################################
####    PRDA beta version    ####
#################################

#----    Package documentation    ----

#' PRDAbeta: A package for cunduct prospective and retospective design analysis.
#'
#' The PRDAbeta package provides two main important functions:
#' \code{prospective()} and \code{retrospective()}.
#'
#' @section PRDAbeta functions:
#' The PRDAbeta functions ...
#'
#' @docType package
#' @name PRDAbeta
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
