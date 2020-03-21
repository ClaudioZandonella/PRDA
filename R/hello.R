# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'



#' Hello world
#'
#' @return A string of text
#'
#' @export
#'
hello <- function() {
  print("Hello, world!")
}


#----    An internal function    -----

#' An internal function
#'
#' @param x numeric value
#' @param y numeric value
#'
#' @return the sum of the two values
#'
simple_sum <- function(x,y){
  x+y
}

#----


