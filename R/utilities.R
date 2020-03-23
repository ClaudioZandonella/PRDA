#########################
####    Utilities    ####
#########################

#----    eval_args    ----

eval_args <- function(...){
  .args <- as.list(match.call()[-1])

  return(.args)
}

#----

