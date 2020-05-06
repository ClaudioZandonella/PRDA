#########################
####    Utilities    ####
#########################

#----    eval_args    ----

eval_args <- function(...){
  .args <- as.list(match.call()[-1])

  return(.args)
}

#----    match_call    ----

match_call <- function(definition = sys.function(sys.parent()),
                       call = sys.call(sys.parent()),
                       expand.dots = TRUE,
                       default= TRUE,
                       envir = parent.frame(2L),
                       envir_mget = parent.frame(1L)) {
  call <- match.call(definition, call, expand.dots, envir)
  formals <-mget(names(formals(definition)), envir_mget)

  if(expand.dots && '...' %in% names(formals))
    formals[['...']] <- NULL

  common_args <- names(call)[which(names(call) %in% names(formals))]

  for(i in common_args)
    call[i] <- list( formals[[i]] )

  if(default)
  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )


  match.call(definition, call, TRUE, envir)
}

#----

is_single_numeric <- function(x){
  is.finite(x) && length(x) == 1
}
#----

