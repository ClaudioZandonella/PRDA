#########################
####    Utilities    ####
#########################

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

#----    is_single_numeric    ----

is_single_numeric <- function(x){
  length(x) == 1L && is.finite(x)
}

#----    Select arguments    ----

select_arguments <- function(arguments, names, remove=FALSE){

  selected_arg <- names(arguments) %in% names
  if (remove==TRUE){selected_arg <- !selected_arg}
  arguments[selected_arg]

}

#----    define_conf_level    ----

define_conf_level <- function(arguments){

  if("conf.level" %in% names(arguments)){
    warning("conf.level is set according to sig_level.")
  }
 1 - arguments$sig_level

}

#----    sample_groups    ----

sample_groups <- function(sample_n1, effect_size, sample_n2=NULL){

  res <- list(x = rnorm(sample_n1, mean=effect_size, sd=1),
              y = NULL)

  if(!is.null(sample_n2)){
    res$y <- rnorm(sample_n2, mean=0, sd=1)
  }
  return(res)
}

#----    check_test_method    ----

check_test_method <- function(effect_type, effect_size, sample_n1, sample_n2 = NULL, paired=FALSE, ...){

  # Cohen d
  if(effect_type == "cohen_d"){
    if(paired && ((is.null(sample_n2) || sample_n1!=sample_n2))){
      stop("If paired = TRUE sample_n1 and sample_n2 must be equal.")
    }

    groups <- sample_groups(sample_n1, effect_size, sample_n2)

    method <- t.test(groups$x, groups$y, paired=paired,...)$method
  }

  return(method)
}



#----
