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

# select_arguments <- function(arguments, names, remove=FALSE){
#
#   selected_arg <- names(arguments) %in% names
#   if (remove==TRUE){selected_arg <- !selected_arg}
#   arguments[selected_arg]
#
# }

#----    define_conf_level    ----

define_conf_level <- function(arguments){

  if("conf.level" %in% names(arguments)){
    warning("conf.level is set according to sig_level.")
  }
 1 - arguments$sig_level

}

#----    list2data    ----

list2data <- function(list, transpose=TRUE, select=NULL){
  if(transpose) list <- t(list)

  if(!is.null(select)){
    slected_arg = dimnames(list)[[2]] %in% select

    save_names = dimnames(list)[[2]][slected_arg]
    save_dim = dim(list)
    save_dim[2] = length(save_names)

    list <- list[rep(slected_arg, each=dim(list)[1])]
    dim(list) <- save_dim
    dimnames(list) <- list(NULL,save_names)
  }

  data <- as.data.frame(matrix(unlist(list),ncol=dim(list)[2], dimnames = dimnames(list)))

  return(data)
}


#----
