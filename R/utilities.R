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
  formals <- mget(names(formals(definition)), envir_mget)

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

is_single_numeric <- function(x, infinite = FALSE){

  if(infinite){
    length(x) == 1L && is.numeric(x)
  }else{
    length(x) == 1L && is.finite(x)
  }
}


#----    list2data    ----

list2data <- function(list, transpose=TRUE, select=NULL){
  if(transpose) list <- t(list)

  if(!is.null(select)){
    slected_arg <- dimnames(list)[[2]] %in% select

    save_names <- dimnames(list)[[2]][slected_arg]
    save_dim <- dim(list)
    save_dim[2] <- length(save_names)

    list <- list[rep(slected_arg, each=dim(list)[1])]
    dim(list) <- save_dim
    dimnames(list) <- list(NULL,save_names)
  }

  data <- as.data.frame(matrix(unlist(list),ncol=dim(list)[2],
                               dimnames = dimnames(list)))

  return(data)
}


#----    round_arg    ----

round_arg <- function(list_name, n_round){
  arguments <- unlist(lapply(list_name, is.numeric))
  if(sum(arguments)!=0){
    list_name[arguments] <- lapply(list_name[arguments], round, n_round)
  }
  return(list_name)
}


#----    sing_effect    ----

sign_effect <- function(critical_effect, alternative){
  if(alternative == "two_sided"){
    critical_effect <- paste0("\u00b1 ",critical_effect)
  } else {
    critical_effect <- paste0(critical_effect)
  }

  return(critical_effect)
}


#----    with_seed    ----

with_seed <- function(seed, code) {
  code <- substitute(code)
  if(!exists(".Random.seed")) rnorm(1) # Ensure .Random.seed exist
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  return(eval.parent(code))
}


#----
