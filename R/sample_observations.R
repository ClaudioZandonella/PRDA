###################################
####    sample observations    ####
###################################

#----    sample_groups    ----

sample_groups <- function(sample_n1, effect_size, sample_n2=NULL){

  if(is.null(sample_n2)){
    res <- list(x = rnorm(sample_n1, mean=effect_size, sd=1),
                y = NULL)
  }else{
    res <- list(x = rnorm(sample_n1, mean=effect_size, sd=1),
                y = rnorm(sample_n1, mean=0, sd=1))
  }

  return(res)
}

#----

