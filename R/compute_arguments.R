#################################
####    Compute arguments    ####
#################################


#----    pool_sd    ----

pool_sd <- function(x, y){
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)

  sqrt((sum((x-mx)^2)+sum((y-my)^2))/(nx + ny -2))
}

#----
