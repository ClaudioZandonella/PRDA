########################
####    my tests    ####
########################


#----    my_t_test    ----

#' my_t_test
#'
#' @param x numeric value
#' @param y numeric value
#' @param alternative character value
#' @param mu numeric value
#' @param paired logic value
#' @param var.equal logic value
#' @param conf.level numeric value
#' @param ... other options
#'
#' @return list with p.values and Cohen's d estimates
#' @importFrom stats pt sd var
#'
my_t_test <-function(x, y = NULL, alternative = "two.sided",
           mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
           ...){
    if (paired) {
      x <- x-y
      y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)

    # Case paired or one-sample
    if(is.null(y)) {
      df <- nx-1
      stderr <- sqrt(vx/nx)
      tstat <- (mx-mu)/stderr
      estimate <- if(paired) (nx-2)/(nx-1.25) * mx/sd(x) else (mx-mu)/sd(x) # todo
    } else {
      ny <- length(y)
      my <- mean(y)
      vy <- var(y)
      if(var.equal) {
        df <- nx+ny-2
        v <- (nx-1)*vx + (ny-1)*vy
        v <- v/df
        stderr <- sqrt(v*(1/nx+1/ny))
        estimate <- (1 - (3/((4*df)-1))) * (mx-my)/sqrt(v)
      } else {
        stderrx <- sqrt(vx/nx)
        stderry <- sqrt(vy/ny)
        stderr <- sqrt(stderrx^2 + stderry^2)
        df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
        estimate <- (mx-my)/sqrt((vx+vy)/2)
      }
      tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "two.sided") {
      pval <- 2 * pt(-abs(tstat), df)
    }
    else if (alternative == "greater") {
      pval <- pt(tstat, df, lower.tail = FALSE)
    }
    else {
      pval <- pt(tstat, df)
      }
    rval <- list(p.value = pval,
                 estimate = estimate)
    return(rval)
}

#----

#----
