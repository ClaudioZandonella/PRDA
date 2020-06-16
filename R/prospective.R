###########################
####    Prospective    ####
###########################

#----    Prospective    ----


#' Prospective Design Analysis
#'
#' Given the hypothetical population effect size and the required power level,
#' the function \code{prospective()} performs a prospective design analysis for
#' Cohen's \emph{d} (\emph{t}-test comparing group means) or  Pearson's
#' correlation test between two variables. According to the defined alternative
#' hypothesis and significance level, the required sample size is computed
#' together with the associated Type-M error, Type-S error, and the the critical
#' correlation value (i.e., the minimum absolute effect size value that would
#' result significant).
#'
#' @param effect_size a numeric value or function (see details) indicating the
#'  hypothetical population effect size.
#' @param power a numeric value indicating the required power level.
#' @param ratio_n2 a numeric value indicating the ratio between \code{sample_n1}
#'   and \code{sample_n2}.
#' @param effect_type a character string specifying the effect type, must be
#'  "cohen_d" (default, Cohen's \emph{d} standardised means difference) or
#'  "pearson" (Pearson's correlation). You can specify just the initial letter.
#' @param alternative a character string specifying the alternative hypothesis,
#'  must be one of "two.sided" (default), "greater" or "less". You can specify
#'  just the initial letter.
#' @param sig_level a numeric value indicating the significance level on which
#'  the alternative hypothesis is evaluated.
#' @param B a numeric  value indicating the number of iterations. Increase the
#'  number of iterations to obtain more stable results.
#' @param seed a numeric value indicating the seed for random number generation.
#'  Set the seed to obtain results reproducible.
#' @param tl optional value indicating the lower truncation point if
#'  \code{effect_size} is defined as a function.
#' @param tu optional value indicating the upper truncation point if
#'  \code{effect_size} is defined as a function.
#' @param B_effect a numeric  value indicating the number of sampled effect size
#'  if \code{effect_size} is defined as a function. Increase the number to
#'  obtain more stable results.
#' @param sample_range a length-2 numeric vector indicating the minimum and maximum
#'   sample size of the first group (\code{sample_n1}).
#' @param tol a numeric value indicating the tolerance of required power level.
#' @param display_message a logical variable indicating whether to display or
#'   not the information about computational steps.
#' @param ... further arguments to be passed to or from methods.
#'
#'@return A list with class "design_analysis" containing the following
#'  components:
#'    \item{design_analysis}{a character string indicating the type of design
#'    analysis: "prospective".}
#'    \item{call_arguments}{a list with all the arguments passed to the
#'    function.}
#'    \item{effect_info}{a list with all the information regarding the
#'    considered hypothetical population effect size. The list includes:
#'    \code{effect_type} indicating the type of effect; \code{effect_function}
#'    indicating the function from which effect are sampled or the string
#'    "single_value" if single value was provided; \code{effect_summary} summary
#'    of the sampled effects; \code{effect_samples} vector with the sampled
#'    effects (or unique value in the case of single value).}
#'    \item{test_info}{a list with all the information regarding the test
#'    performed. The list includes: \code{test_method} character sting
#'    indicating the test method (e.g., "pearson", "one-sample", "paired",
#'    "two-samples", or "welch"); the required sample size (\code{sample_n1} and
#'    if relevant \code{sample_n2}), alternative hypothesis
#'    (\code{alternative}), significance level (\code{sig_level})  and  degrees
#'    of freedom (\code{df}) of the statistical test; \code{critical_effect} the
#'    minimum absolute effect value that would result significant. Note that
#'    \code{critical_effect} in the case of \code{alternative = "two.sided"} is
#'    the absolute value and both positive and negative values should be
#'    considered.}
#'    \item{prospective_res}{a data frame with the resulting inferential
#'    errors. Columns names are \code{power}, \code{typeM}, and \code{typeS}.}
#'
#' @details Conduct a prospective design analysis to define the required sample
#'   size and the associated inferential risks according to study design. A
#'   general overview is provided in the \code{vignette(todo)}.
#'
#'   \strong{Population effect size}
#'
#'   The hypothetical population effect size (\code{effect_size}) can be set to
#'   a single value or a function that allows to sample values from a given
#'   distribution. The function has to be defined as \code{function(x)
#'   my_function(x, ...)}, with only one single variable \code{x} that represent
#'   the number of samples (e.g., \code{function(x) rnorm(x, mean = 0, sd = 1)};
#'   \code{function(x) sample(c(.1,.3,.5), x, replace = TRUE)}). This allows
#'   users to define hypothetical effect size distribution according to their
#'   needs.
#'
#'   Argument \code{B_effect} allows to define the number of sampled effect
#'   size. Users can access sampled effects in the \code{effect_info} list
#'   included in the output to evaluate if sample is representative of their
#'   specification. Increase the number to obtain more accurate results but it
#'   will require more computational time (default is 250).
#'
#'   Optional arguments \code{tl} and \code{tu} allow to truncate the sampling
#'   distribution specifying the lower truncation point and upper truncation
#'   point respectively. Note that if \code{effect_type = "correlation"},
#'   distribution is automatically truncated between -1 and 1.
#'
#'   \strong{Effect type options}
#'
#'   The \code{effect_type} argument can be set to \code{"cohen_d"} (default)
#'   for standardized mean difference or \code{"correlation"} if Pearson's
#'   correlation is evaluated.
#'
#'   In the case of \code{"cohen_d"} one-sample or two-samples \emph{t}-test are
#'   considered following same options specification of basic function
#'   \code{t.test()}, note that default options of \code{t.test()} are
#'   \code{paired = FALSE} and \code{var.equal = FALSE}. For one-sample
#'   \emph{t}-test \code{ratio_n = NULL} is required. For paired \emph{t}-test
#'   \code{ratio_n = 1} and option \code{paired = TRUE} are required. For
#'   two-samples \emph{t}-test \code{ratio_n} can be specified according to user
#'   needs and option \code{var.equal = TRUE} is required. For Welch
#'   \emph{t}-test, \code{ratio_n} can be specified according to user needs
#'   (default option is \code{var.equal = FALSE}).
#'
#'   In the case of \code{"correlation"}, only Pearson's correlation between two
#'   variables is available and \code{ratio_n} is set to 1 (default). The
#'   Kendall's \emph{tau} or Spearman's \emph{rho} are not implemented.
#'
#'   \strong{Study design}
#'
#'   Study design can be further defined according to statistical test
#'   directionality and required \eqn{\alpha}-level using the arguments
#'   \code{alternative} and \code{sig_level} respectively.
#'
#' @examples
#'
#' # One-sample t-test
#' prospective(effect_size = .3, power = .8, ratio_n = NULL,
#'             effect_type = "cohen_d", seed = 2020, B = 1e3)
#' # Paired t-test
#' prospective(effect_size = .3, power = .8, ratio_n = 1,
#'             effect_type = "cohen_d", paired = TRUE, seed = 2020, B = 1e3)
#' # Two-samples t-test
#' prospective(effect_size = .3, power = .8, ratio_n = 1.5,
#'             effect_type ="cohen_d", var.equal = TRUE, seed = 2020, B = 1e3)
#' # Welch t-test
#' prospective(effect_size = .3, power = .8, ratio_n = 2,
#'             effect_type ="cohen_d", seed = 2020, B = 1e3)
#'
#' # Pearson's correlation
#' prospective(effect_size = .3, power = .8, effect_type = "correlation",
#'             seed = 2020, B = 1e3)
#'
#' \dontrun{
#' # Define effect_size using functions (long computational time)
#' prospective(effect_size = function(x) rnorm(x, .3, .1), power = .8,
#'               effect_type = "correlation", seed = 2020)
#' prospective(effect_size = function(x) rnorm(x, .3, .1), power = .8,
#'               effect_type = "cohen_d", tl = .2, tu = .4, seed = 2020)
#' }
#'
#'@references Altoè, G., Bertoldo, G., Zandonella Callegher, C., Toffalini, E.,
#'  Calcagnì, A., Finos, L., & Pastore, M. (2020). Enhancing Statistical
#'  Inference in Psychological Research via Prospective and Retrospective Design
#'  Analysis. Frontiers in Psychology, 10.
#'  https://doi.org/10.3389/fpsyg.2019.02893
#'
#'  Gelman, A., & Carlin, J. (2014). Beyond Power Calculations: Assessing Type S
#'  (Sign) and Type M (Magnitude) Errors. Perspectives on Psychological Science,
#'  9(6), 641–651. https://doi.org/10.1177/1745691614551642
#'
#'  Bertoldo, G., Altoè, G., & Zandonella Callegher, C. (2020, June 15).
#'  Designing Studies and Evaluating Research Results: Type M and Type S Errors
#'  for Pearson Correlation Coefficient. Retrieved from https://psyarxiv.com/q9f86/
#'
#' @export
#'
#' @importFrom stats median
#'
prospective <- function(effect_size,
                        power,
                        ratio_n2 = 1,
                        effect_type = c("cohen_d","correlation"),
                        alternative = c("two.sided","less","greater"),
                        sig_level = .05,
                        B = 1e4,
                        seed = NULL,
                        tl = -Inf,
                        tu = Inf,
                        B_effect = 250,
                        sample_range = c(2, 1000),
                        tol = .01,
                        display_message = FALSE,
                        ...){



  #----    Save call    ----

  # Match arguments
  effect_type <- match.arg(effect_type)
  alternative <- match.arg(alternative)

  # Save call
  design_analysis = "prospective"
  call_arguments = as.list(match_call(default = TRUE))[-1]

  # eval possible errors
  do.call(eval_arguments_prospective,
          call_arguments)

  # Define conf.level according to sig_level
  call_arguments$conf.level <- define_conf_level(call_arguments)

  #----    Set seed    ----

  # Set seed
  if(!is.null(seed)){
    old_seed <- .Random.seed
    on.exit( { .Random.seed <<- old_seed })
    set.seed(seed = seed)
  }

  #----    Evaluate effect size    ----

  effect_info <- eval_effect_size(effect_type = effect_type,
                                  effect_size = effect_size,
                                  tl = tl,
                                  tu = tu,
                                  B_effect = B_effect)
  effect_target = effect_info$effect_summary[["Mean"]]

  #----    Evaluate samples    ----

  if(effect_type == "correlation" && ratio_n2 != 1){
    call_arguments["ratio_n2"] <- list(1)
    warning("If effect_type is set to 'correlation', ratio_n2 is set to 1")
  }

  sample_info <- do.call(eval_samples,
                         c(call_arguments,
                           current_n = sample_range[2]))

  #----    Get test method    ----

  # Evaluate test test_method
  test_method <- do.call(eval_test_method, c(call_arguments,
                                             sample_n1 = sample_info$sample_n1,
                                             sample_n2 = sample_info$sample_n2,
                                             effect_target = effect_target))

  #----    Prospective ananlysis    ----

  # Check with maximum N
  prospective_res <- do.call(simulate_analysis,
                             c(call_arguments,
                               effect_info["effect_samples"],
                               test_method = test_method,
                               sample_info))

  est_power <- mean(prospective_res$power)

  if ( est_power < power ) {
    stop(paste0("Actual power = ", est_power, " with n = ", sample_range[2],"\n",
                "  try to increase maximum of sample_range > ", sample_range[2],"."))
     } else {

       # Loop prospective
        find_power <- FALSE
        n_seq <- seq( sample_range[1], sample_range[2], by = 1 )
        n_target <- round(median(n_seq))

        while( (!find_power) ) {
          sample_info <- do.call(eval_samples,
                                 c(call_arguments,
                                   current_n = n_target))

          prospective_res <- do.call(simulate_analysis,
                                     c(call_arguments,
                                       effect_info["effect_samples"],
                                       test_method = test_method,
                                       sample_info))

          est_power <- mean(prospective_res$power)

          if (display_message == TRUE){
            cat("Evaluate n =", n_target, fill=TRUE)
            cat("Estimated power is", round(est_power,2), fill=TRUE)
            cat("\n")
          }

          # Evaluate if power was obtained according to tolerance value
          if ( (est_power<=(power+tol)) && (est_power>=(power-tol)) ) {
            find_power <- TRUE
          } else {
            if (length(n_seq)==1) {
              message("Required power according to tolerance value can not be obtained.\nIncrease tolerance value.")
              find_power <- TRUE
            } else if (est_power > (power+tol)) {
              (n_seq <- seq( min(n_seq), n_target-1, by = 1))
              (n_target <- round(median(n_seq)))
            } else {
              (n_seq <- seq(n_target+1, max(n_seq), by = 1))
              (n_target <- round(median(n_seq)))
            }
          }
        }
     }

  #----    Get test_info    ----
  #Compute df and critical value
  crit_values <- do.call(compute_critical_effect,
                         c(call_arguments,
                           sample_n1 = sample_info$sample_n1,
                           sample_n2 = sample_info$sample_n2,
                           test_method = test_method))

  test_info <- c(test_method = test_method,
                 sample_info,
                 alternative = alternative,
                 sig_level = sig_level,
                 crit_values)


  #----    save results    ----
  design_fit <- structure(list(design_analysis = design_analysis,
                               call_arguments = call_arguments,
                               effect_info = c(effect_type = effect_type, effect_info),
                               test_info = test_info,
                               prospective_res = prospective_res),
                          class = c("design_analysis","list"))



  return(design_fit)
}


#-----

