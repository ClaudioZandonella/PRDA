###########################
####    Prospective    ####
###########################

#----    Prospective    ----


#' Prospective Design Analysis
#'
#' Given the hypothetical population effect size and the required power level,
#' the function \code{prospective()} performs a prospective design analysis for
#' Pearson's correlation test between two variables or \emph{t}-test comparing
#' group means (Cohen's \emph{d}). According to the defined alternative
#' hypothesis and the significance level, the required sample size is computed
#' together with the associated Type M error, Type S error, and the critical
#' correlation value (i.e., the minimum absolute effect size value that would
#' result significant).
#'
#'@param effect_size a numeric value or function (see details) indicating the
#'  hypothetical population effect size.
#'@param power a numeric value indicating the required power level.
#'@param ratio_n a numeric value indicating the ratio between \code{sample_n1}
#'   and \code{sample_n2}.
#'@param effect_type a character string specifying the effect type, must be one
#'  of \code{"correlation"} (default, Pearson's correlation) or \code{"cohen_d"}
#'  (Cohen's \emph{d} standardized mean difference) or ". You can specify just
#'  the initial letters.
#'@param test_method a character string specifying the test type, must be one of
#'  \code{"pearson"} (default, Pearson's correlation), \code{"two_sample"}
#'  (independent two-sample \emph{t}-test), \code{"welch"} (Welch's
#'  \emph{t}-test), \code{"paired"} (dependent \emph{t}-test for paired
#'  samples), or \code{"one_sample"} (one-sample \emph{t}-test). You can specify
#'  just the initial letters.
#'@param alternative a character string specifying the alternative hypothesis,
#'  must be one of "two_sided" (default), "greater" or "less". You can specify
#'  just the initial letter.
#'@param sig_level a numeric value indicating the significance level on which
#'  the alternative hypothesis is evaluated.
#'@param ratio_sd a numeric value indicating the ratio between the standard
#'  deviation in the first group and in the second group. This argument is
#'  required only in the case of Welch's \emph{t}-test.
#'@param B a numeric  value indicating the number of iterations. Increase the
#'  number of iterations to obtain more stable results.
#'@param tl optional value indicating the lower truncation point if
#'  \code{effect_size} is defined as a function.
#'@param tu optional value indicating the upper truncation point if
#'  \code{effect_size} is defined as a function.
#'@param B_effect a numeric  value indicating the number of sampled effects
#'  if \code{effect_size} is defined as a function. Increase the number to
#'  obtain more stable results.
#'@param sample_range a length-2 numeric vector indicating the minimum and
#'   maximum sample size of the first group (\code{sample_n1}).
#'@param eval_power a character string specifying the function used to summarize
#'  the resulting distribution of power values. Must be one of "median"
#'  (default) or "mean". You can specify just the initial letters. See details.
#'@param tol a numeric value indicating the tolerance of required power level.
#'@param display_message a logical variable indicating whether to display or
#'   not the information about computational steps.
#'@param seed a numeric value indicating the seed for random number generation.
#'  Set the seed to obtain reproducible results.
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
#'    "single_value" if a single value was provided; \code{effect_summary}
#'    summary of the sampled effects; \code{effect_samples} vector with the
#'    sampled effects (or unique value in the case of a single value); if
#'    relevant \code{tl} and \code{tu} specifying the lower upper truncation
#'    point respectively.}
#'    \item{test_info}{a list with all the information regarding the test
#'    performed. The list includes: \code{test_method} character sting
#'    indicating the test method (i.e., "pearson", "one_sample", "paired",
#'    "two_sample", or "welch"); the required sample size (\code{sample_n1} and
#'    if relevant \code{sample_n2}), the alternative hypothesis
#'    (\code{alternative}), significance level (\code{sig_level})  and  degrees
#'    of freedom (\code{df}) of the statistical test; \code{critical_effect} the
#'    minimum absolute effect value that would result significant. Note that
#'    \code{critical_effect} in the case of \code{alternative = "two_sided"} is
#'    the absolute value and both positive and negative values should be
#'    considered.}
#'    \item{prospective_res}{a data frame with the results of the design
#'    analysis. Columns names are \code{power}, \code{typeM}, and \code{typeS}.}
#'
#' @details Conduct a prospective design analysis to define the required sample
#'   size and the associated inferential risks according to study design. A
#'   general overview is provided in the \code{vignette("prospective")}.
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
#'   Argument \code{B_effect} allows defining the number of sampled effects.
#'   Users can access sampled effects in the \code{effect_info} list included in
#'   the output to evaluate if the sample is representative of their
#'   specification. Increase the number to obtain more accurate results but it
#'   will require more computational time (default is 1000). To avoid long
#'   computational times, we suggest adjusting \code{B} when using a function to
#'   define the hypothetical population effect size.
#'
#'   Optional arguments \code{tl} and \code{tu} allow truncating the sampling
#'   distribution specifying the lower truncation point and upper truncation
#'   point respectively. Note that if \code{effect_type = "correlation"},
#'   distribution is automatically truncated between -1 and 1.
#'
#'   When a distribution of effects is specified, a corresponding distribution
#'   of power values is obtained as result. To evaluate whether the required
#'   level of power is obtained, user can decide between the median or the mean
#'   value as a summary of the distribution using the argument
#'   \code{eval_power}. They answer two different questions. Which is the
#'   required sample size to obtain 50% of the time a power equal or greater
#'   than the required level (median)?; Which is the required sample size to
#'   obtain on average a power equal or greater than the required level (mean)?.
#'
#'   \strong{Effect type and test method options}
#'
#'   The \code{effect_type} argument can be set to \code{"correlation"}
#'   (default) if a correlation is evaluated or \code{"cohen_d"} for
#'   standardized mean difference.
#'
#'   In the case of \code{"correlation"}, only Pearson's correlation between two
#'   variables is available. In this case \code{"pearson"} has to be set as
#'   \code{test_method} and \code{ratio_n} argument is ignored. The Kendall's
#'   \emph{tau} and Spearman's \emph{rho} are not implemented.
#'
#'   In the case of \code{"cohen_d"}, the available \emph{t}-tests can be
#'   selected specifying the argument \code{test_method}. For independent
#'   two-sample \emph{t}-test, use \code{"two_sample"} and indicate the ratio
#'   between the sample size of the first group and the second group
#'   (\code{ratio_n}). For Welch's \emph{t}-test, use \code{"welch"} and
#'   indicate the ratio between the sample size of the first group and the
#'   second group (\code{ratio_n}) and the ratio between the standard deviation
#'   in the first group and in the second group (\code{ratio_sd}). For dependent
#'   \emph{t}-test for paired samples, use \code{"paired"} (\code{ratio_n} has
#'   to be 1). For one-sample \emph{t}-test, use \code{"one_sample"}
#'   (\code{ratio_n} has to be \code{NULL}).
#'
#'   \strong{Study design}
#'
#'   Study design can be further defined according to statistical test
#'   directionality and required \eqn{\alpha}-level using the arguments
#'   \code{alternative} and \code{sig_level} respectively.
#'
#' @examples
#'
#' # Pearson's correlation
#' prospective(effect_size = .3, power = .8, effect_type = "correlation",
#'             test_method = "pearson", B = 1e3, seed = 2020)
#'
#' # Two-sample t-test
#' prospective(effect_size = .3, power = .8, ratio_n = 1.5,
#'             effect_type = "cohen_d", test_method = "two_sample",
#'             B = 1e3, seed = 2020)
#' # Welch t-test
#' prospective(effect_size = .3, power = .8, ratio_n = 2,
#'             effect_type ="cohen_d", test_method = "welch",
#'             ratio_sd = 1.5, B = 1e3, seed = 2020)
#' # Paired t-test
#' prospective(effect_size = .3, power = .8, ratio_n = 1,
#'             effect_type = "cohen_d", test_method = "paired",
#'             B = 1e3, seed = 2020)
#' # One-sample t-test
#' prospective(effect_size = .3, power = .8, ratio_n = NULL,
#'             effect_type = "cohen_d", test_method = "one_sample",
#'             B = 1e3, seed = 2020)
#'
#'
#'
#' \dontrun{
#' # Define effect_size using functions (long computational time)
#' prospective(effect_size = function(x) rnorm(x, .3, .1), power = .8,
#'             effect_type = "correlation", test_method = "pearson",
#'             B_effect = 500, B = 500, tl = .15, seed = 2020)
#' prospective(effect_size = function(x) rnorm(x, .3, .1), power = .8,
#'             effect_type = "cohen_d", test_method = "two_sample", ratio_n = 1,
#'             B_effect = 500, B = 500, tl = .2, tu = .4, seed = 2020)
#' }
#'
#'@references Altoè, G., Bertoldo, G., Zandonella Callegher, C., Toffalini, E.,
#'  Calcagnì, A., Finos, L., & Pastore, M. (2020). Enhancing Statistical
#'  Inference in Psychological Research via Prospective and Retrospective Design
#'  Analysis. Frontiers in Psychology, 10.
#'  \url{https://doi.org/10.3389/fpsyg.2019.02893}
#'
#'  Bertoldo, G., Altoè, G., & Zandonella Callegher, C. (2020, June 15).
#'  Designing Studies and Evaluating Research Results: Type M and Type S Errors
#'  for Pearson Correlation Coefficient. Retrieved from
#'  \url{https://psyarxiv.com/q9f86/}
#'
#'  Gelman, A., & Carlin, J. (2014). Beyond Power Calculations: Assessing Type S
#'  (Sign) and Type M (Magnitude) Errors. Perspectives on Psychological Science,
#'  9(6), 641–651. \url{https://doi.org/10.1177/1745691614551642}
#'
#'
#' @export
#'
prospective <- function(effect_size,
                        power,
                        ratio_n = 1,
                        effect_type = c("correlation", "cohen_d"),
                        test_method = c("pearson", "two_sample", "welch",
                                        "paired", "one_sample"),
                        alternative = c("two_sided","less","greater"),
                        sig_level = .05,
                        ratio_sd = 1,
                        B = 1e4,
                        tl = -Inf,
                        tu = Inf,
                        B_effect = 1e3,
                        sample_range = c(2, 1000),
                        eval_power = c("median", "mean"),
                        tol = .01,
                        display_message = TRUE,
                        seed = NULL){



  #----    Save call    ----

  # Match arguments
  effect_type = match.arg(effect_type)
  alternative = match.arg(alternative)
  test_method = match.arg(test_method)
  eval_power = match.arg(eval_power)

  # Save call
  design_analysis = "prospective"
  call_arguments = as.list(match_call(default = TRUE))[-1]

  # eval possible errors
  do.call(eval_arguments_prospective,
          call_arguments)


  #----    Set seed    ----

  # Set seed
  if(!is.null(seed)){
    if(!exists(".Random.seed")) rnorm(1)
    old_seed = .Random.seed
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

  if(effect_type == "correlation" && (is.null(ratio_n) || ratio_n != 1)){
    call_arguments["ratio_n"] = list(1)
    ratio_n = 1
    warning("If 'effect_type = correlation', argument 'ratio_n' is set to 1")
  }

  sample_info = eval_samples(ratio_n = ratio_n, current_n = sample_range[2])

  #----    Get test method    ----

  # Evaluate test test_method
  # (use t.test or cor.tes() to evaluate possible errors)
  do.call(eval_test_method, c(call_arguments,
                              sample_n1 = sample_info$sample_n1,
                              sample_n2 = sample_info$sample_n2,
                              effect_target = effect_target))

  #----    Prospective ananlysis    ----

  # Loop prospective
  find_power = FALSE
  n_seq = seq( sample_range[1], sample_range[2], by = 1 )
  n_target = round(median(n_seq))

  while( (!find_power) ) {
    sample_info = eval_samples(ratio_n = ratio_n, current_n = n_target)

    prospective_res = do.call(simulate_analysis,
                              c(call_arguments,
                                effect_info["effect_samples"],
                                sample_info))

    est_power = do.call(eval_power, list(prospective_res$power))

    if (display_message == TRUE){
      cat("Evaluate n =", n_target, fill=TRUE)
      cat("Estimated power is", round(est_power,2), fill=TRUE)
      cat("\n")
    }

    # Evaluate if power was obtained according to tolerance value
    if ( (est_power <= (power+tol)) && (est_power >= (power-tol)) ) {
      find_power = TRUE
    } else {
      if (isTRUE(all.equal(n_target, sample_range[2])) && est_power<=(power+tol)) {
        stop(paste0("Actual power = ", est_power, " with n = ", sample_range[2],"\n",
                    "  try to increase maximum of sample_range > ", sample_range[2],"."))
      } else if (length(n_seq)==1) {
        message("Required power according to tolerance value can not be obtained.\nIncrease tolerance value.")
        find_power = TRUE
      } else if (est_power > (power+tol)) {
        n_seq = seq( min(n_seq), n_target-1, by = 1)
        n_target = round(median(n_seq))
      } else {
        n_seq = seq(n_target+1, max(n_seq), by = 1)
        n_target = round(median(n_seq))
      }
    }
  }


  #----    Get test_info    ----

  #Compute df and critical value
  crit_values = do.call(compute_critical_effect,
                        c(call_arguments,
                          sample_n1 = sample_info$sample_n1,
                          sample_n2 = sample_info$sample_n2))

  test_info = c(test_method = test_method,
                sample_info,
                alternative = alternative,
                sig_level = sig_level,
                crit_values)


  #----    save results    ----
  design_fit = structure(list(design_analysis = design_analysis,
                              call_arguments = call_arguments,
                              effect_info = c(effect_type = effect_type, effect_info),
                              test_info = test_info,
                              prospective_res = prospective_res),
                         class = c("design_analysis","list"))



  return(design_fit)
}


#-----

