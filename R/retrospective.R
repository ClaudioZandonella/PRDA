#############################
####    Retrospective    ####
#############################

#----    Retrospective    ----

#' Retrospective Design Analysis
#'
#' Given the hypothetical population effect size and study sample size, the
#' function \code{retrospective()} performs a retrospective design analysis for
#' Cohen's \emph{d} (\emph{t}-test comparing group means) or  Pearson's
#' correlation test between two variables. According to the defined alternative
#' hypothesis and significance level, inferential errors (i.e., Power level,
#' Type-M error, and Type-S error) are computed together with the the critical
#' effect value (i.e., the minimum absolute effect size value that would result
#' significant).
#'
#'@param effect_size a numeric value or function (see details) indicating the
#'  hypothetical population effect size.
#'@param sample_n1 a numeric value indicating the sample size of the first
#'  group.
#'@param sample_n2 an optional numeric value indicating the sample size of the
#'  second group.
#'@param effect_type a character string specifying the effect type, must be
#'  "cohen_d" (default, Cohen's \emph{d} standardised means difference) or
#'  "pearson" (Pearson's correlation). You can specify just the initial letter.
#'@param alternative a character string specifying the alternative hypothesis,
#'  must be one of "two.sided" (default), "greater" or "less". You can specify
#'  just the initial letter.
#'@param sig_level a numeric value indicating the significance level on which
#'  the alternative hypothesis is evaluated.
#'@param B a numeric  value indicating the number of iterations. Increase the
#'  number of iterations to obtain more stable results.
#'@param seed a numeric value indicating the seed for random number generation.
#'  Set the seed to obtain results reproducible.
#'@param tl optional value indicating the lower truncation point if
#'  \code{effect_size} is defined as a function.
#'@param tu optional value indicating the upper truncation point if
#'  \code{effect_size} is defined as a function.
#'@param B_effect a numeric  value indicating the number of sampled effect size
#'  if \code{effect_size} is defined as a function. Increase the number to
#'  obtain more stable results.
#'@param ... further arguments to be passed to or from methods.
#'
#'@return A list with class "design_analysis" containing the following
#'  components:
#'    \item{design_analysis}{a character string indicating the type of design
#'    analysis: "retrospective".}
#'    \item{call_arguments}{a list with all the arguments passed to the
#'    function.}
#'    \item{effect_info}{a list with all the information regarding the
#'    considered hypothetical population effect size. The list includes:
#'    \code{effect_function} indicating the function from which effect are
#'    sampled or the string "single_value" if single value was provided;
#'    \code{effect_summary} summary of the sampled effects;
#'    \code{effect_samples} vector with the sampled effects (or unique value in
#'    the case of single value).}
#'    \item{test_info}{a list with all the information regarding the test
#'    performed. The list includes: \code{test_method} character sting
#'    indicating the test method (e.g., "pearson", "one-sample", "paired",
#'    "two-samples", or "welch"); sample size (\code{sample_n1} and if relevant
#'    \code{sample_n2}), alternative hypothesis (\code{alternative}}),
#'    significance level (\code{sig_level})  and  degrees of freedom (\code{df})
#'    of the statistical test; \code{critical_effect} the minimum absolute
#'    effect value that would result significant. Note that
#'    \code{critical_effect} in the case of \code{alternative = "two.sided"} is
#'    the absolute value and both positive and negative values should be
#'    considered.}
#'    \item{retrospective_res}{a data frame with the resulting inferential
#'    errors. Columns names are \code{power}, \code{typeM}, and \code{typeS}.}
#'
#'@details Conduct a retrospective design analysis to evaluate inferential risks
#'  according to study design. A general overview is provided in the
#'  \code{vignette(todo)}.
#'
#'  \strong{Population effect size}
#'
#'  The hypothetical population effect size (\code{effect_size}) can be set to a
#'  single value or a function that allows to sample values from a given
#'  distribution. The function has to be defined as \code{function(x)
#'  my_function(x, ...)}, with only one single variable \code{x} that represent
#'  the number of samples (e.g., \code{function(x) rnorm(x, mean = 0, sd = 1)};
#'  \code{function(x) sample(c(.1,.3,.5), x, replace = TRUE)}). This allows
#'  users to define hypothetical effect size distribution according to their
#'  needs.
#'
#'  Argument \code{B_effect} allows to define the number of sampled effect size.
#'  Users can access sampled effects in the \code{effect_info} list included in
#'  the output to evaluate if sample is representative of their specification.
#'  Increase the number to obtain more accurate results but it will require more
#'  computational time (default is 250).
#'
#'  Optional arguments \code{tl} and \code{tu} allow to truncate the sampling
#'  distribution specifying the lower truncation point and upper  truncation
#'  point respectively. Note that if \code{effect_type = "correlation"},
#'  distribution is automatically truncated between -1 and 1.
#'
#'  \strong{Effect type options}
#'
#'  The \code{effect_type} argument can be set to \code{"cohen_d"} (default) for
#'  standardized mean difference or \code{"correlation"} if Pearson's
#'  correlation is evaluated.
#'
#'  In the case of \code{"cohen_d"} one-sample or two-samples \emph{t}-test are
#'  considered following same options specification of basic function
#'  \code{t.test()}, note that default options of \code{t.test()} are \code{paired
#'  = FALSE} and \code{var.equal = FALSE}. For one-sample \emph{t}-test only
#'  \code{sample_n1} is specified and \code{sample_n2 = NULL} is required. For
#'  paired \emph{t}-test \code{sample_n1} and \code{sample_n2} needs to be
#'  identical and option \code{paired = TRUE} is required. For two-samples
#'  \emph{t}-test \code{sample_n1} and \code{sample_n2} can differ and option
#'  \code{var.equal = TRUE} is required. For Welch \emph{t}-test, only
#'  \code{sample_n1} and \code{sample_n2} are required (default option is
#'  \code{var.equal = FALSE}).
#'
#'  In the case of \code{"correlation"}, only Pearson's correlation between two
#'  variables is available and \code{sample_n2} argument is ignored. The
#'  Kendall's \emph{tau} or Spearman's \emph{rho} are not implemented.
#'
#'  \strong{Study design}
#'
#'  Study design can be further defined according to statistical test
#'  directionality and required \eqn{\alpha}-level using the arguments
#'  \code{alternative} and \code{sig_level} respectively.
#'
#'
#'
#'
#' @examples
#'
#' # One-sample t-test
#' retrospective(effect_size = .3, sample_n1 = 25, sample_n2 = NULL,
#'               effect_type = "cohen_d", seed = 2020)
#' # Paired t-test
#' retrospective(effect_size = .3, sample_n1 = 25, sample_n2 = 25,
#'               effect_type = "cohen_d", paired = TRUE, seed = 2020)
#' # Two-samples t-test
#' retrospective(effect_size = .3, sample_n1 = 25, sample_n2 = 35,
#'               effect_type ="cohen_d", var.equal = TRUE, seed = 2020)
#' # Welch t-test
#' retrospective(effect_size = .3, sample_n1 = 25, sample_n2 = 35,
#'               effect_type ="cohen_d", seed = 2020)
#'
#' # Pearson's correlation
#' retrospective(effect_size = .3, sample_n1 = 25, effect_type = "correlation",
#'               seed = 2020)
#'
#' \dontrun{
#' # Define effect_size using functions (long computational time)
#' retrospective(effect_size = function(x) rnorm(x, .3, .1), sample_n1 = 25,
#'               effect_type = "correlation", seed = 2020)
#' retrospective(effect_size = function(x) rnorm(x, .3, .1), sample_n1 = 25,
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
#'@export

retrospective <- function(effect_size,
                          sample_n1,
                          sample_n2 = NULL,
                          effect_type = c("cohen_d","correlation"),
                          alternative = c("two.sided","less","greater"),
                          sig_level = .05,
                          B = 1e4,
                          seed = NULL,
                          tl = -Inf,
                          tu = Inf,
                          B_effect = 250,
                          ...){



  #----    Save call    ----

  # Match arguments
  effect_type <- match.arg(effect_type)
  alternative <- match.arg(alternative)

  # Save call
  design_analysis = "retrospective"
  call_arguments = as.list(match_call(default = TRUE))[-1]

  # eval possible errors
  do.call(eval_arguments_retrospective,
          call_arguments)

  # Define conf.level according to sig_level
  call_arguments$conf.level <- define_conf_level(call_arguments)

  # Check sample_n2 for correlation
  if(effect_type == "correlation"){
    if(!is.null(sample_n2)){
      call_arguments["sample_n2"] <- list(NULL)
      warning("If effect_type is set to 'correlation', sample_n2 is ignored.")
    }
    sample_n2 <- sample_n1
  }

  # Check for mu argument
  if("mu" %in% names(call_arguments) && mu != 0){
    stop("Desing Analysis is allowed only if the Null Hypothesis is 0")
  }
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

  #----    Get test info    ----

  # Evaluate test test_method
  test_method <- do.call(eval_test_method, c(call_arguments,
                                             effect_target = effect_target))
  #Compute df and critical value
  crit_values <- do.call(compute_critical_effect,
                         c(call_arguments,
                           test_method = test_method))

  test_info <- c(test_method = test_method,
                 sample_n1 = sample_n1,
                 sample_n2 = list(sample_n2),
                 alternative = alternative,
                 sig_level = sig_level,
                 crit_values)

  #----    Retrospective analysis    ----

  retrospective_res <- do.call(simulate_analysis,
                               c(call_arguments,
                                 effect_info["effect_samples"],
                                 test_method = test_method))

  #----    save results    ----
  design_fit <- structure(list(design_analysis = design_analysis,
                               call_arguments = call_arguments,
                               effect_info = effect_info,
                               test_info = test_info,
                               retrospective_res = retrospective_res),
                          class = c("design_analysis","list"))

  return(design_fit)

}


#-----

