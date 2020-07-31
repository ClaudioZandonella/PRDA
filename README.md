# PRDAbeta
<!-- badges: start -->
[![Travis build status](https://travis-ci.org/ClaudioZandonella/PRDA_beta.svg?branch=develop)](https://travis-ci.org/ClaudioZandonella/PRDA_beta)
<!-- badges: end -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3630733.svg)](https://doi.org/10.5281/zenodo.3630733)
<hr>

### Prospective and Retrospective Design Analysis

Performs a prospective or retrospective design analysis given a plausible value of effect size to evaluate inferential risks (i.e., power, Type M error, and Type S error) related to the study design.

`PRDAbeta` package can be used for Personson's correlation between two variables or mean comparisons  (one-sample, paired, two-samples, and Welch's *t*-test) considering a plausible value of $\rho$ or Cohen's *d* respectively. 


### Set up

To install this github version type in R:

```{r}
# if devtools is not installed yet: 
# install.packages( "devtools" )  
library( devtools )
install_github( "CaludioZandonella/PRDAbeta", build_vignettes = TRUE)
```

### Functions

#### `rerospective()`

Given the hypothetical population effect size and study sample size, the function `retrospective()` performs a retrospective design analysis for Cohen's d (t-test comparing group means) or Pearson's correlation test between two variables. According to the defined alternative hypothesis and significance level, inferential errors (i.e., Power level, Type-M error, and Type-S error) are computed together with the the critical effect value (i.e., the minimum absolute effect size value that would result significant).

To know more about function arguments and examples `?rerospective()`.

#### `prospective()`

Given the hypothetical population effect size and the required power level, the function `prospective()` performs a prospective design analysis for Cohen's *d* (*t*-test comparing group means) or Pearson's correlation test between two variables. According to the defined alternative hypothesis and significance level, the required sample size is computed together with the associated Type-M error, Type-S error, and the the critical correlation value (i.e., the minimum absolute effect size value that would result significant).

To know more about function arguments and examples `?prospective()`.

###  Hypothetical effect size

The hypothetical population effect size (argument `effect_size`) can be set to a single value or a function that allows to sample values from a given distribution. The function has to be of the type `function(x) my_function(x, ...)`, with only one single variable x that represent the number of samples (e.g., `function(x) rnorm(x, mean = 0, sd = 1)`; `function(x) sample(c(.1,.3,.5), x, replace = TRUE))`. This allows users to define hypothetical effect size distribution according to their needs.

Optional arguments `tl` and `tu` allow to truncate the sampling distribution specifying the lower truncation point and upper truncation point respectively.


### References

Altoè, G., Bertoldo, G., Zandonella Callegher, C., Toffalini E., Calcagnì, A., Finos, L., Pastore, M. (2019).
Enhancing statistical inference in psychological research via prospective and retrospective design analysis. URL: https://arxiv.org/abs/1909.13773. 

Bertoldo, G., Altoè, G., & Zandonella Callegher, C. (2020, June 15). Designing Studies and Evaluating Research Results: Type M and Type S Errors for Pearson Correlation Coefficient. Retrieved from https://psyarxiv.com/q9f86/
