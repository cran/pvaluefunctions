---
title: "*P*-value functions: Tutorial using the `pvaluefunctions` package"
author: "Denis Infanger"
date: "2020-12-08"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{P-value functions}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Accompanying paper

We published an [accompanying paper](https://doi.org/10.1002/sim.8293) to illustrate the use of *p*-value functions:

Infanger D, Schmidt-Trucksäss A. (2019): *P* value functions: An underused method to present research results and to promote quantitative reasoning. *Statistics in Medicine,* **38:** 4189-4197. doi: 10.1002/sim.8293.

### Recreation of the graphics in the paper

The code and instructions to reproduce all graphics in our paper can be found in the following GitHub repository: https://github.com/DInfanger/pvalue_functions

## Overview

This vignette shows how to use the `pvaluefunctions` package wich contains an R function to create graphics of *p*-value functions, confidence distributions, confidence densities, or the [Surprisal value (S-value)](http://www.umsl.edu/~fraundorfp/egsurpri.html) (Greenland 2019).

## Installation

Install (`install.packages("pvaluefunctions")`) and load the package from CRAN.


```r
library(pvaluefunctions)
```

<!-- Download the file `confidence_distributions.R` to your computer. You can either `source()` the function in R or open it, select and run everything. After loading the function, it's ready for use. -->

<!-- To reproduce the plots from the publication, download the file `paper_plots.R` and run it *after* loading the main function contained in the file `confidence_distributions.R` (see above). -->

<!-- Alternatively, you can source the files directly from the GitHub repository using the [`devtools`](https://CRAN.R-project.org/package=devtools) package: -->



## Dependencies

The function depends on the following R packages:

* [ggplot2](https://cran.r-project.org/package=ggplot2)
* [scales](https://cran.r-project.org/package=scales)
* [zipfR](https://cran.r-project.org/package=zipfR)
* [pracma](https://cran.r-project.org/package=pracma)
* [gsl](https://cran.r-project.org/package=gsl)

## Usage

There is only one function needed to create the plots: `conf_dist()`. The function has the following arguments:

* `estimate`: Numerical vector containing the estimate(s).
* `n`: Numerical vector containing the sample size(s). Required for correlations, variances, proportions and differences between proportions. Must be equal the number of estimates.
* `df`: Numerical vector containing the degrees of freedom. Required for statistics based on the *t*-distribution (e.g. linear regression) and *t*-tests. Must be equal the number of estimates.
* `stderr`: Numerical vector containing the standard error(s) of the estimate(s). Required for statistics based on the *t*-distribution (e.g. linear regression) and the normal distribution (e.g. logistic regression). Must be equal the number of estimate(s).
* `tstat`: Numerical vector containing the *t*-statistic(s). Required for *t*-tests (means and mean differences). Must be equal the number of estimates. 
* `type`: String indicating the type of the estimate. Must be one of the following: `ttest`, `linreg`, `gammareg`, `general_t`, `logreg`, `poisreg`, `coxreg`, `general_z`, `pearson`, `spearman`, `kendall`, `var`, `prop`, `propdiff`.
* `plot_type`: String indicating the type of plot. Must be one of the following: `cdf` (confidence distribution), `pdf` (confidence density), `p_val` (*p*-value function, the default), `s_val` (Surprisal). For differences between independent proportions, only *p*-value functions and Surprisal value functions are available.
* `n_values` (optional): Integer indicating the number of points that are used to generate the graphics. The higher this number, the higher the computation time and resolution.
* `est_names` (optional): String vector indicating the names of the estimate(s). Must be equal the number of estimates.
* `conf_level` (optional): Numerical vector indicating the confidence level(s). Bust be between 0 and 1.
* `null_values` (optional): Numerical vector indicating the null value(s) in the plot on the *untransformed* scale. For example: If you want to plot odds ratios from logistic regressions, the `null_values` have to be given on the log-odds scale. If x limits are specified with `xlim`, all null values outside of the specified x limits are ignored for plotting and a message is printed.
* `trans` (optional): String indicating the transformation function that will be applied to the estimates and confidence curves. For example: `"exp"` for an exponential transformation of the log-odds in logistic regression. 
* `alternative`: String indicating if the confidence level(s) are two-sided or one-sided. Must be one of the following: `two_sided`, `one_sided`.
* `log_yaxis`: Logical. Indicating if a portion of the y-axis should be displayed on the logarithmic scale.
* `cut_logyaxis`: Numerical value indicating the threshold below which the y-axis will be displayed logarithmically. Must lie between 0 and 1.
* `xlim` (optional) Optional numerical vector of length 2 (x1, x2) indicating the limits of the x-axis on the *untransformed* scale. For example: If you want to plot *p*-value functions for odds ratios from logistic regressions, the limits have to be given on the log-odds scale. Note that x1 > x2 is allowed but then x2 will be the left limit and x1 the right limit (i.e. the limits are sorted before plotting). Null values (specified in `null_values`) that are outside of the specified limits are ignored and a message is printed.
* `together`: Logical. Indicating if graphics for multiple estimates should be displayed together or on separate plots.
* `plot_legend` Logical. Indicating if a legend should be plotted if multiple curves are plotted together with different colors (i.e. `together = TRUE` and `same_color = FALSE`).
* `same_color`: Logical. Indicating if curves should be distinguished using colors if they are plotted together (i.e. `together = TRUE`).
* `col` String indicating the colour of the curves. Only relevant for single curves, multiple curves not plotted together (i.e. `together = FALSE`) and multiple curves plotted together but with the option `same_color` set to `TRUE`.
* `nrow`: (optional) Integer greater than 0 indicating the number of rows when `together = FALSE` is specified for multiple estimates. Used in `facet_wrap` in ggplot2.
* `ncol`: (optional) Integer greater than 0 indicating the number of columns when `together = FALSE` is specified for multiple estimates. Used in `facet_wrap` in ggplot2.
* `plot_p_limit`: Numerical value indicating the lower limit of the y-axis. Must be greater than 0 for a logarithmic scale (i.e. `log_yaxis = TRUE`). The default is to omit plotting *p*-values smaller than 1 - 0.999 = 0.001.
* `plot_counternull`: Logical. Indicating if the counternull should be plotted as a point. Only available for \emph{p}-value functions and s-value functions. Counternull values that are outside of the plotted functions are not shown.
* `title` (optional): String containing a title for the plot.
* `xlab` (optional): String indicating the label of the x-axis.
* `ylab` (optional): String containing a title for the primary (left) y-axis.
* `ylab_sec` (optional): String containing a title for the secondary (right) y-axis.
* `inverted` Logical. Indicating the orientation of the *P*-value function (`p_val`), S-value function (`s_val`) and confidence distribution (`cdf`). By default (i.e. `inverted = FALSE`) small *P*-values are plotted at the bottom and large ones at the top. By setting `inverted = TRUE`, the y-axis is invertedd. Ignored for confidence densities.
* `x_scale` String indicating the scaling of the x-axis. The default is to scale the x-axis logarithmically if the transformation specified in `trans` is "exp" (exponential) and linearly otherwise. The option `linear` (can be abbreviated) forces a linear scaling and the option `logarithm` (can be abbreviated) forces a logarithmic scaling.
* `plot` Logical. Should a plot be created (`TRUE`, the default) or not (`FALSE`). `FALSE` can be useful if users want to create their own plots. If `FALSE`, no ggplot2 object is returned.

### Required arguments for different estimate types

* *t*-tests: `estimate`, `df`, `tstat`.
* Linear regression, Gamma regression, general estimates with inference based on the *t*-distribution: `estimate`, `df`, `stderr`.
* Logistic regression, Poisson regression, Cox regression, general estimates with inference based on the normal distribution: `estimate`, `stderr`.
* Correlation coefficients (Pearson, Spearman, Kendall), proportions, difference between proportions, variances: `estimate`, `n`.

### Returned values

The main function `conf_dist()` returns five objects in a list:

* `res_frame`: A data frame containing the values used to construct the plot. It contains the following variables: `values` contain the values of the effect (e.g. mean difference, odds ratio etc.), `conf_dist` the values for the confidence distribution, `conf_dens` the values for the confidence density, `p_two` the values for the two-sided *p*-value function, `p_one` the values for the one-sided *p*-value function, `s_val` the S-value (surprisal) of the two-sided *p*-values, `variable` the name of the estimate, `hypothesis` indicating the alternative hypothesis if one-sided *p*-value functions were specified and `counternull` containing the counternull values.
* `conf_frame`: A data frame containing the confidence intervals for the specified confidence levels for all estimates.
* `counternull_frame`: A data frame containing the counternull values for the specified null values (see Rosenthal & Rubin (1994) for more information about the counternull).
* `point_est`: A data frame containing the point estimates for all estimates. The point estimates correspond to the mean, median or mode of the confidence density (see Xie & Singh (2013) for more information). Estimates are produced using numerical procedures: Increase the number of points `n_values` for higher numerical precision.
* `aucc_frame`: A data frame containing the estimated AUCC (area under the confidence curves). The AUCC is a measures of precision with higher values indicating a poorer precision compared to smaller values (see Berrar (2017)).
* `plot`: A [ggplot2](https://ggplot2.tidyverse.org/) plot object (only returned if the option `plot = TRUE` was specified).

## Examples

### Two-sample *t*-test with unequal variances (Welch-Test)


```r
#-----------------------------------------------------------------------------
# T-Test
#-----------------------------------------------------------------------------

with(sleep, mean(extra[group == 1])) - with(sleep, mean(extra[group == 2]))
#> [1] -1.58
t.test(extra ~ group, data = sleep, var.equal = FALSE)
#> 
#> 	Welch Two Sample t-test
#> 
#> data:  extra by group
#> t = -1.8608, df = 17.776, p-value = 0.07939
#> alternative hypothesis: true difference in means between group 1 and group 2 is not equal to 0
#> 95 percent confidence interval:
#>  -3.3654832  0.2054832
#> sample estimates:
#> mean in group 1 mean in group 2 
#>            0.75            2.33

#-----------------------------------------------------------------------------
# Create p-value function
#-----------------------------------------------------------------------------

res <- conf_dist(
  estimate = c(-1.58)
  , df = c(17.77647)
  , tstat = c(-1.860813)
  , type = "ttest"
  , plot_type = "p_val"
  , n_values = 1e4L
  # , est_names = c("")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Mean difference (group 1 - group 2)"
  , together = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
  , x_scale = "line"
  , plot = TRUE
)
```

<img src="figure/ttest-1.png" title="plot of chunk ttest" alt="plot of chunk ttest" width="80%" style="display: block; margin: auto;" />

### Single coefficient from a linear regression model
#### *P*-value function

Because it's difficult to see very small *p*-values in the graph, you can set the option `log_yaxis = TRUE` so that *p*-values (i.e. the y-axes) below the value set in `cut_logyaxis` will be plotted on a logarithmic scale. This will make it much easier to see small *p*-values but has the disadvantage of creating a "kink" in the *p*-value function which is a pure artifact and puts an undue emphasis on the specified cutoff.



```r
#-----------------------------------------------------------------------------
# Model
#-----------------------------------------------------------------------------

mod <- lm(Infant.Mortality~Agriculture + Fertility + Examination, data = swiss)

summary(mod)
#> 
#> Call:
#> lm(formula = Infant.Mortality ~ Agriculture + Fertility + Examination, 
#>     data = swiss)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -8.5375 -1.4021 -0.0066  1.7381  5.9150 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)   
#> (Intercept) 11.01896    4.47291   2.463  0.01784 * 
#> Agriculture -0.02143    0.02394  -0.895  0.37569   
#> Fertility    0.13115    0.04145   3.164  0.00285 **
#> Examination  0.04913    0.08351   0.588  0.55942   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.645 on 43 degrees of freedom
#> Multiple R-squared:  0.2291,	Adjusted R-squared:  0.1753 
#> F-statistic:  4.26 on 3 and 43 DF,  p-value: 0.01014

#-----------------------------------------------------------------------------
# Create p-value function
#-----------------------------------------------------------------------------

res <- conf_dist(
  estimate = c(-0.02143)
  , df = c(43)
  , stderr = (0.02394)
  , type = "linreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  # , est_names = c("")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "Coefficient Agriculture"
  , xlim = c(-0.12, 0.065)
  , together = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = FALSE
  , plot = TRUE
)
```

<img src="figure/linreg_single_pval-1.png" title="plot of chunk linreg_single_pval" alt="plot of chunk linreg_single_pval" width="80%" style="display: block; margin: auto;" />

#### Confidence distribution


```r
res <- conf_dist(
  estimate = c(-0.02143)
  , df = c(43)
  , stderr = (0.02394)
  , type = "linreg"
  , plot_type = "cdf"
  , n_values = 1e4L
  # , est_names = c("")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  # , log_yaxis = TRUE
  # , cut_logyaxis = 0.05
  , xlab = "Coefficient Agriculture"
  , xlim = c(-0.12, 0.065)
  , together = FALSE
  , col = "#08A9CF"
  # , plot_p_limit = 1 - 0.999
  , plot_counternull = FALSE
)
```

<img src="figure/linreg_single_cdf-1.png" title="plot of chunk linreg_single_cdf" alt="plot of chunk linreg_single_cdf" width="80%" style="display: block; margin: auto;" />

The point where the confidence distribution is $0.5$ is the median unbiased estimator (see Xie & Singh (2013) for a review and proofs). 

### Multiple coefficients from a linear regression model
#### *P*-value functions


```r
res <- conf_dist(
  estimate = c(0.13115, 0.04913)
  , df = c(43, 43)
  , stderr = c(0.04145, 0.08351)
  , type = "linreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , est_names = c("Fertility", "Examination")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Regression coefficients"
  , together = TRUE
  , same_color = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = FALSE
  , inverted = FALSE
)
```

<img src="figure/linreg_multiple_pval-1.png" title="plot of chunk linreg_multiple_pval" alt="plot of chunk linreg_multiple_pval" width="80%" style="display: block; margin: auto;" />

#### Surprisal values


```r
res <- conf_dist(
  estimate = c(0.13115, 0.04913)
  , df = c(43, 43)
  , stderr = c(0.04145, 0.08351)
  , type = "linreg"
  , plot_type = "s_val"
  , n_values = 1e4L
  , est_names = c("Fertility", "Examination")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  # , log_yaxis = TRUE
  # , cut_logyaxis = 0.05
  , xlab = "Regression coefficients"
  , together = TRUE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
)
```

<img src="figure/linreg_multiple_sval-1.png" title="plot of chunk linreg_multiple_sval" alt="plot of chunk linreg_multiple_sval" width="80%" style="display: block; margin: auto;" />

### Pearson correlation coefficient (one-sided)


```r
#-----------------------------------------------------------------------------
# Calculate Pearson's correlation coefficient
#-----------------------------------------------------------------------------

cor.test(swiss$Fertility, swiss$Agriculture, alternative = "two.sided", method = "pearson")
#> 
#> 	Pearson's product-moment correlation
#> 
#> data:  swiss$Fertility and swiss$Agriculture
#> t = 2.5316, df = 45, p-value = 0.01492
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  0.07334947 0.58130587
#> sample estimates:
#>       cor 
#> 0.3530792

#-----------------------------------------------------------------------------
# Create p-value function
#-----------------------------------------------------------------------------

res <- conf_dist(
  estimate = c(0.3530792)
  , n = 47
  , type = "pearson"
  , plot_type = "p_val"
  , n_values = 1e4L
  # , est_names = c("")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "one_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Pearson correlation"
  , together = TRUE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = FALSE
)
```

<img src="figure/corr_pearson-1.png" title="plot of chunk corr_pearson" alt="plot of chunk corr_pearson" width="80%" style="display: block; margin: auto;" />

### Odds ratio from logistic regression


```r
#-----------------------------------------------------------------------------
# Create p-value function
#-----------------------------------------------------------------------------

res <- conf_dist(
  estimate = c(0.804037549)
  , stderr = c(0.331819298)
  , type = "logreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , est_names = c("GPA")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(log(1)) # null value on the log-odds scale
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Odds Ratio (GPA)"
  , xlim = log(c(0.7, 5.2)) # axis limits on the log-odds scale
  , together = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
  , x_scale = "default"
)
```

<img src="figure/logreg-1.png" title="plot of chunk logreg" alt="plot of chunk logreg" width="80%" style="display: block; margin: auto;" />

### Proportion

The *p*-value function (and thus the confidence intervals) are based on Wilson's score interval and not the normal approximation. This means that the *p*-value function will never be outside the interval $[0, 1]$.


```r
res <- conf_dist(
  estimate = c(0.44)
  , n = c(50)
  , type = "prop"
  , plot_type = "p_val"
  , n_values = 1e4L
  # , est_names = c("")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0.5)
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Proportion"
  # , xlim = c(0.25, 0.65)
  , together = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
  , x_scale = "default"
)
#> 
#> Transformation changed to identity.
```

<img src="figure/prop-1.png" title="plot of chunk prop" alt="plot of chunk prop" width="80%" style="display: block; margin: auto;" />

### Difference between two independent proportions: Wilson's score by Newcombe with continuity correction


```r
res <- conf_dist(
  estimate = c(68/100, 98/150)
  , n = c(100, 150)
  , type = "propdiff"
  , plot_type = "p_val"
  , n_values = 1e4L
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Difference between proportions"
  , together = FALSE
  , plot_p_limit = 1 - 0.9999
  , plot_counternull = FALSE
)
```

<img src="figure/propdiff_Wilson-1.png" title="plot of chunk propdiff_Wilson" alt="plot of chunk propdiff_Wilson" width="80%" style="display: block; margin: auto;" />

### Difference between two independent proportions: Agresti-Caffo adjusted Wald interval

The standard Wald interval can be modified in a simple manner to drastically improve its coverage probabilities. Simply add 1 to the number of successes and add 2 to the sample size for both proportions. Then proceed to calculate the Wald interval with these modified data. The point estimate for the difference between proportions is still calculated using the unmodified data. The function `conf_dist` does not have a dedicaded type for this kind of estimator but as the Wald interval is based on the normal distribution, we can use `type = general_z` to create the *p*-value function.


```r

# First proportion

x1 <- 8
n1 <- 40

# Second proportion

x2 <- 11
n2 <- 30

# Apply the correction 

p1hat <- (x1 + 1)/(n1 + 2)
p2hat <- (x2 + 1)/(n2 + 2)

# The estimator (unmodified)

est0 <- (x1/n1) - (x2/n2)

# The modified estimator and its standard error using the correction

est <- p1hat - p2hat
se <- sqrt(((p1hat*(1 - p1hat))/(n1 + 2)) + ((p2hat*(1 - p2hat))/(n2 + 2)))

res <- conf_dist(
  estimate = c(est)
  , stderr = c(se)
  , type = "general_z"
  , plot_type = "p_val"
  , n_values = 1e4L
  # , est_names = c("Estimate")
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , conf_level = c(0.95, 0.99)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  , xlab = "Difference of proportions"
  # , xlim = c(-0.75, 0.5)
  , together = FALSE
  , plot_p_limit = 1 - 0.9999
  , plot_counternull = FALSE
)
```

<img src="figure/propdiff_agresticaffo-1.png" title="plot of chunk propdiff_agresticaffo" alt="plot of chunk propdiff_agresticaffo" width="80%" style="display: block; margin: auto;" />

### Confidence density of a variance estimate from a normal distribution

The confidence density of a variance estimate is skewed. This means that the mean, mode and median of the confidence density will not be identical, in general.


```r
# Simulate some data from a normal distribution

set.seed(142857)
var_est <- var(x <- rnorm(20, 100, 15))

res <- conf_dist(
  estimate = var_est
  , n = length(x)
  , type = "var"
  , plot_type = "pdf"
  , n_values = 1e4L
  , est_names = c("Variance")
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , conf_level = c(0.95)
  # , null_values = c(15^2, 18^2)
  , trans = "identity"
  , alternative = "two_sided"
  , xlab = "Variance"
  , xlim = c(100, 900)
  , together = TRUE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
)
```


```r
# Add vertical lines at the point estimates (mode, median, mean)

res$plot + ggplot2::geom_vline(xintercept = as.numeric(res$point_est[1, 1:3]), linetype = 2, size = 1)
```

<img src="figure/variance_plot-1.png" title="plot of chunk variance_plot" alt="plot of chunk variance_plot" width="80%" style="display: block; margin: auto;" />

### *P*-value function and confidence distribution for the relative survival effect (1 - HR%)

Here, I'm going to replicate **Figure 1** and **Figure 2** in Bender et al. (2005). To do this, we first need to define the transformation that transforms the log-HR into the relative survival effect. By specifying `trans = rse_fun`, the values are automatically transformed and plotted. In addition, we will plot the *p*-value function inverted so that the cusp is located at the bottom. First the *p*-value function:


```r
# Define the transformation function and its inverse for the relative survival effect

rse_fun <- function(x){
  100*(1 - exp(x))
}

rse_fun_inv <- function(x){
  log(1 - (x/100))
}

res <- conf_dist(
  estimate = c(log(0.72))
  , stderr = (0.187618)
  , type = "coxreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , est_names = c("RSE")
  , conf_level = c(0.95, 0.8, 0.5)
  , null_values = rse_fun_inv(c(0))
  , trans = "rse_fun"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Relative survival effect (1 - HR%)"
  , xlim = rse_fun_inv(c(-30, 60))
  , together = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
  , inverted = TRUE
  , title = "Figure 1 in Bender et al. (2005)"
  , x_scale = "default"
)
```

<img src="figure/rse_pval-1.png" title="plot of chunk rse_pval" alt="plot of chunk rse_pval" width="80%" style="display: block; margin: auto;" />

```r

rm(rse_fun, rse_fun_inv)
```
Now the confidence distribution (Figure 2):

```r
# Define the transformation function and its inverse for the relative survival effect

rse_fun <- function(x){
  100*(1 - exp(-x))
}

rse_fun_inv <- function(x){
  log(-(100)/(x - 100))
}

res <- conf_dist(
  estimate = c(-log(0.72))
  , stderr = (0.187618)
  , type = "coxreg"
  , plot_type = "cdf"
  , n_values = 1e4L
  , est_names = c("RSE")
  , conf_level = c(0.95, 0.883, 0.5)
  , null_values = rse_fun_inv(c(0))
  , trans = "rse_fun"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Relative survival effect (1 - HR%)"
  , together = FALSE
  , xlim = rse_fun_inv(c(-3, 60))
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
  , inverted = TRUE
  , title = "Figure 2 in Bender et al. (2005)"
)
```

<img src="figure/rse_cdf-1.png" title="plot of chunk rse_cdf" alt="plot of chunk rse_cdf" width="80%" style="display: block; margin: auto;" />

```r

rm(rse_fun, rse_fun_inv)
```


### Comparing the precision of multiple estimates using the AUCC (area under the confidence curve)
The AUCC is a scalar representing the area under the *p*-value function (see Berrar (2017)). Values close to 0 indicate a high precision while larger values indicate poorer precision. Multiple estimates can be compared by their AUCC (in the context of a meta-analysis for example). The AUCC is calculated numerically using trapezoidal integration on the untransformed *p*-value function. The estimate can be quite poor if few x-values are used (i.e. argument `n_values` is low).


```r

# Lungcancer dataset from the "meta" package

res <- conf_dist(
  estimate = c(2.512, 2.298, 2.455, 2.255, 2.989, 1.59, 2.674) # Log-incidence rate ratio (IRR)
  , stderr = c(0.087, 0.127, 0.144, 0.153, 0.265, 0.318, 0.584) # Standard errors of the log-IRR
  , type = "general_z"
  , plot_type = "p_val"
  , n_values = 1e4L
  , est_names = c("U.S. Veterans", "Men in 9 States", "Canadian Veterans", "Men in 25 States", "British Doctors", "California Legion", "California Occupational") # Study names
  , conf_level = c(0.95)
  , null_values = c(log(1))
  , trans = "exp"
  , alternative = "two_sided"
  , xlab = "Incidence rate ratio (IRR)"
  , xlim = c(log(0.95), log(50))
  , together = TRUE
  , same_color = TRUE
  , col = "#C977A2"
  , plot_p_limit = 1 - 0.9999
  , plot_counternull = FALSE
  , inverted = FALSE
)
```

<img src="figure/aucc_comparison-1.png" title="plot of chunk aucc_comparison" alt="plot of chunk aucc_comparison" width="80%" style="display: block; margin: auto;" />

```r

# Print the AUCCs
print(res$aucc_frame, row.names = FALSE)
#>                 variable      aucc
#>            U.S. Veterans 0.1388319
#>          Men in 9 States 0.2026627
#>        Canadian Veterans 0.2297908
#>         Men in 25 States 0.2441527
#>          British Doctors 0.4228789
#>        California Legion 0.5074546
#>  California Occupational 0.9319293
```

## Similar packages

Check out the [concurve](https://cran.r-project.org/package=concurve) package that offers a similar functionality.

## References

Bender R, Berg G, Zeeb H. (2005): Tutorial: using confidence curves in medical research. *Biom J.* 47(2): 237-47.

Berrar D (2017): Confidence Curves: an alternative to null hypothesis significance testing for the comparison of classifiers. *Mach Learn* 106:911-949.

Fraser  D. A. S. (2019): The *p*-value function and statistical inference. *Am Stat,* 73:sup1, 135-147.

Greenland S (2019): Valid *P*-Values Behave Exactly as They Should: Some Misleading Criticisms of *P*-Values and Their Resolution with *S*-Values. *Am Stat,* 73sup1, 106-114.

Infanger D, Schmidt-Trucksäss A. (2019): *P* value functions: An underused method to present research results and to promote quantitative reasoning. *Stat Med,* 38, 4189-4197. doi: 10.1002/sim.8293.

Poole C. (1987a): Beyond the confidence interval. *Am J Public Health.* 77(2): 195-9.

Poole C. (1987b) Confidence intervals exclude nothing. *Am J Public Health.* 77(4): 492-3.

Rosenthal R, Rubin DB. (1994): The counternull value of an effect size: A new statistic. Psychol Sci. 5(6): 329-34.

Schweder T, Hjort NL. (2016): Confidence, likelihood, probability: statistical inference with confidence distributions. New York, NY: Cambridge University Press.

Xie M, Singh K, Strawderman WE. (2011): Confidence Distributions and a Unifying Framework for Meta-Analysis. *J Am Stat Assoc* 106(493): 320-33. doi: 10.1198/jasa.2011.tm09803.

Xie Mg, Singh K. (2013): Confidence distribution, the frequentist distribution estimator of a parameter: A review. *Internat Statist Rev.* 81(1): 3-39.

## Contact

[Denis Infanger](https://dsbg.unibas.ch/de/personen/denis-infanger/)

## Session info


```
#> R Under development (unstable) (2020-12-07 r79587)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 19042)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=C                        LC_CTYPE=German_Switzerland.1252   
#> [3] LC_MONETARY=German_Switzerland.1252 LC_NUMERIC=C                       
#> [5] LC_TIME=German_Switzerland.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] pvaluefunctions_1.6.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] Rcpp_1.0.5         pracma_2.2.9       RColorBrewer_1.1-2 highr_0.8         
#>  [5] pillar_1.4.7       compiler_4.1.0     prettyunits_1.1.1  tools_4.1.0       
#>  [9] digest_0.6.27      testthat_3.0.0     pkgbuild_1.1.0     pkgload_1.1.0     
#> [13] evaluate_0.14      lifecycle_0.2.0    tibble_3.0.4       gtable_0.3.0      
#> [17] pkgconfig_2.0.3    rlang_0.4.9        cli_2.2.0          rstudioapi_0.13   
#> [21] parallel_4.1.0     xfun_0.19          withr_2.3.0        stringr_1.4.0     
#> [25] roxygen2_7.1.1     xml2_1.3.2         knitr_1.30         vctrs_0.3.5       
#> [29] desc_1.2.0         rprojroot_2.0.2    grid_4.1.0         glue_1.4.2        
#> [33] R6_2.5.0           processx_3.4.5     fansi_0.4.1        farver_2.0.3      
#> [37] zipfR_0.6-70       callr_3.5.1        purrr_0.3.4        ggplot2_3.3.2     
#> [41] magrittr_2.0.1     ellipsis_0.3.1     scales_1.1.1       ps_1.5.0          
#> [45] assertthat_0.2.1   colorspace_2.0-0   stringi_1.5.3      munsell_0.5.0     
#> [49] crayon_1.3.4
```

<!-- ## License -->

<!-- [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) -->

<!-- [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0). -->













