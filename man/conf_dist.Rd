% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence_distributions.R
\name{conf_dist}
\alias{conf_dist}
\title{Create and Plot \emph{P}-Value Functions, S-Value Functions, Confidence Distributions and Confidence Densities}
\usage{
conf_dist(
  estimate = NULL,
  n = NULL,
  df = NULL,
  stderr = NULL,
  tstat = NULL,
  type = NULL,
  plot_type = c("p_val", "s_val", "cdf", "pdf"),
  n_values = 10000L,
  est_names = NULL,
  conf_level = NULL,
  null_values = NULL,
  trans = "identity",
  alternative = c("two_sided", "one_sided"),
  log_yaxis = FALSE,
  cut_logyaxis = 0.05,
  xlab = NULL,
  xlim = NULL,
  together = FALSE,
  plot_legend = TRUE,
  same_color = FALSE,
  col = "black",
  nrow = NULL,
  ncol = NULL,
  plot_p_limit = (1 - 0.999),
  plot_counternull = FALSE,
  title = NULL,
  ylab = NULL,
  ylab_sec = NULL,
  inverted = FALSE,
  x_scale = c("default", "linear", "logarithm"),
  plot = TRUE
)
}
\arguments{
\item{estimate}{Numerical vector containing the estimate(s).}

\item{n}{Numerical vector containing the sample size(s). Required for correlations, variances, proportions and differences between proportions. Must be equal the number of estimates.}

\item{df}{Numerical vector containing the degrees of freedom. Required for statistics based on the \emph{t}-distribution (e.g. linear regression) and \emph{t}-tests. Must be equal the number of estimates.}

\item{stderr}{Numerical vector containing the standard error(s) of the estimate(s). Required for statistics based on the \emph{t}-distribution (e.g. linear regression) and the normal distribution (e.g. logistic regression). Must be equal the number of estimate(s).}

\item{tstat}{Numerical vector contaiqning the \emph{t}-statistic(s). Required for \emph{t}-tests (means and mean differences). Must be equal the number of estimates.}

\item{type}{String indicating the type of the estimate. Must be one of the following: \code{ttest}, \code{linreg}, \code{gammareg}, \code{general_t}, \code{logreg}, \code{poisreg}, \code{coxreg}, \code{general_z}, \code{pearson}, \code{spearman}, \code{kendall}, \code{var}, \code{prop}, \code{propdiff}.}

\item{plot_type}{String indicating the type of plot. Must be one of the following: \code{cdf} (confidence distribution), \code{pdf} (confidence density), \code{p_val} (\emph{p}-value function, the default), \code{s_val} (Surprisal value functions). For differences between independent proportions, only \emph{p}-value functions and Surprisal values are available.}

\item{n_values}{(optional) Integer indicating the number of points that are used to generate the graphics. The higher this number, the higher the computation time and resolution.}

\item{est_names}{(optional) String vector indicating the names of the estimate(s). Must be equal the number of estimates.}

\item{conf_level}{(optional) Numerical vector indicating the confidence level(s). Bust be between 0 and 1.}

\item{null_values}{(optional) Numerical vector indicating the null value(s) in the plot on the \emph{untransformed (original)} scale. For example: The null values for an odds ratio of 1 is 0 on the log-odds scale. If x limits are specified with \code{xlim}, all null values outside of the specified x limits are ignored for plotting and a message is printed.}

\item{trans}{(optional) String indicating the transformation function that will be applied to the estimates and confidence curves. For example: \code{"exp"} for an exponential transformation of the log-odds in logistic regression. Can be a custom function.}

\item{alternative}{String indicating if the confidence level(s) are two-sided or one-sided. Must be one of the following: \code{two_sided}, \code{one_sided}.}

\item{log_yaxis}{Logical. Indicating if a portion of the y-axis should be displayed on the logarithmic scale.}

\item{cut_logyaxis}{Numerical value indicating the threshold below which the y-axis will be displayed logarithmically. Must lie between 0 and 1.}

\item{xlab}{(optional) String indicating the label of the x-axis.}

\item{xlim}{(optional) Optional numerical vector of length 2 (x1, x2) indicating the limits of the x-axis on the \emph{untransformed} scale if \code{trans} is not \code{identity}. The scale of the x-axis set by \code{x_scale} does not affect the x limits. For example: If you want to plot \emph{p}-value functions for odds ratios from logistic regressions, the limits have to be given on the log-odds scale if \code{trans = "exp"}. Note that x1 > x2 is allowed but then x2 will be the left limit and x1 the right limit (i.e. the limits are sorted before plotting). Null values (specified in \code{null_values}) that are outside of the specified limits are ignored and a message is printed.}

\item{together}{Logical. Indicating if graphics for multiple estimates should be displayed together or on separate plots.}

\item{plot_legend}{Logical. Indicating if a legend should be plotted if multiple curves are plotted together with different colors (i.e. \code{together = TRUE)} and \code{same_color = FALSE}).}

\item{same_color}{Logical. Indicating if curves should be distinguished using colors if they are plotted together (i.e. \code{together = TRUE}). Setting this to FALSE also disables the default behavior that the two halves of the curves are plotted in different colors for a one-sided alternative.}

\item{col}{String indicating the colour of the curves. Only relevant for single curves, multiple curves not plotted together (i.e. \code{together = FALSE}) and multiple curves plotted together but with the option \code{same_color} set to \code{TRUE}.}

\item{nrow}{(optional) Integer greater than 0 indicating the number of rows when \code{together = FALSE} is specified for multiple estimates. Used in \code{facet_wrap} in ggplot2.}

\item{ncol}{(optional) Integer greater than 0 indicating the number of columns when \code{together = FALSE} is specified for multiple estimates. Used in \code{facet_wrap} in ggplot2.}

\item{plot_p_limit}{Numerical value indicating the lower limit of the y-axis. Must be greater than 0 for a logarithmic scale (i.e. \code{log_yaxis = TRUE}). The default is to omit plotting \emph{p}-values smaller than 1 - 0.999 = 0.001.}

\item{plot_counternull}{Logical. Indicating if the counternull should be plotted as a point. Only available for \emph{p}-value functions and s-value functions. Counternull values that are outside of the plotted functions are not shown.}

\item{title}{(optional) String containing a title of the plot.}

\item{ylab}{(optional) String indicating the title for the primary (left) y-axis.}

\item{ylab_sec}{(optional) String indicating the title for the secondary (right) y-axis.}

\item{inverted}{Logical. Indicating the orientation of the y-axis for the \emph{P}-value function (\code{p_val}), S-value function (\code{s_val}) and the confidence distribution (\code{cdf}). By default (i.e. \code{inverted = FALSE}) small \emph{P}-values are plotted at the bottom and large ones at the top so that the cusp of the \emph{P}-value function is a the top. By setting \code{inverted = TRUE}, the y-axis is inverted. Ignored for confidence densities.}

\item{x_scale}{String indicating the scaling of the x-axis. The default is to scale the x-axis logarithmically if the transformation specified in \code{trans} is "exp" (exponential) and linearly otherwise. The option \code{linear} (can be abbreviated) forces a linear scaling and the option \code{logarithm} (can be abbreviated) forces a logarithmic scaling, regardless what has been specified in \code{trans}.}

\item{plot}{Logical. Should a plot be created (\code{TRUE}, the default) or not (\code{FALSE}). \code{FALSE} can be useful if users want to create their own plots using the returned data from the function. If \code{FALSE}, no ggplot2 object is returned.}
}
\value{
\code{conf_dist} returns four data frames and if \code{plot = TRUE} was specified, a ggplot2-plot object: \code{res_frame} (contains parameter values (e.g. mean differences, odds ratios etc.), \emph{p}-values (one- and two-sided), s-values, confidence distributions and densities, variable names and type of hypothesis), \code{conf_frame} (contains the specified confidence level(s) and the corresponding lower and upper limits as well as the corresponding variable name), \code{counternull_frame} (contains the counternull and the corresponding null values), \code{point_est} (contains the mean, median and mode point estimates) and if \code{plot = TRUE} was specified, \code{aucc_frame} contains the estimated AUCC (area under the confidence curve, see Berrar 2017) calculated by trapezoidal integration on the untransformed scale. Also provides the proportion of the aucc that lies above the null value(s) if they are provided. \code{plot} (a ggplot2 object).
}
\description{
The function \code{conf_dist} generates confidence distributions (cdf), confidence densities (pdf), Shannon suprisal (s-value) functions and \emph{p}-value functions for several commonly used estimates. In addition, counternulls (see Rosenthal et al. 1994), point estimates and the area under the confidence curve (AUCC) are calculated.
}
\details{
\emph{P}-value functions and confidence intervals are calculated based on the \emph{t}-distribution for \emph{t}-tests, linear regression coefficients, and gamma regression models (GLM). The normal distribution is used for logistic regression, poisson regression and cox regression models. For correlation coefficients, Fisher's transform is used using the corresponding variances (see Bonett et al. 2000). \emph{P}-value functions and confidence intervals for variances are constructed using the Chi2 distribution. Finally, Wilson's score intervals are used for one proportion. For differences of proportions, the Wilson score interval with continuity correction is used (Newcombe 1998).
}
\examples{

#======================================================================================
# Create a p-value function for an estimate using the normal distribution
#======================================================================================

res <- conf_dist(
  estimate = c(-0.13)
  , stderr = c(0.224494)
  , type = "general_z"
  , plot_type = "p_val"
  , n_values = 1e4L
  , est_names = c("Parameter value")
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , conf_level = c(0.95)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  , xlab = "Var"
  , xlim = c(-1, 1)
  , together = TRUE
  , plot_p_limit = 1 - 0.9999
  , plot_counternull = TRUE
  , title = NULL
  , ylab = NULL
  , ylab_sec = NULL
  , inverted = FALSE
  , x_scale = "default"
  , plot = TRUE
)

#======================================================================================
# P-value function for a single regression coefficient (Agriculture in the model below)
#======================================================================================

mod <- lm(Infant.Mortality~Agriculture + Fertility + Examination, data = swiss)
summary(mod)

res <- conf_dist(
  estimate = c(-0.02143)
  , df = c(43)
  , stderr = (0.02394)
  , type = "linreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0)
  , trans = "identity"
  , alternative = "two_sided"
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "Coefficient Agriculture"
  , together = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = FALSE
  , title = NULL
  , ylab = NULL
  , ylab_sec = NULL
  , inverted = FALSE
  , x_scale = "default"
  , plot = TRUE
)

#=======================================================================================
# P-value function for an odds ratio (logistic regression), plotted with inverted y-axis
#=======================================================================================

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
  , title = NULL
  , ylab = NULL
  , ylab_sec = NULL
  , inverted = TRUE
  , x_scale = "default"
  , plot = TRUE
)

#======================================================================================
# Difference between two independent proportions: Newcombe with continuity correction
#======================================================================================

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
  , col = "#A52A2A" # Color curve in auburn
  , plot_p_limit = 1 - 0.9999
  , plot_counternull = FALSE
  , title = NULL
  , ylab = NULL
  , ylab_sec = NULL
  , inverted = FALSE
  , x_scale = "default"
  , plot = TRUE
)

#======================================================================================
# Difference between two independent proportions: Agresti & Caffo
#======================================================================================

# First proportion
x1 <- 8
n1 <- 40

# Second proportion
x2 <- 11
n2 <- 30

# Apply the correction
p1hat <- (x1 + 1)/(n1 + 2)
p2hat <- (x2 + 1)/(n2 + 2)

# The original estimator
est0 <- (x1/n1) - (x2/n2)

# The unmodified estimator and its standard error using the correction

est <- p1hat - p2hat
se <- sqrt(((p1hat*(1 - p1hat))/(n1 + 2)) + ((p2hat*(1 - p2hat))/(n2 + 2)))

res <- conf_dist(
  estimate = c(est)
  , stderr = c(se)
  , type = "general_z"
  , plot_type = "p_val"
  , n_values = 1e4L
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , conf_level = c(0.95, 0.99)
  , null_values = c(0, 0.3)
  , trans = "identity"
  , alternative = "two_sided"
  , xlab = "Difference of proportions"
  , together = FALSE
  , plot_p_limit = 1 - 0.9999
  , plot_counternull = FALSE
  , title = "P-value function for the difference of two independent proportions"
  , ylab = NULL
  , ylab_sec = NULL
  , inverted = FALSE
  , x_scale = "default"
  , plot = TRUE
)

#========================================================================================
# P-value function and confidence distribution for the relative survival effect (1 - HR\%)
# Replicating Figure 1 in Bender et al. (2005)
#========================================================================================

# Define the transformation function and its inverse for the relative survival effect

rse_fun <- function(x){ # x is the log-hazard ratio
  100*(1 - exp(x))
}

rse_fun_inv <- function(x){
  log(1 - (x/100))
}

res <- conf_dist(
  estimate = log(0.72)
  , stderr = 0.187618
  , type = "coxreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , est_names = c("RSE")
  , conf_level = c(0.95, 0.8, 0.5)
  , null_values = rse_fun_inv(0)
  , trans = "rse_fun"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "Relative survival effect (1 - HR\%)"
  , xlim = rse_fun_inv(c(-30, 60))
  , together = FALSE
  , plot_p_limit = 1 - 0.999
  , plot_counternull = TRUE
  , inverted = TRUE
  , title = "Figure 1 in Bender et al. (2005)"
  , x_scale = "default"
  , plot = TRUE
)

}
\references{
Bender R, Berg G, Zeeb H. Tutorial: using confidence curves in medical research. \emph{Biom J.} 2005;47(2):237-247.

Berrar D. Confidence curves: an alternative to null hypothesis significance testing for the comparison of classifiers. \emph{Mach Learn.} 2017;106:911-949.

Bonett DG, Wright TA. Sample size requirements for estimating Pearson, Kendall and Spearman correlations. \emph{Psychometrika.} 2000;65(1):23-28.

Cole SR, Edwards JK, Greenland S. Surprise! \emph{Am J Epidemiol.} 2021:190(2):191-193.

Infanger D, Schmidt-Trucksäss A. \emph{P} value functions: An underused method to present research results and to promote quantitative reasoning. \emph{Stat Med.} 2019;38:4189-4197.

Newcombe RG. Interval estimation for the difference between independent proportions: comparison of eleven methods. \emph{Stat Med.} 1998;17:873-890.

Poole C. Confidence intervals exclude nothing. \emph{Am J Public Health.} 1987;77(4):492-493.

Poole C. Beyond the confidence interval. \emph{Am J Public Health.} 1987;77(2):195-199.

Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical science: replace confidence and significance by compatibility and surprise. \emph{BMC Med Res Methodol} 2020;20:244.

Rosenthal R, Rubin D. The counternull value of an effect size: a new statistic. \emph{Psychological Science.} 1994;5(6):329-334.

Rothman KJ, Greenland S, Lash TL. Modern epidemiology. 3rd ed. Philadelphia, PA: Wolters Kluwer; 2008.

Schweder T, Hjort NL. Confidence, likelihood, probability: statistical inference with confidence distributions. New York, NY: Cambridge University Press; 2016.

Sullivan KM, Foster DA. Use of the confidence interval function. \emph{Epidemiology.} 1990;1(1):39-42.

Xie Mg, Singh K. Confidence distribution, the frequentist distribution estimator of a parameter: A review. \emph{Internat Statist Rev.} 2013;81(1):3-39.
}
