## ----setup, include = FALSE, echo = FALSE-------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
  , comment = "#>"
)

## ----load_package, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE----
library(pvaluefunctions)

## ----source_github, message = FALSE, warning = FALSE, echo = FALSE, eval = FALSE----
#  library(devtools)
#  
#  # Load function
#  source_url("https://raw.githubusercontent.com/DInfanger/pvaluefunctions/master/R/confidence_distributions.R")
#  

## ----ttest, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
#-----------------------------------------------------------------------------
# T-Test
#-----------------------------------------------------------------------------

with(sleep, mean(extra[group == 1])) - with(sleep, mean(extra[group == 2]))
t.test(extra ~ group, data = sleep, var.equal = FALSE)

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


## ----linreg_single_pval, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
#-----------------------------------------------------------------------------
# Model
#-----------------------------------------------------------------------------

mod <- lm(Infant.Mortality~Agriculture + Fertility + Examination, data = swiss)

summary(mod)

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

## ----linreg_single_cdf, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

## ----linreg_multiple_pval, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

## ----linreg_multiple_sval, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

## ----corr_pearson, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
#-----------------------------------------------------------------------------
# Calculate Pearson's correlation coefficient
#-----------------------------------------------------------------------------

cor.test(swiss$Fertility, swiss$Agriculture, alternative = "two.sided", method = "pearson")

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

## ----logreg, message = FALSE, warning = FALSE, fig.width = 10.0, fig.height = 7.2, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

## ----prop, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

## ----propdiff_Wilson, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

## ----propdiff_agresticaffo, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----

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


## ----variance_calcs, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', eval = TRUE, echo = TRUE, fig.show = "hide", dev = "png", dpi = 200----
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


## ----variance_plot, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', eval = TRUE, echo = TRUE, dev = "png", dpi = 200----
# Add vertical lines at the point estimates (mode, median, mean)

res$plot + ggplot2::geom_vline(xintercept = as.numeric(res$point_est[1, 1:3]), linetype = 2, size = 1)

## ----rse_pval, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

rm(rse_fun, rse_fun_inv)


## ----rse_cdf, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----
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

rm(rse_fun, rse_fun_inv)


## ----aucc_comparison, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "png", dpi = 200----

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

# Print the AUCCs
print(res$aucc_frame, row.names = FALSE)

## ----session_info, include=TRUE, echo=FALSE-----------------------------------
sessionInfo()

