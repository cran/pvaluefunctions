## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
  , comment = "#>"
  # , tidy.opts = list(width.cutoff = 160, tidy = FALSE)
)

## ----source_github, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE----
library(devtools)

# Load function
source_url("https://raw.githubusercontent.com/DInfanger/pvalue_functions/master/confidence_distributions.R")


## ----ttest, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----

#-----------------------------------------------------------------------------
# Sourcing function
#-----------------------------------------------------------------------------

#source("confidence_distributions.R")

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
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "Mean difference (group 1 - group 2)"
  , together = FALSE
  , plot_p_limit = 1 - 0.999
)

## ----linreg_single_pval, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----
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
  , together = FALSE
  , plot_p_limit = 1 - 0.999
)

## ----linreg_single_cdf, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----
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
  # , plot_p_limit = 1 - 0.999
)


## ----linreg_multiple_pval, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----
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
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "Coefficients"
  , together = TRUE
  , plot_p_limit = 1 - 0.999
)

## ----linreg_multiple_sval, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----
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
  , xlab = "Coefficients"
  , together = TRUE
  , plot_p_limit = 1 - 0.999
)

## ----corr_pearson, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----
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
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "Pearson correlation"
  , together = TRUE
  , plot_p_limit = 1 - 0.999
)

## ----logreg, message = FALSE, warning = FALSE, fig.width = 9.2, fig.height = 7.2, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----
#-----------------------------------------------------------------------------
# Calculate logistic regression model using a dataset from UCLA
#-----------------------------------------------------------------------------

dat_tmp <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

dat_tmp$rank <- factor(dat_tmp$rank)
logistic_mod <- glm(admit ~ gre + gpa + rank, data = dat_tmp, family = "binomial")

summary(logistic_mod)

rm(dat_tmp)

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
  , null_values = c(log(1))
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "Odds Ratio (GPA)"
  , xlim = log(c(0.4, 5))
  , together = FALSE
  , plot_p_limit = 1 - 0.999
)

## ----prop, message = FALSE, warning = FALSE, fig.width = 9, fig.height = 7, out.width = "80%", fig.align='center', dev = "tiff", dev.args = list(compression = "lzw"), dpi = 300----
res <- conf_dist(
  estimate = c(0.44)
  , n = c(50)
  , type = "prop"
  , plot_type = "p_val"
  , n_values = 1e4L
  # , est_names = c("")
  , conf_level = c(0.95, 0.90, 0.80)
  , null_values = c(0.5)
  , trans = "identity"
  , alternative = "two_sided"
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "Proportion"
  # , xlim = log(c(0.95, 1.2))
  , together = FALSE
  , plot_p_limit = 1 - 0.999
)

## ----session_info, include=TRUE, echo=FALSE------------------------------
sessionInfo()

