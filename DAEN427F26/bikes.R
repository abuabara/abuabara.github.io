# PACKAGES
library(tidyverse)
library(bayesrules)
library(rstanarm)

# DATA
data(bikes)

head(bikes)

bikes$day_of_week

glimpse(bikes)

summary(bikes[, c("rides", "temp_feel")])

nrow(bikes)

cor(bikes$temp_feel, bikes$rides)

lm(rides ~ temp_feel, data = bikes)

summary(lm(rides ~ temp_feel, data = bikes))

hist(bikes$rides)

boxplot(bikes$rides)

############################################################

# BAYESIAN REGRESSION - BIKE RIDERSHIP
bike_model <- stan_glm(
  rides ~ temp_feel,
  data = bikes,
  family = gaussian(),
  prior_intercept = normal(5000, 1000),
  prior = normal(100, 40),
  prior_aux = exponential(0.0008),
  chains = 4,
  iter = 10000,
  seed = 84735
)

summary(bike_model)

plot(bike_model)

# Extract MCMC draws as an array (for bayesplot)
bike_draws_array <- as.array(bike_model)

head(bike_draws_array)

library(bayesplot)

mcmc_trace(bike_draws_array, alpha = 0.1) # Trace plots
mcmc_dens_overlay(bike_draws_array) # Density plots
mcmc_acf(bike_draws_array) # Autocorrelation plots

draws <- as.matrix(bike_model) # Obtain posterior parameter draws

head(draws)

# Expected ridership at 75 F
mu_75 <- draws[, "(Intercept)"] + 75 * draws[, "temp_feel"]

plot(mu_75)

hist(mu_75)

# 95% credible interval for mean ridership
quantile(mu_75, c(0.025, 0.975))

# Simulate actual ridership on a new day
y_new <- rnorm(
  n = length(mu_75),
  mean = mu_75,
  sd = draws[, "sigma"]
)

# 95% posterior predictive interval
quantile(y_new, c(0.025, 0.975))
