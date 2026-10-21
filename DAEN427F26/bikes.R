library(tidyverse)
library(bayesrules)
library(rstanarm)
library(bayesplot)

data(bikes)

# 1. Explore the observed data
head(bikes)

glimpse(bikes)

summary(bikes[, c("rides", "temp_feel")])

nrow(bikes)

(correlation <- cor(bikes$temp_feel, bikes$rides, use = "complete.obs"))

summary(lm(rides ~ temp_feel, data = bikes))

hist(bikes$rides, main = "Observed daily rides", xlab = "Rides")

boxplot(bikes$rides, main = "Daily rides", horizontal = TRUE)

# 2. Center temperature: 75 F is represented by temp_feel_c = 5
bikes <- mutate(bikes, temp_feel_c = temp_feel - 70)

summary(bikes$temp_feel_c)

boxplot(bikes$temp_feel_c, main = "Centered temp (70 F)", horizontal = TRUE)

# Fit ONE Bayesian model with centered temperature.
# prior_intercept is applied to the intercept after rstanarm's INTERNAL
# predictor centering (at the observed mean of temp_feel_c), not necessarily
# the model intercept at temp_feel_c = 0 (70 F).
# autoscale = FALSE keeps the stated prior scales in original units.
# Normal(mean, SD); exponential(rate) prior for residual SD sigma.

bike_model <- stan_glm(rides ~ temp_feel_c,
                       data = bikes,
                       family = gaussian(),
                       prior_intercept = normal(5000, 1000),  # , autoscale = FALSE
                       prior = normal(100, 40),               # , autoscale = FALSE
                       prior_aux = exponential(0.0008),       # , autoscale = FALSE
                       chains = 4,
                       iter = 10000,
                       seed = 1)

# 3. Diagnose sampling; review warnings, Rhat, effective sample size
bike_model

summary(bike_model)

prior_summary(bike_model)

plot(bike_model)

bike_draws_array <- as.array(bike_model)

mcmc_trace(bike_draws_array, pars = c("(Intercept)", "temp_feel_c", "sigma"), alpha = 0.25)

mcmc_dens_overlay(bike_draws_array, pars = c("(Intercept)", "temp_feel_c", "sigma"))

mcmc_acf(bike_draws_array, pars = c("(Intercept)", "temp_feel_c", "sigma"))

# 4. Describe posterior coefficient uncertainty
draws <- as.matrix(bike_model)

params <- c("(Intercept)", "temp_feel_c", "sigma")

posterior_table <- round(rbind(
  mean = colMeans(draws[, params, drop = FALSE]),
  lower80 = apply(draws[, params, drop = FALSE], 2, quantile, probs = 0.10),
  upper80 = apply(draws[, params, drop = FALSE], 2, quantile, probs = 0.90)
), 1)

posterior_table

c(prob_slope_positive = mean(draws[, "temp_feel_c"] > 0))

# 5. Prediction at 75 F: x_c = 75 - 70 = 5 (NOT 75)
new_day <- data.frame(temp_feel_c = 5)

mu_draws <- as.numeric(posterior_epred(bike_model, newdata = new_day))

# This model is Gaussian with an identity link, so this is equivalent to:
mu_manual <- draws[, "(Intercept)"] + 5 * draws[, "temp_feel_c"]

c(mean_at_75 = mean(mu_draws),
  mean_lower95 = unname(quantile(mu_draws, .025)),
  mean_upper95 = unname(quantile(mu_draws, .975)))

# Simulate NEW day outcomes: posterior predictive includes residual noise.
# posterior_predict simulates the residual noise internally.
set.seed(1)

y_pred <- as.numeric(posterior_predict(bike_model, newdata = new_day))

c(predictive_lower95 = unname(quantile(y_pred, .025)),
  predictive_upper95 = unname(quantile(y_pred, .975)))

# posterior_epred() --> Uncertainty about the mean.
# posterior_predict() --> Uncertainty about a future observation

# Manual simulation (distinct random draws, similar distribution):
set.seed(1)

y_manual <- rnorm(length(mu_manual), mean = mu_manual, sd = draws[, "sigma"])

quantile(y_manual, c(.025, .975))

# 6. Operational event probabilities and decision analysis
capacity <- 5000

(p_exceed <- mean(y_pred > capacity))

(p_below <- mean(y_pred < capacity))

(loss_standard <- 4000 * p_exceed)

(loss_expanded <- 600)

recommended_action <-
  if (loss_standard < loss_expanded) {
    "Standard"
  } else if (loss_standard > loss_expanded) {
    "Expanded"
  } else {
    "Tie"
  }

c(p_exceed_5000 = p_exceed,
  p_below_5000 = p_below,
  loss_standard = loss_standard,
  loss_expanded = loss_expanded)

recommended_action

# 7. Plots
# png("bikes_observed_scatter.png", width = 1100, height = 750, res = 140)
plot(bikes$temp_feel, bikes$rides, pch = 16, col = rgb(0.2, 0.2, 0.2, 0.4),
     xlab = "Feels-like temperature (F)", ylab = "Daily rides",
     main = "Observed bike ridership and temperature")
abline(lm(rides ~ temp_feel, data = bikes), col = "firebrick", lwd = 3)
dev.off()

# png("bikes_posterior_mean_75.png", width = 1100, height = 750, res = 140)
hist(mu_draws, breaks = 40, col = "lightsteelblue", border = "white",
     xlab = "Expected rides at 75 F", main = "Posterior: expected mean at 75 F")
abline(v = quantile(mu_draws, c(.025, .975)), col = "firebrick", lwd = 2, lty = 2)
dev.off()

# png("bikes_predictive_75.png", width = 1100, height = 750, res = 140)
hist(y_pred, breaks = 40, col = "lightsteelblue", border = "white",
     xlab = "Rides on a new 75 F day", main = "Posterior predictive: one new day")
abline(v = 5000, col = "firebrick", lwd = 3, lty = 2)
legend("topright", legend = "Capacity = 5000", col = "firebrick", lty = 2, lwd = 3, bty = "n")
dev.off()

# png("bikes_trace.png", width = 1400, height = 900, res = 140)
print(mcmc_trace(bike_draws_array, pars = params))
dev.off()
