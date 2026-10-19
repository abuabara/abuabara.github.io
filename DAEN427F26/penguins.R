# PACKAGES
library(tidyverse)
library(bayesrules)
library(rstanarm)
library(bayesplot)

# DATA
data(bikes)

glimpse(bikes)

summary(bikes[, c("rides", "temp_feel")])

nrow(bikes)

lm(rides ~ temp_feel,
   data = bikes)

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

############################################################
############################################################
############################################################

# PENGUINS
data("penguins_bayes", package = "bayesrules")

penguins <- penguins_bayes

table(penguins$year, penguins$species)

# Data exploration
dim(penguins)
str(penguins)
glimpse(penguins_bayes)

penguins %>%
  count(species)

penguins %>%
  group_by(species) %>%
  summarise(
    n = n(),
    flipper_mean = mean(flipper_length_mm, na.rm = TRUE),
    bill_mean = mean(bill_length_mm, na.rm = TRUE),
    mass_mean = mean(body_mass_g, na.rm = TRUE),
    .groups = "drop"
  )

# NORMAL-NORMAL CONJUGATE POSTERIOR
# Prior parameters
mu0 <- 190
prior_sd <- 10

# Known population standard deviation
sigma_known <- 7

# Observed data
# Select Adelie flipper lengths
y <- na.omit(
  penguins$flipper_length_mm[
    penguins$species == "Adelie"
  ]
)

# Alternatively:
# y <- penguins %>%
#   filter(species == "Adelie") %>%
#   pull(flipper_length_mm) %>%
#   na.omit() %>%
#   as.numeric()

n <- length(y)
y_bar <- mean(y)

# Posterior variance
post_var <- 1 / (
  1 / prior_sd^2 + n / sigma_known^2
)

# Posterior mean
post_mean <- post_var * (
  mu0 / prior_sd^2 +
    n * y_bar / sigma_known^2
)

# Posterior standard deviation
post_sd <- sqrt(post_var)

# 95% credible interval
credible_interval <- qnorm(
  c(0.025, 0.975),
  mean = post_mean,
  sd = post_sd
)

# Results
post_mean
post_var
post_sd
credible_interval

summarize_normal_normal(
  mean = mu0,
  sd = prior_sd,
  sigma = sigma_known,
  y_bar = y_bar,
  n = n
)

# Random generation for the normal distribution
post_mu <- rnorm(
  10000,
  mean = post_mean,
  sd = post_sd
)

ggplot(data.frame(mu = post_mu), aes(mu)) +
  geom_density(fill = "steelblue", alpha = .4) +
  labs(
    title = "Posterior: mean Adelie flipper length",
    x = "Mean length (mm)"
  )

# SIMPLE BAYESIAN NORMAL REGRESSION
reg9 <- penguins %>%
  # filter(sex == "male") %>%
  select(
    flipper_length_mm,
    bill_length_mm
  ) %>%
  drop_na()

head(reg9)

hist(reg9$flipper_length_mm)

ggplot(
  reg9,
  aes(bill_length_mm, flipper_length_mm)
) +
  geom_point(alpha = .65) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Observed bill–flipper relationship"
  )

# Center predictor so prior intercept is the expected
# flipper length at the average bill length, rather
# than at an unrealistic bill length of 0

bill_reference <- mean(reg9$bill_length_mm)

reg9 <- reg9 %>%
  mutate(
    bill_centered = bill_length_mm - bill_reference
  )

ggplot(
  reg9,
  aes(bill_centered, flipper_length_mm)
) +
  geom_point(alpha = .65) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Observed bill–flipper relationship"
  )

# BAYESIAN REGRESSION USING RSTANARM
# Note: This is a posterior model, not a prior-only model
#
# autoscale = FALSE keeps the specified prior scales
# rather than automatically adjusting them

fit9_prior_stan <- stan_glm(
  flipper_length_mm ~ bill_centered,
  data = reg9,
  family = gaussian(),
  prior_intercept = normal(
    190, 10, autoscale = FALSE
  ),
  prior = normal(
    0, 5, autoscale = FALSE
  ),
  prior_aux = exponential(
    0.1, autoscale = FALSE
  ),
  chains = 4,
  iter = 10000,
  seed = 84735
)

prior_summary(fit9_prior_stan)

# Update the rstanarm model
# Without changed arguments, this refits the model
# using its existing specification

fit9 <- update(fit9_prior_stan)

summary(fit9)

posterior_interval(fit9, prob = .95)

pp_check(fit9)

plot(fit9, plotfun = "trace")

# BAYESIAN REGRESSION USING BRMS
library(brms) # https://github.com/paul-buerkner/brms

# This model is explicitly prior-only
# It does not condition its parameter draws on the
# observed flipper lengths

fit9_prior <- brm(
  formula = flipper_length_mm ~ bill_centered,
  data = reg9,
  family = gaussian(),
  prior = c(
    prior(
      normal(190, 10),
      class = "Intercept"
    ),
    prior(
      normal(0, 5),
      class = "b"
    ),
    prior(
      exponential(0.1),
      class = "sigma"
    )
  ),
  sample_prior = "only",
  chains = 4,
  iter = 10000,
  seed = 84735
)

summary(fit9_prior)

plot(fit9_prior)

# BRMS POSTERIOR MODEL
# Fit the same model using observed data
# Unlike fit9_prior, this returns posterior draws

fit9_brms <- update(
  fit9_prior,
  sample_prior = "no",
  seed = 84735
)

summary(fit9_brms)

plot(fit9_brms)

# Posterior credible intervals
posterior_interval(fit9_brms, prob = .95)

# Posterior predictive check
pp_check(fit9_brms)

# Plot 50 prior linear predictions
# (without observation-level noise)

prior_draws <- as.matrix(fit9_prior)

# Check actual names of the brms parameters
colnames(prior_draws)

# In brms, the expected parameter names include:
# b_Intercept
# b_bill_centered
# sigma

# Set seed for reproducible selection of prior draws
set.seed(84735)

prior_draws <- prior_draws[
  sample(
    seq_len(nrow(prior_draws)),
    50
  ),
  ,
  drop = FALSE
]

x_seq <- seq(
  min(reg9$bill_centered),
  max(reg9$bill_centered),
  length.out = 100
)

# ALTERNATIVE CODE
# prior_lines <-
#   # Combine all results
#   do.call(
#   rbind,
#   # Iterate through the prior draws
#   lapply(seq_len(nrow(prior_draws)), function(i) {
#     data.frame(
#       draw = i,
#       # Construct the original bill lengths
#       bill_length_mm = x_seq + bill_reference,
#       # Calculate each regression line
#       expected_flipper =
#         prior_draws[i, "(Intercept)"] +
#         prior_draws[i, "bill_centered"] * x_seq
#     )
#   })
# )

# If using the alternative code above with brms, replace:
# "(Intercept)"  with "b_Intercept"
# "bill_centered" with "b_bill_centered"

# PRIOR REGRESSION LINES USING TIDYR
library(tidyr)

prior_lines <-
  as.data.frame(prior_draws) %>%
  mutate(draw = row_number()) %>%
  crossing(x_seq = x_seq) %>%
  mutate(
    bill_length_mm = x_seq + bill_reference,
    
    # Use brms parameter names
    expected_flipper =
      .data[["b_Intercept"]] +
      .data[["b_bill_centered"]] * x_seq
  ) %>%
  select(
    draw,
    bill_length_mm,
    expected_flipper
  )

ggplot(
  prior_lines,
  aes(
    bill_length_mm,
    expected_flipper,
    group = draw
  )
) +
  geom_line(alpha = .1) +
  labs(
    title = "50 simulated prior regression lines",
    x = "Bill length (mm)",
    y = "Expected flipper length (mm)"
  )

# PRIOR PREDICTIVE SIMULATIONS
# Four datasets simulated from the model
# prior predictive distribution

# This now uses a genuine prior-only brms model
prior_yrep <- posterior_predict(
  fit9_prior,
  ndraws = 4
)

prior_datasets <- do.call(
  rbind,
  lapply(
    seq_len(nrow(prior_yrep)),
    function(i) {
      data.frame(
        simulation = factor(i),
        bill_length_mm = reg9$bill_length_mm,
        flipper_length_mm = prior_yrep[i, ]
      )
    }
  )
)

ggplot(
  prior_datasets,
  aes(bill_length_mm, flipper_length_mm)
) +
  geom_point(alpha = .4) +
  facet_wrap(~simulation) +
  labs(
    title = "Four prior-predictive datasets",
    x = "Bill length (mm)",
    y = "Simulated flipper length (mm)"
  )

# PREDICTIONS
# Predicted flipper length for a penguin with a 45 mm bill

new9 <- data.frame(
  bill_centered = 45 - bill_reference
)

# rstanarm results

# 95% credible interval for expected mean
posterior_interval(
  fit9,
  newdata = new9,
  prob = .95
)

# 95% posterior predictive interval for an individual penguin
predictive_interval(
  fit9,
  newdata = new9,
  prob = .95
)

# EQUIVALENT PREDICTIONS USING BRMS
# brms predictions for comparison with rstanarm

# Expected mean flipper length (epred)
brms_mean_prediction <- posterior_epred(
  fit9_brms,
  newdata = new9
)

quantile(
  as.vector(brms_mean_prediction),
  probs = c(0.025, 0.975)
)

# Actual future flipper length (predictive)
brms_individual_prediction <- posterior_predict(
  fit9_brms,
  newdata = new9
)

quantile(
  as.vector(brms_individual_prediction),
  probs = c(0.025, 0.975)
)

# COMPARE RSTANARM AND BRMS
# Compare posterior parameter summaries
summary(fit9)
summary(fit9_brms)

# Compare posterior credible intervals
posterior_interval(fit9, prob = .95)
posterior_interval(fit9_brms, prob = .95)

# Compare fitted relationships visually
pp_check(fit9)
pp_check(fit9_brms)
