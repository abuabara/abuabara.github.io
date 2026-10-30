# BAYES RULES! — CHAPTER 17
# Normal Hierarchical Models with Predictors

# ============================================================
# 0. PACKAGES AND DATA
# ============================================================

# Install packages if necessary
# install.packages(c(
#   "bayesrules", "dplyr", "ggplot2", "rstanarm", "tidyverse"
#   "broom.mixed", "bayesplot", "loo"
# ))

library(bayesrules)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstanarm)
library(broom.mixed)
library(bayesplot)
library(loo)

# set.seed(84735)

data(cherry_blossom_sample)

running <- cherry_blossom_sample |>
  select(runner, age, net) |>
  na.omit() |>
  mutate(runner = factor(runner))

# Explore the dataset
head(running)
str(running)
summary(running)

# Number of observations and runners
nrow(running)
n_distinct(running$runner)

# Number of races per runner
running |>
  count(runner) |>
  arrange(desc(n))

# ============================================================
# 1. EVALUATE — EXPLORE THE DATA
# ============================================================

# Visualize the relationship between age and race time
ggplot(running, aes(x = age, y = net)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Age and Running Time",
    x = "Age (years)",
    y = "Net Race Time (minutes)"
  ) +
  theme_minimal()

# Examine each runner separately
ggplot(running, aes(x = age, y = net)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~runner) +
  labs(
    title = "Running Times by Runner",
    x = "Age (years)",
    y = "Net Race Time (minutes)"
  ) +
  theme_minimal()

# Fit a complete-pooling regression
m_pool <- stan_glm(
  net ~ age,
  data = running,
  family = gaussian(),
  chains = 2,
  iter = 1000,
  seed = 84735,
  refresh = 0
)

print(m_pool)

# Examine posterior estimates
posterior_interval(
  m_pool,
  prob = 0.80
)

# ============================================================
# 2. APPLY — VARYING-INTERCEPT MODEL
# ============================================================

# Each runner has a different intercept.
# The age slope is shared across runners.

m_int <- stan_glmer(
  net ~ age + (1 | runner),
  data = running,
  family = gaussian(),
  
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1),
  
  prior_aux = exponential(
    1,
    autoscale = TRUE
  ),
  
  prior_covariance = decov(
    reg = 1,
    conc = 1,
    shape = 1,
    scale = 1
  ),
  
  chains = 2,
  iter = 1000,
  seed = 84735,
  refresh = 0
)

print(m_int)

# Fixed effects
tidy(
  m_int,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.80
)

# Group-level variability
tidy(
  m_int,
  effects = "ran_pars"
)

# Runner-specific random effects
head(ranef(m_int)$runner)

# Runner-specific coefficients
head(coef(m_int)$runner)

# Posterior intervals
posterior_interval(
  m_int,
  prob = 0.80
)

# ============================================================
# 3. UNDERSTAND — PRIOR PREDICTIVE CHECKS
# ============================================================

# Simulate from the prior predictive distribution
m_prior <- update(
  m_int,
  prior_PD = TRUE,
  chains = 2,
  iter = 1000,
  seed = 84735,
  refresh = 0
)

# Prior predictive distribution
pp_check(
  m_prior,
  plotfun = "dens_overlay",
  nsamples = 30
)

# Posterior predictive distribution
pp_check(
  m_int,
  plotfun = "dens_overlay",
  nsamples = 30
)

# Questions:
# 1. Are the simulated race times plausible?
# 2. Are the priors too restrictive or too diffuse?
# 3. Does the fitted model reproduce the observed data?

# ============================================================
# 4. EVALUATE — VARYING-INTERCEPT AND VARYING-SLOPE MODEL
# ============================================================

# Each runner can have:
# - Their own intercept
# - Their own age slope

m_slope <- stan_glmer(
  net ~ age + (age | runner),
  data = running,
  family = gaussian(),
  
  prior_intercept = normal(100, 10),
  prior = normal(2.5, 1),
  
  prior_aux = exponential(
    1,
    autoscale = TRUE
  ),
  
  prior_covariance = decov(
    reg = 1,
    conc = 1,
    shape = 1,
    scale = 1
  ),
  
  chains = 2,
  iter = 1000,
  seed = 84735,
  adapt_delta = 0.99,
  refresh = 0
)

print(m_slope)

# Examine runner-specific coefficients
head(coef(m_slope)$runner)

# Fixed effects
tidy(
  m_slope,
  effects = "fixed",
  conf.int = TRUE,
  conf.level = 0.80
)

# Group-level parameters
tidy(
  m_slope,
  effects = "ran_pars"
)

# ============================================================
# 5. EVALUATE — MCMC DIAGNOSTICS
# ============================================================

# Trace plot of the global age coefficient
bayesplot::mcmc_trace(
  as.array(m_slope),
  pars = "age"
)

# MCMC diagnostics
summary(m_slope)$stan_summary |>
  head()

# Look for:
# - R-hat close to 1
# - Adequate effective sample sizes
# - No divergent transitions
# - Well-mixed chains

# ============================================================
# 6. EVALUATE — POSTERIOR PREDICTIVE CHECKS
# ============================================================

# Varying-intercept model
pp_check(
  m_int,
  nsamples = 40
)

# Varying-intercept and varying-slope model
pp_check(
  m_slope,
  nsamples = 40
)

# Compare posterior predictive distributions
pp_check(
  m_int,
  plotfun = "dens_overlay",
  nsamples = 40
)

pp_check(
  m_slope,
  plotfun = "dens_overlay",
  nsamples = 40
)

# ============================================================
# 7. EVALUATE — MODEL COMPARISON
# ============================================================

# Leave-one-observation-out cross-validation
loo_int <- loo(m_int)

loo_slope <- loo(m_slope)

# Compare expected predictive performance
loo_compare(
  loo_int,
  loo_slope
)

# Note:
# These LOO calculations concern individual observations.
# To evaluate prediction for entirely new runners,
# use cross-validation that holds out whole runners.

# ============================================================
# 8. CREATE — POSTERIOR PREDICTIONS
# ============================================================

# Select two runners from the dataset
runner_ids <- levels(running$runner)

# Use the first two existing runners
existing_runners <- runner_ids[1:2]

# Predict race times at age 61
new_races <- data.frame(
  runner = factor(
    c(existing_runners, "Miles"),
    levels = c(runner_ids, "Miles")
  ),
  age = c(61, 61, 61)
)

print(new_races)

# Posterior predictive draws
# Miles represents a previously unobserved runner.

yrep <- posterior_predict(
  m_int,
  newdata = new_races,
  allow_new_levels = TRUE,
  sample_new_levels = "gaussian"
)

# Median predicted race times
apply(
  yrep,
  2,
  median
)

# 80% posterior predictive intervals
apply(
  yrep,
  2,
  quantile,
  probs = c(0.10, 0.90)
)

# Summarize predictions
prediction_summary <- data.frame(
  runner = as.character(new_races$runner),
  age = new_races$age,
  median = apply(yrep, 2, median),
  lower = apply(yrep, 2, quantile, probs = 0.10),
  upper = apply(yrep, 2, quantile, probs = 0.90)
)

print(prediction_summary)

# Visualize posterior predictions
ggplot(
  prediction_summary,
  aes(x = runner, y = median)
) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15
  ) +
  labs(
    title = "Predicted Race Times at Age 61",
    subtitle = "80% Posterior Predictive Intervals",
    x = "Runner",
    y = "Race Time (minutes)"
  ) +
  theme_minimal()

# Possible questions:
# 1. Why is complete pooling potentially misleading?
# 2. What does partial pooling accomplish?
# 3. When are varying slopes useful?
# 4. Which model has stronger predictive support?
# 5. Why are predictions for new runners more uncertain?
# 6. Which model would you recommend, and why?
