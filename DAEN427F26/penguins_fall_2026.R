# PACKAGES
library(tidyverse)
library(ggplot2)
library(dplyr)

library(bayesrules)
library(rstanarm)
library(bayesplot)

# DATA
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
    bill_length_mm,
    species
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





fit9_cond_stan <- rstanarm::stan_glmer(
  #flipper_length_mm ~ bill_centered | species,
  flipper_length_mm ~ bill_centered + (1 | species),
  data = reg9,
  family = gaussian(),
  prior_intercept = rstanarm::normal(190, 10, autoscale = FALSE),
  prior = rstanarm::normal(0, 5, autoscale = FALSE),
  prior_aux = rstanarm::exponential(0.1, autoscale = FALSE),
  chains = 4,
  iter = 10000,
  seed = 84735)

summary(fit9_cond_stan)

posterior_interval(fit9_cond_stan, prob = .95)

pp_check(fit9_cond_stan)

plot(fit9_cond_stan, plotfun = "trace")

# Prediction grid for each species
newdata <- reg9 %>%
  group_by(species) %>%
  summarise(
    bill_centered = list(seq(
      min(bill_centered),
      max(bill_centered),
      length.out = 100
    )),
    .groups = "drop"
  ) %>%
  tidyr::unnest(bill_centered)

# Posterior expected predictions
post <- posterior_epred(
  fit9_cond_stan,
  newdata = newdata,
  re.form = NULL
)

# Posterior summaries
newdata$estimate <- apply(post, 2, median)
newdata$lower    <- apply(post, 2, quantile, probs = 0.025)
newdata$upper    <- apply(post, 2, quantile, probs = 0.975)

# Regression visualization
ggplot(reg9, aes(x = bill_centered,
                 y = flipper_length_mm,
                 color = species)) +
  geom_point(alpha = 0.45) +
  geom_ribbon(
    data = newdata,
    aes(x = bill_centered,
        ymin = lower,
        ymax = upper,
        fill = species),
    inherit.aes = FALSE,
    alpha = 0.15,
    color = NA) +
  geom_line(data = newdata,
            aes(x = bill_centered, y = estimate, color = species),
            linewidth = 1.2) +
  labs(title = "Bayesian Hierarchical Regression by Species",
       x = "Centered Bill Length (mm)",
       y = "Flipper Length (mm)",
       color = "Species",
       fill = "Species") + theme_minimal(base_size = 13)
