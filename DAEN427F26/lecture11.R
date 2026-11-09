# ==============================================================
# Lecture 11: Evaluating Regression Models
# DAEN 427 / ISEN 427 / ISEN 627 - Fall 2026
# ==============================================================

library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)

# --------------------------------------------------------------
# Data and model: Capital Bikeshare
#   Y = daily rides, X = temp_feel (deg F), n = 500 days
# --------------------------------------------------------------
data(bikes)

bike_model <- stan_glm(rides ~ temp_feel, data = bikes,
  family = gaussian,
  prior_intercept = normal(5000, 1000),
  prior = normal(100, 40),
  prior_aux = exponential(0.0008),
  chains = 4, iter = 5000*2, seed = 84735)

# Posterior draws as a data frame: columns `(Intercept)`, temp_feel, sigma
bike_model_df <- as.data.frame(bike_model)

# ==============================================================
# Q2: How wrong is the model?
# ==============================================================

# Posterior predictive check: compare 50 of the 20,000 simulated datasets to the data
pp_check(bike_model, nreps = 50) + xlab("rides")

# ==============================================================
# RMSE
# ==============================================================

y     <- bikes$rides
y_hat <- colMeans(posterior_predict(bike_model, newdata = bikes, seed = 84735))

sqrt(mean((y - y_hat)^2))

# Python equivalent (for reference):
#   import numpy as np
#   np.sqrt(np.mean((y - y_hat)**2))
#   from sklearn.metrics import root_mean_squared_error

# Bayesian note: RMSE for each posterior draw -> a distribution of RMSE values
mu_draws   <- posterior_linpred(bike_model, newdata = bikes)   # 20000 x 500
rmse_draws <- apply(mu_draws, 1, function(m) sqrt(mean((y - m)^2)))
summary(rmse_draws)

# ==============================================================
# Q3: How accurate are the posterior predictions?
# ==============================================================

# --- One case: October 22, 2012 (temp = 75 F, observed Y = 6228) ---
set.seed(84735)
predict_75 <- bike_model_df %>%
  mutate(mu = `(Intercept)` + temp_feel*75,
         y_new = rnorm(20000, mean = mu, sd = sigma))

predict_75 %>%
  summarize(mean = mean(y_new),
            error = 6228 - mean(y_new),
            sd = sd(y_new),
            error_scaled = error / sd,
            lower_50 = quantile(y_new, 0.25),
            upper_50 = quantile(y_new, 0.75),
            lower_95 = quantile(y_new, 0.025),
            upper_95 = quantile(y_new, 0.975))

# --- Posterior predictive summaries on the training data ---
set.seed(84735)
predictions <- posterior_predict(bike_model, newdata = bikes)
dim(predictions)   # 20000 x 500
ppc_intervals(bikes$rides, yrep = predictions,
              x = bikes$temp_feel, prob = 0.5, prob_outer = 0.95)
# ppc_intervals(): one posterior predictive interval per day (columns of yrep)
#  - dark dot = observed rides; light circle = predicted median
#  - thick band = 50% interval; thin line = 95% interval
#  - calibration: ~50% of dots in thick bands, ~95% in thin lines
#  - Note: constant-width bands vs. data spread growing with temp (non-constant variance);
#          intervals dip below 0 (impossible counts)


prediction_summary(bike_model, data = bikes)
#        mae mae_scaled within_50 within_95
# 1 988.3791  0.7662876     0.438     0.968

# --- 10-fold cross-validation ---
set.seed(84735)

cv_procedure <- prediction_summary_cv(
  model = bike_model, data = bikes, k = 10)

cv_procedure$folds   # metrics per fold
#    fold       mae mae_scaled within_50 within_95
# 1     1  989.9749  0.7701399      0.46      0.98
# 2     2  965.3306  0.7432939      0.42      1.00
# 3     3  949.8875  0.7289988      0.42      0.98
# 4     4 1018.6022  0.7915012      0.46      0.98
# 5     5 1161.6049  0.9090955      0.36      0.96
# 6     6  937.6905  0.7327537      0.46      0.94
# 7     7 1270.5717  1.0057234      0.32      0.96
# 8     8 1112.1833  0.8605382      0.36      1.00
# 9     9 1098.3175  0.8662956      0.40      0.92
# 10   10  786.6327  0.6058450      0.56      0.96

cv_procedure$cv      # averaged estimate
#       mae mae_scaled within_50 within_95
# 1 1029.08  0.8014185     0.422     0.968

# --- Expected log-predictive density (ELPD) ---
model_elpd <- loo(bike_model)   # leave-one-out CV
model_elpd$estimates
#          Estimate      SE
# elpd_loo  -4289.0 13.1186
# p_loo         2.5  0.1638
# looic      8578.1 26.2371

# Comparing two models fit to the same data (larger elpd wins):
# loo_compare(loo(model_1), loo(model_2))

# ==============================================================
# Normal Hierarchical Models
# ==============================================================
# Another generic data frame ...
# Example: Spotify songs (popularity) grouped by artist.
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)

data(spotify)
glimpse(spotify)

dat <- spotify %>%
  select(y = popularity, group = artist) %>%
  mutate(group = factor(group))

# Complete pooling: one mean for everyone
complete_pool <- stan_glm(
  y ~ 1, data = dat, family = gaussian,
  chains = 4, iter = 5000*2, seed = 84735)

# No pooling: separate mean per group
no_pool <- stan_glm(
  y ~ group - 1, data = dat, family = gaussian,
  chains = 4, iter = 5000*2, seed = 84735)

# Partial pooling: hierarchical model with only a group-level intercept
hier_model <- stan_glmer(
  y ~ (1 | group), data = dat, family = gaussian,
  chains = 4, iter = 5000*2, seed = 84735)

# --- Compare the three pooling strategies: group mean estimates ---
groups <- dat %>%
  group_by(group) %>%
  summarize(n_j = n(), ybar_j = mean(y))
new_groups <- data.frame(group = groups$group)

# Posterior mean and 80% interval of each group's mean, per model
summ <- function(model, label) {
  draws <- posterior_linpred(model, newdata = new_groups)   # draws x J
  tibble(group = groups$group, model = label,
         est   = colMeans(draws),
         lower = apply(draws, 2, quantile, 0.10),
         upper = apply(draws, 2, quantile, 0.90))
}

compare_df <- bind_rows(summ(no_pool,    "No pooling"),
                        summ(hier_model, "Partial pooling")) %>%
  left_join(groups, by = "group") %>%
  mutate(group = fct_reorder(group, ybar_j))

global_mean <- mean(as.data.frame(complete_pool)$`(Intercept)`)

# Plot 1: estimates per group; dashed line = complete pooling
ggplot(compare_df, aes(x = group, y = est, color = model)) +
  geom_hline(yintercept = global_mean, linetype = "dashed") +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.6), size = 0.2) +
  coord_flip() +
  labs(x = NULL, y = "Mean popularity (80% interval)", color = NULL,
       title = "Complete (dashed) vs. no vs. partial pooling")

# Plot 2: shrinkage. Points off the 45-degree line are pulled toward
# the global mean; groups with small n_j are pulled the most
shrink_df <- compare_df %>%
  select(group, model, est, n_j) %>%
  pivot_wider(names_from = model, values_from = est)

ggplot(shrink_df, aes(x = `No pooling`, y = `Partial pooling`, size = n_j)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = global_mean, linetype = "dashed") +
  geom_point(alpha = 0.6) +
  labs(size = "Songs per artist",
       title = "Shrinkage toward the global mean")

# --- Step 1: Is it wrong? Posterior predictive check ---
pp_check(hier_model, nreps = 50)

# Where does the variability live? sigma_y vs sigma_mu and the ICC
hier_df  <- as.data.frame(hier_model)
sigma_y  <- hier_df$sigma
sigma_mu <- sqrt(hier_df$`Sigma[group:(Intercept),(Intercept)]`)
icc      <- sigma_mu^2 / (sigma_mu^2 + sigma_y^2)
summary(icc)

# --- Step 2: Accuracy ---
# In-sample summaries (existing groups)
set.seed(84735)
prediction_summary(hier_model, data = dat)

# New groups: hold out whole groups at a time
prediction_summary_cv(model = hier_model, data = dat,
                      group = "group", k = 10)

# ELPD (LOO) to compare pooling strategies
loo_compare(loo(complete_pool), loo(no_pool), loo(hier_model))

# --- Shrinkage worked example: three classrooms ---
mu_global <- 75
sig_y     <- 10
sig_mu    <- 5
classrooms <- tibble(classroom = c("A", "B", "C"),
                     n_j = c(4, 25, 100),
                     ybar_j = 90) %>%
  mutate(w_j = (n_j / sig_y^2) / (n_j / sig_y^2 + 1 / sig_mu^2),
         mu_hat_j = w_j * ybar_j + (1 - w_j) * mu_global)
classrooms
