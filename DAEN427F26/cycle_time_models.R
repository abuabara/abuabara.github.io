# Machine cycle times: simple, multiple and hierarchical models
#   1. Simulate a synthetic dataset (runs within machines within plants)
#   2. Simple linear model           cycle_time ~ hours
#   3. Multiple linear model         cycle_time ~ hours + shift + plant
#   4. Bayesian hierarchical model   cycle_time ~ hours + shift + (1 | machine)
#   5. Side-by-side summary table    (modelsummary)

# install.packages(c("rstanarm", "modelsummary", "broom.mixed"))
library(rstanarm)
library(modelsummary)
library(broom.mixed)

# 1. Synthetic dataset
set.seed(2026)

# True values used to simulate
mu         <- 60    # average cycle time (s) right after maintenance
sd_mach    <- 4     # between-machine SD of baselines
beta_hr    <- 0.05  # +0.05 s per hour since maintenance (tool wear)
sd_slope   <- 0.02  # machines wear at slightly different rates
beta_night <- 1.5   # night shift adds 1.5 s
sigma_true <- 2     # run-to-run noise within a machine

# Machine level
J <- 24
machines <- data.frame(
  machine = sprintf("M%02d", 1:J),
  plant   = rep(c("Plant A", "Plant B", "Plant C"), each = J / 3),
  b0      = rnorm(J, 0, sd_mach),
  b1      = rnorm(J, 0, sd_slope),
  n_runs  = sample(3:20, J, replace = TRUE)   # unbalanced group sizes
)

# Run level
cycle <- machines[rep(1:J, machines$n_runs), 
                  c("machine", "plant", "b0", "b1")]
N <- nrow(cycle)
cycle$hours_since_maint <- round(runif(N, 0, 200), 1)
cycle$shift <- factor(sample(c("Day", "Night"), N, replace = TRUE),
                      levels = c("Day", "Night"))
cycle$cycle_time <- round(
  mu + 
    cycle$b0 + 
    (beta_hr + cycle$b1) * 
    cycle$hours_since_maint + 
    beta_night * (cycle$shift == "Night") +
  rnorm(N, 0, sigma_true), 2)

# Keep only observable columns
cycle <- cycle[, c("machine", "plant", "shift", "hours_since_maint", "cycle_time")]
cycle$machine <- factor(cycle$machine)
cycle$plant   <- factor(cycle$plant)
rownames(cycle) <- NULL

str(cycle)
table(cycle$machine)   # runs per machine
# write.csv(cycle, "machine_cycle_times.csv", row.names = FALSE)

boxplot(cycle_time ~ machine, data = cycle, las = 2,
        ylab = "Cycle time (s)", main = "Cycle time by machine")

# 2. Simple linear model (complete pooling)
m_lm <- lm(cycle_time ~ hours_since_maint, data = cycle)
summary(m_lm)

plot(cycle_time ~ hours_since_maint, data = cycle, pch = 19, col = "grey50",
     xlab = "Hours since maintenance", ylab = "Cycle time (s)",
     main = "Simple linear model")
abline(m_lm, col = "blue", lwd = 2)

# 3. Multiple linear model (still not hierarchical)
m_multi <- lm(cycle_time ~ hours_since_maint + shift + plant, data = cycle)
summary(m_multi)
anova(m_lm, m_multi)

# Residuals by machine: structure the lm models leave behind
boxplot(resid(m_multi) ~ cycle$machine, las = 2,
        ylab = "Residual (s)", main = "Multiple model residuals by machine")
abline(h = 0, lty = 2)

# 4. Bayesian hierarchical model (varying intercepts by machine)
m_hier <- stan_glmer(
  cycle_time ~ hours_since_maint + shift + (1 | machine),
  data = cycle, family = gaussian,
  prior_intercept  = normal(0, 2.5, autoscale = TRUE),
  prior            = normal(0, 2.5, autoscale = TRUE),
  prior_aux        = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 4000, seed = 2026, refresh = 0)

print(m_hier, digits = 2)
coef(m_hier)$machine                  # each machine's baseline
plot(m_hier, regex_pars = "^b\\[")    # machine effects (shrinkage)

# 5. Side-by-side summary table
draws      <- as.matrix(m_hier)
sd_machine <- median(sqrt(draws[, "Sigma[machine:(Intercept),(Intercept)]"]))
sd_within  <- median(draws[, "sigma"])

extra <- data.frame(
  term         = c("SD between machines", "SD within machine"),
  Simple       = c("", sprintf("%.2f", sigma(m_lm))),
  Multiple     = c("", sprintf("%.2f", sigma(m_multi))),
  Hierarchical = c(sprintf("%.2f", sd_machine), sprintf("%.2f", sd_within))
)

models <- list("Simple" = m_lm, "Multiple" = m_multi, "Hierarchical" = m_hier)

omit <- "^b\\[|Sigma|sigma"

modelsummary(models,
             statistic   = "conf.int", conf_level = 0.95,
             coef_omit   = omit, add_rows    = extra,
             coef_rename = c(hours_since_maint = "Hours since maintenance",
                             shiftNight = "Night shift"),
             gof_map     = c("nobs", "rmse"),
             title       = "Cycle time models (95% intervals)",
             output      = "huxtable")

# 6. Which one is the best?
m_lin  <- stan_glm(cycle_time ~ hours_since_maint + shift,
                   data = cycle, seed = 2026, refresh = 0)
m_cat  <- stan_glm(cycle_time ~ hours_since_maint + shift + machine,
                   data = cycle, seed = 2026, refresh = 0)
m_hier <- stan_glmer(cycle_time ~ hours_since_maint + shift + (1 | machine),
                     data = cycle, seed = 2026, refresh = 0)

loo_compare(loo(m_lin), loo(m_cat), loo(m_hier))

#  .
#  .
#  .
#  .
# 7. Wrong hierarchy
m_hier_2 <- stan_glmer(
  cycle_time ~ hours_since_maint + (shift | machine),
  data = cycle, family = gaussian,
  prior_intercept  = normal(0, 2.5, autoscale = TRUE),
  prior            = normal(0, 2.5, autoscale = TRUE),
  prior_aux        = exponential(1, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 4000, seed = 2026, refresh = 0)

print(m_hier_2, digits = 2)
coef(m_hier_2)$machine                  # each machine's baseline
plot(m_hier_2, regex_pars = "^b\\[")    # machine effects (shrinkage)

# posterior median of an SD from a variance parameter
sd_med <- function(m, par) {
  d <- as.matrix(m)
  if (par %in% colnames(d)) sprintf("%.2f", median(sqrt(d[, par]))) else ""
}

extra <- data.frame(
  term = c("SD machine intercept", "SD machine night-shift effect",
           "SD within machine"),
  `Varying intercept` = c(
    sd_med(m_hier,   "Sigma[machine:(Intercept),(Intercept)]"),
    sd_med(m_hier,   "Sigma[machine:shiftNight,shiftNight]"),
    sprintf("%.2f", median(as.matrix(m_hier)[, "sigma"]))),
  `Varying shift` = c(
    sd_med(m_hier_2, "Sigma[machine:(Intercept),(Intercept)]"),
    sd_med(m_hier_2, "Sigma[machine:shiftNight,shiftNight]"),
    sprintf("%.2f", median(as.matrix(m_hier_2)[, "sigma"]))),
  check.names = FALSE
)

# Side-by-side table
modelsummary(
  list("Varying intercept" = m_hier, "Varying shift" = m_hier_2),
  statistic = "conf.int", conf_level = 0.95,
  coef_omit = "^b\\[|Sigma|sigma",
  add_rows  = extra,
  gof_map   = c("nobs", "rmse"))

# 2. Predictive comparison (ELPD, higher is better)
loo_compare(loo(m_hier), loo(m_hier_2))

#    model elpd_diff se_diff p_worse       diag_diff diag_elpd
#   m_hier       0.0     0.0      NA                          
# m_hier_2      -2.7     4.9    0.71 |elpd_diff| < 4

# The extra structure isn’t real. In the simulation every 
# machine has the same 1.5 s night penalty. 
# So m_hier_2’s machine-specific shift effects have
# nothing true to find. The only differences between 
# machines’ night penalties in the data are random noise.

# LOO measures exactly this. Leave-one-out cross-validation 
# checks how well each model predicts a run it didn't see.

# Hierarchical shrinkage limits the damage. The machine 
# shift effects come from a common distribution.