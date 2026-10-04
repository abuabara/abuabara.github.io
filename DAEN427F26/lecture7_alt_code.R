# ------------------------------------------------------------------
# Bayes Rules! Chapter 6 -- Approximating the posterior
# Risk & decision analysis example: failure probability of a component
# Prior: pi ~ Beta(2, 8);  Data: Y = 5 failures in n = 10 tests
# Exact posterior (conjugate): Beta(7, 13)
# ------------------------------------------------------------------
library(ggplot2)
library(rstan)      # MCMC engine
library(bayesplot)  # mcmc_trace(), mcmc_dens_overlay(), mcmc_acf(), ...

set.seed(84735)

# ---------- PART 1: GRID APPROXIMATION (Beta-Binomial) -------------
grid_post <- function(m) {
  pi_grid      <- seq(0, 1, length = m)
  prior        <- dbeta(pi_grid, 2, 8)
  likelihood   <- dbinom(5, size = 10, prob = pi_grid)
  unnormalized <- prior * likelihood
  data.frame(pi_grid, prior, likelihood, unnormalized,
             posterior = unnormalized / sum(unnormalized))
}

grid6   <- grid_post(6)
round(grid6, 3)                     # the table on the slides

grid101 <- grid_post(101)
draws   <- sample(grid101$pi_grid, size = 10000, replace = TRUE,
                  prob = grid101$posterior)

# compare with the exact posterior
ggplot(data.frame(pi = draws), aes(pi)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "tomato", alpha = 0.6) +
  stat_function(fun = dbeta, args = list(shape1 = 7, shape2 = 13),
                color = "darkgreen", linewidth = 1)

# ---------- Risk & decision from the draws -------------------------
mean(draws)                         # posterior mean failure prob.
mean(draws > 0.3)                   # P(pi > 0.3 | data)
quantile(draws, c(0.025, 0.975))    # 95% credible interval

cost_fail <- 100; cost_repl <- 30
c(keep = cost_fail * mean(draws), replace = cost_repl)

# ---------- Gamma-Poisson grid (failures per month) ----------------
y        <- c(2, 4, 3)
lam_grid <- seq(0, 12, length = 200)
prior    <- dgamma(lam_grid, shape = 3, rate = 1)
lik      <- sapply(lam_grid, function(l) prod(dpois(y, l)))
post     <- prior * lik / sum(prior * lik)
lam_draws <- sample(lam_grid, 10000, replace = TRUE, prob = post)
mean(lam_draws)                     # exact: Gamma(12, 4) has mean 3

# ---------- PART 2: METROPOLIS (random-walk MCMC) ------------------
target <- function(pi) {            # prior x likelihood (unnormalized)
  if (pi < 0 || pi > 1) return(0)
  dbeta(pi, 2, 8) * dbinom(5, 10, pi)
}

metropolis <- function(N, w, start) {
  chain <- numeric(N)
  current <- start
  for (i in 1:N) {
    proposal <- runif(1, current - w, current + w)
    alpha    <- min(1, target(proposal) / target(current))
    if (runif(1) < alpha) current <- proposal
    chain[i] <- current
  }
  chain
}

chain <- metropolis(N = 5000, w = 0.2, start = 0.5)
plot(chain[1:1000], type = "l", xlab = "iteration", ylab = "pi")
hist(chain, breaks = 40, freq = FALSE)
curve(dbeta(x, 7, 13), add = TRUE, lwd = 2)

# ---------- PART 3: MCMC WITH rstan --------------------------------
# DEFINE
bb_model <- "
  data {
    int<lower = 0, upper = 10> Y;
  }
  parameters {
    real<lower = 0, upper = 1> pi;
  }
  model {
    Y  ~ binomial(10, pi);
    pi ~ beta(2, 8);
  }
"
# SIMULATE: 4 chains x 5000 iterations (first 2500 = warm-up, discarded)
bb_sim <- stan(model_code = bb_model, data = list(Y = 5),
               chains = 4, iter = 5000 * 2, seed = 84735)

# ---------- DIAGNOSE -----------------------------------------------
mcmc_trace(bb_sim, pars = "pi", size = 0.1)
mcmc_dens_overlay(bb_sim, pars = "pi")
mcmc_acf(bb_sim, pars = "pi")
neff_ratio(bb_sim, pars = "pi")     # want > 0.1
rhat(bb_sim, pars = "pi")           # want < 1.05

# ---------- USE THE DRAWS ------------------------------------------
pi_draws <- as.data.frame(bb_sim, pars = "pi")$pi
mean(pi_draws > 0.3)
quantile(pi_draws, c(0.025, 0.975))
