###################
# 1. Define prior parameters: Beta(2, 2)
alpha_prior <- 2
beta_prior  <- 2

# 2. Enter observed data: 7 heads in 10 flips
n_flips <- 10
heads <- 7
tails <- n_flips - heads

# 3. Compute posterior parameters: Beta(9, 5)
alpha_posterior <- alpha_prior + heads
beta_posterior  <- beta_prior + tails

# 4. Create theta values
x <- seq(0, 1, length.out = 500)

# Prior density
prior_curve <- dbeta(
  x,
  shape1 = alpha_prior,
  shape2 = beta_prior
)

# Binomial likelihood as a function of theta
likelihood_curve <- dbinom(
  heads,
  size = n_flips,
  prob = x
)

# Normalize likelihood for comparison with the densities
dx <- x[2] - x[1]
likelihood_curve <- likelihood_curve /
  sum(likelihood_curve * dx)

# Posterior density
posterior_curve <- dbeta(
  x,
  shape1 = alpha_posterior,
  shape2 = beta_posterior
)

# 5. Plot all three curves
y_max <- max(prior_curve, likelihood_curve, posterior_curve)

plot(
  x, prior_curve,
  type = "l",
  col = "red",
  lwd = 3,
  lty = 2,
  ylim = c(0, y_max),
  main = "Bayesian Update",
  xlab = expression("Probability of Heads (" * theta * ")"),
  ylab = "Normalized Density"
)

lines(
  x, likelihood_curve,
  col = "darkgreen",
  lwd = 3,
  lty = 3
)

lines(
  x, posterior_curve,
  col = "blue",
  lwd = 3,
  lty = 1
)

legend(
  "topleft",
  legend = c(
    "Prior: Beta(2, 2)",
    "Likelihood: 7 heads in 10 flips",
    "Posterior: Beta(9, 5)"
  ),
  col = c("red", "darkgreen", "blue"),
  lty = c(2, 3, 1),
  lwd = 3,
  bty = "n"
)

alpha <- 45; beta <- 55
y <- 30; n <- 50

a_post <- alpha + y
b_post <- beta + n - y

post_mean <- a_post / (a_post + b_post)
p_viable  <- 1 - pbeta(.50, a_post, b_post)
ci_95     <- qbeta(c(.025, .975), a_post, b_post)

# posterior predictive simulation for m = 100
pi_draw <- rbeta(100000, a_post, b_post)
y_new   <- rbinom(100000, 100, pi_draw)
y_new


###################
x <- seq(0, 1, length.out = 500)

beta1 <- dbeta(
  x,
  shape1 = 3,
  shape2 = 2
)

beta2 <- dbeta(
  x,
  shape1 = 30,
  shape2 = 20
)

mean1 <- 3 / (3 + 2)
mean2 <- 30 / (30 + 20)

3 / (3 + 2)
30 / (30 + 20)
300 / (300 + 200)

plot(
  x, beta1,
  type = "l",
  lwd = 2,
  col = "steelblue",
  xlab = expression("" * theta * ""),
  ylab = "Density",
  main = "Comparison of Beta Distributions",
  ylim = range(c(beta1, beta2))
)

lines(x, beta2, lwd = 2, col = "firebrick")

# Vertical mean lines
abline(v = mean1, col = "steelblue", lwd = 2, lty = 2)
abline(v = mean2, col = "firebrick", lwd = 2, lty = 3)

legend(
  "topleft",
  legend = c(
    "Beta(3, 2)",
    "Beta(30, 20)",
    sprintf("Mean Beta(3, 2) = %.2f", mean1),
    sprintf("Mean Beta(30, 20) = %.2f", mean2)
  ),
  col = c("steelblue", "firebrick", "steelblue", "firebrick"),
  lwd = 2,
  lty = c(1, 1, 2, 3),
  bty = "n"
)
