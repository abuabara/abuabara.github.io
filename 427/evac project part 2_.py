# reference
# https://gist.github.com/pb111/512c840affb32593d28573fbb764045b

# 1. libraries
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import os

# 2. set working dir / load data
os.chdir('/Users/abuabara/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Teaching/2025:2026/2025 Fall/427/My_files/Project')
survey = pd.read_csv("survey_short.csv")

# confirm load
print(survey.head())
print(type(survey))

# in case it's not a dataframe:
# dat_dicretized = pd.DataFrame(dat_dicretized)

# 3. data prep
# recoding categorical variables into numeric
dat_discretized = (
    survey
    # create or modify multiple columns
    .assign(
        # lambda function allows referencing the df (survey)
        evacuation=lambda df: df['evacuation'].map({'yes': 1, 'no': 0}),
        evac_orders=lambda df: df['evac_orders'].map({'mandatory': 1, 'voluntary': 0}),
        risk_area=lambda df: df['risk_area'].map({'yes': 1, 'no': 0}),
        gender=lambda df: df['gender'].map({'male': 1, 'female': 0}),
    )
    # keep only selected columns
    .loc[:, [
        'evacuation',
        'expected_hh_impacts',
        'signif_evac_orders',
        'evac_orders',
        'unnecessary_evac_exp',
        'multiple_concerns',
        'risk_area',
        'social_cues',
        'consult_info',
        'age',
        'education',
        'gender'
    ]]
    # drop rows with any NaNs ... ie, keep just completed observations
    .dropna()
)

print(dat_discretized.head())

# 4. descriptive statistics
print("\nDataset shape:", dat_discretized.shape)
print(dat_discretized.info())

print("\nMissing values in original dataset:\n", survey.isnull().sum())
print("\nMissing values after cleaning:\n", dat_discretized.isnull().sum())

print(dat_discretized.describe())

# 5. exploratory data analysis
plt.cla()
sns.distplot(dat_discretized['education'], bins=10, kde=True)
plt.title("Distribution of Education Level")
plt.show()

plt.cla()
sns.displot(dat_discretized['education'], kde=True)
plt.title("Education Level Distribution (Displot)")
plt.show()

plt.cla()
plt.boxplot(dat_discretized['education'])
plt.title("Boxplot of Education")
plt.show()

# 6. correlation matrix
correlation_matrix = dat_discretized.corr()
plt.figure(figsize=(8, 6))
sns.heatmap(
    correlation_matrix,
    annot=True,
    cmap='coolwarm_r', # reverse so negative=red, positive=blue
    center=0,          # set 0 as the center
    fmt=".2f",
    linewidths=.5
)
plt.title("Correlation Matrix of Variables")
plt.show()

# 7. modeling
# define predictor and outcome
X = dat_discretized[["signif_evac_orders"]]
X = sm.add_constant(X)
y = dat_discretized["evacuation"]

# simple models
# linear regression
linear_model = sm.OLS(y, X).fit()
print(linear_model.summary())

# logistic regression
logit_model = sm.Logit(y, X).fit()
print(logit_model.summary())

# 8. prediction / visualization
# smooth range of X values for plotting
x_range = np.linspace(X["signif_evac_orders"].min(),
                      X["signif_evac_orders"].max(), 100)
x_plot = pd.DataFrame({"const": 1, "signif_evac_orders": x_range})

y_linear_pred = linear_model.predict(x_plot)
y_logit_pred = logit_model.predict(x_plot)

# jitter actual values (for visualization)
y_jittered = y + np.random.uniform(-0.05, 0.05, size=len(y))

# plot results
plt.figure(figsize=(8, 5))
plt.scatter(X["signif_evac_orders"], y_jittered, label="Actual Evacuation", color="black", marker="x")
plt.plot(x_range, y_linear_pred, linestyle="--", label="Linear Prediction")
plt.plot(x_range, y_logit_pred, label="Logistic Prediction")
plt.xlabel("Significance of Evacuation Orders")
plt.ylabel("Evacuation Probability")
plt.title("Linear vs Logistic Model Comparison")
plt.legend()
plt.show()

# 9. bayesian linear regression with PyMC
import pymc as pm
import arviz as az

# prepare data
X_lin = X.values # includes intercept and 'signif_evac_orders'
y_lin = y.values # evacuation 0/1 (still usable for linear model)

# start a PyMC model
with pm.Model() as bayes_linear_model:
    # priors (assumptions before looking at data)
    # before seeing data, we believe coefficients are likely around 0, but we allow large deviations (σ = 10 is weakly informative)
    beta = pm.Normal("beta", mu=0, sigma=10, shape=X_lin.shape[1])  # intercept + slope
    # we're uncertain about "noise" but assume it's not extremely large
    sigma = pm.HalfNormal("sigma", sigma=5)  # error term
    
    # linear predictor
    # this is your regression line based on unknown coefficients β
    # equivalent to μ=β0+β1⋅signif_evac_orders
    mu = pm.math.dot(X_lin, beta)
    
    # likelihood (Gaussian)
    # the actual evacuation values are normally distributed around the line μ with noise σ
    y_obs = pm.Normal("y_obs", mu=mu, sigma=sigma, observed=y_lin)
    
    # sampling
    # we use MCMC to estimate the most probable values of β and σ given the data
    idata_lin = pm.sample(
        draws=2000, # number of posterior samples per chain
        tune=1000, # warmup/adaptation phase
        chains=4, # runs independent simulations
        target_accept=0.9, # more conservative step size (helps prevent divergences)
        random_seed=123,
        return_inferencedata=True # returns structured object for diagnostics / plotting
    )

# visual summary
pm.model_to_graphviz(bayes_linear_model)
# (Priors) → (Regression Model) → (Data Likelihood) → (Sampling) → (Posterior)
#  Normal     β0 + β1X             y ~ Normal          MCMC         Distributions

# summary
print(az.summary(idata_lin, var_names=["beta", "sigma"], round_to=2))

# trace plots
az.plot_trace(idata_lin, var_names=["beta", "sigma"])
plt.tight_layout()
plt.show()

# 10. bayesian linear model predictions
X_pred = x_plot.values  # design matrix from your earlier linear/logit plots

# extract posterior samples
beta_samples_lin = (
    idata_lin.posterior["beta"]
    .stack(sample=("chain", "draw"))
    .values.T
)
beta_samples_lin = beta_samples_lin.T if beta_samples_lin.shape[0] < beta_samples_lin.shape[1] else beta_samples_lin

# compute posterior predictions
y_samples_lin = X_pred @ beta_samples_lin.T

# mean and 95% CI
y_lin_mean = y_samples_lin.mean(axis=1)
y_lin_lower = np.percentile(y_samples_lin, 2.5, axis=1)
y_lin_upper = np.percentile(y_samples_lin, 97.5, axis=1)

# plot
plt.figure(figsize=(8, 5))
plt.scatter(X["signif_evac_orders"], y_jittered, label="Actual (jittered)", color="black", marker="x", alpha=0.6)
plt.plot(x_range, y_linear_pred, linestyle="--", label="Frequentist Linear (OLS)")
plt.plot(x_range, y_lin_mean, label="Bayesian Linear Mean")
plt.fill_between(x_range, y_lin_lower, y_lin_upper, alpha=0.2, label="Bayesian 95% CrI")
plt.xlabel("Significance of Evacuation Orders")
plt.ylabel("Evacuation (Linear Prediction)")
plt.title("Frequentist vs Bayesian Linear Regression")
plt.legend()
plt.show()

# 11. bayesian logistic regression with PyMC
# prepare design matrices as plain numpy arrays
X_mat = X.values # includes intercept column "const" and "signif_evac_orders"
y_vec = y.values.astype(int)

# start a PyMC model
with pm.Model() as bayes_logit_model:
    # priors on coefficients (including intercept)
    # beta[0] = intercept (for "const"), beta[1] = slope for "signif_evac_orders"
    beta = pm.Normal("beta", mu=0, sigma=5, shape=X_mat.shape[1])

    # linear predictor
    eta = pm.math.dot(X_mat, beta)

    # logistic link
    p = pm.Deterministic("p", pm.math.sigmoid(eta))

    # likelihood
    y_obs = pm.Bernoulli("y_obs", p=p, observed=y_vec)

    # sampling from the posterior
    idata = pm.sample(
        draws=2000,
        tune=1000,
        target_accept=0.9,
        chains=4,
        random_seed=123,
        return_inferencedata=True
    )

pm.model_to_graphviz(bayes_logit_model)

# 12. posterior summaries and diagnostics
print(az.summary(idata, var_names=["beta"], round_to=2))

# trace plots for coefficients
az.plot_trace(idata, var_names=["beta"])
plt.tight_layout()
plt.show()

# 13. posterior predictive / fitted curve over x_range
# build prediction design matrix corresponding to x_range (same as for statsmodels part)
X_pred = x_plot.values  # columns: ["const", "signif_evac_orders"]

# extract posterior samples for beta
beta_samples = (
    idata.posterior["beta"]
    .stack(sample=("chain", "draw"))
    .values.T          # shape: (n_params, n_samples)
)                       # or (n_samples, n_params) depending on your preference

# ensure shape is (n_samples, n_params)
beta_samples = beta_samples.T if beta_samples.shape[0] < beta_samples.shape[1] else beta_samples
# beta_samples.shape = (n_samples, n_params)

# compute predicted probabilities for each posterior draw
# logits: (n_points, n_samples)
logits = X_pred @ beta_samples.T
p_samples = 1 / (1 + np.exp(-logits))  # logistic transform

# posterior mean and 95% credible interval at each x
p_mean = p_samples.mean(axis=1)
p_lower = np.percentile(p_samples, 2.5, axis=1)
p_upper = np.percentile(p_samples, 97.5, axis=1)

# 14. compare frequentist and Bayesian fits visually
plt.figure(figsize=(8, 5))

# jittered actual data (from previous section)
plt.scatter(
    X["signif_evac_orders"],
    y_jittered,
    label="Actual Evacuation (jittered)",
    color="black",
    marker="x",
    alpha=0.6
)

# frequentist logistic curve
plt.plot(
    x_range,
    y_logit_pred,
    label="Frequentist Logit (statsmodels)",
    linestyle="--"
)

# bayesian posterior mean curve
plt.plot(
    x_range,
    p_mean,
    label="Bayesian Logit (PyMC posterior mean)"
)

# 95% credible band
plt.fill_between(
    x_range,
    p_lower,
    p_upper,
    alpha=0.2,
    label="Bayesian 95% CrI"
)

plt.xlabel("Significance of Evacuation Orders")
plt.ylabel("Evacuation Probability")
plt.title("Frequentist vs Bayesian Logistic Regression")
plt.legend()
plt.show()