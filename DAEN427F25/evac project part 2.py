# reference
# https://gist.github.com/pb111/512c840affb32593d28573fbb764045b
# https://www.sfu.ca/~mjbrydon/tutorials/BAinPy/01_intro.html

# 1. libraries
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import pymc as pm
import arviz as az
import os

# 2. set working dir / load data (auto choose between personal computer or school computer folder structure)
try:
    os.chdir('/Users/alexander/GitHub/abuabara.github.io/DAEN427F25')
except FileNotFoundError:
    os.chdir('/Users/abuabara/Documents/GitHub/abuabara.github.io/DAEN427F25')

survey = pd.read_csv('survey_short.csv')

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
    # keep only 'selected' columns
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
print('\nDataset shape:', dat_discretized.shape)
print(dat_discretized.info())

print('\nMissing values in original dataset:\n', survey.isnull().sum())
print('\nMissing values after cleaning:\n', dat_discretized.isnull().sum())
# any patterns? imputation methods for any of the variables??

print(dat_discretized.describe())

# 5. exploratory data analysis (EDA)
plt.cla()
sns.displot(dat_discretized['education'], kde=True, stat='density')
plt.title('Education Level Distribution (Displot)')

plt.cla()
plt.boxplot(dat_discretized['education'])
plt.title('Boxplot of Education')

# split education variable by gender
edu_m = dat_discretized.loc[dat_discretized['gender'] == 1, 'education']
edu_f = dat_discretized.loc[dat_discretized['gender'] == 0, 'education']

plt.clf()
plt.boxplot([edu_m, edu_f], labels=['male', 'female'])
plt.title('education distribution by gender')
plt.ylabel('education')
plt.show()

plt.clf()
sns.boxplot(data=dat_discretized, x="gender", y="education")
plt.title("education distribution by gender")
plt.show()

plt.clf()
sns.boxplot(data=dat_discretized, x="evacuation", y="education")
plt.title("education distribution by evacuation")
plt.show()

plt.clf()
sns.boxplot(data=dat_discretized, x="evacuation", y="expected_hh_impacts")
plt.title("expected_hh_impacts distribution by evacuation")
plt.show()

# 6. correlation matrix
correlation_matrix = dat_discretized.corr()

plt.figure(figsize=(8, 6))
# generate mask for the upper triangle (you can invert it for lower :)
mask = np.triu(np.ones_like(correlation_matrix, dtype=bool), k=1)
sns.heatmap(
    correlation_matrix,
    mask=mask,
    annot=True,
    cmap='coolwarm_r', # reverse so negative=red, positive=blue
    center=0,          # set 0 as the center
    vmin=-1,            # force min scale
    vmax=1,              # force max scale
    fmt='.2f',
    linewidths=.5
)
plt.title('Correlation Matrix of Variables')

# 7. modeling
# define predictor and outcome
X = dat_discretized[['signif_evac_orders']]
X = sm.add_constant(X)
y = dat_discretized['evacuation']

# simple models
# linear regression
linear_model = sm.OLS(y, X).fit()
print(linear_model.summary())

# logistic regression
logit_model = sm.Logit(y, X).fit()
print(logit_model.summary())

# 8. prediction / visualization
# smooth range of X values for plotting
x_range = np.linspace(X['signif_evac_orders'].min(),
                      X['signif_evac_orders'].max(), 100)
x_plot = pd.DataFrame({'const': 1, 'signif_evac_orders': x_range})

y_linear_pred = linear_model.predict(x_plot)
y_logit_pred = logit_model.predict(x_plot)

# jitter actual values (for visualization)
y_jittered = y + np.random.uniform(-0.05, 0.05, size=len(y))
X_jittered = X['signif_evac_orders'] + np.random.uniform(-0.1, 0.1, size=len(X))

# plot results
plt.figure(figsize=(8, 5))
plt.scatter(X_jittered, y_jittered, label='Data', color='gray', marker='.', alpha=.5)
plt.plot(x_range, y_linear_pred, linestyle=':', label='Linear', color='b')
plt.plot(x_range, y_logit_pred, linestyle='--', label='Logistic', color='r')
plt.xlabel('Significance of Evacuation Orders')
plt.ylabel('Evacuation Probability')
plt.title('Linear vs Logistic')
plt.ylim(-.075, 1.075)
plt.legend()

#####################################################################################
# PyMC https://www.pymc.io/
#####################################################################################
# 9. bayesian linear regression with PyMC
# start a PyMC model
with pm.Model() as bayes_linear_model:
    # priors (assumptions before looking at data)
    # before seeing data, we believe coefficients are likely around 0, but we allow large deviations (σ = 10 is weakly informative)
    beta = pm.Normal('beta', mu=0, sigma=10, shape=X.values.shape[1])  # intercept + slope
    # we're uncertain about 'noise' but assume it's not extremely large
    sigma = pm.HalfNormal('sigma', sigma=5) # error term
    # linear predictor
    # this is your regression line based on unknown coefficients β
    # equivalent to μ=β0+β1⋅signif_evac_orders
    mu = pm.math.dot(X.values, beta)
    # likelihood (Gaussian)
    # the Data values are normally distributed around the line μ with noise σ
    y_obs = pm.Normal('y_obs', mu=mu, sigma=sigma, observed=y.values)
    # sampling
    # we use MCMC to estimate the most probable values of β and σ given the data
    idata_lin = pm.sample(
        draws=2000,        # number of posterior samples per chain
        tune=1000,         # warmup/adaptation phase
        chains=4,          # runs independent simulations
        cores=2,
        target_accept=0.9, # more conservative step size (helps prevent divergences)
        random_seed=123,
        return_inferencedata=True # returns structured object for diagnostics / plotting
    )

# visual summary
pm.model_to_graphviz(bayes_linear_model)
# (Priors) → (Regression Model) → (Data Likelihood) → (Sampling) → (Posterior)
#  Normal     β0 + β1X             y ~ Normal          MCMC         Distributions

# summary
print(az.summary(idata_lin, var_names=['beta', 'sigma'], round_to=2))

# trace plots
az.plot_trace(idata_lin, var_names=['beta', 'sigma'])

#####################################################################################
# Bambi https://bambinos.github.io/bambi/
#####################################################################################
# print(dat_discretized.head())
# import bambi as bmb

# model1 = bmb.Model(
#     'evacuation ~ signif_evac_orders',
#     dat_discretized
#     )
# model1.plot_priors
# model1.build()
# model1.graph()
# model1_idata = model1.fit()
# print(az.summary(model1_idata))
# bmb.interpret.plot_predictions(model1, model1_idata, 'signif_evac_orders')
# plt.ylim(-.075, 1.075)

# # dat_discretized['gender_cat'] = dat_discretized['gender'].astype('category')
# dat_discretized['gender_cat'] = dat_discretized['gender'].map({0: 'female', 1: 'male'})

# model2 = bmb.Model('evacuation ~ signif_evac_orders + gender_cat',
#                    dat_discretized)
# model2.plot_priors
# model2.build()
# model2.graph()
# model2_idata = model2.fit()
# bmb.interpret.plot_predictions(model2, model2_idata, 'signif_evac_orders')
# plt.ylim(-.075, 1.075)

# ax = bmb.interpret.plot_predictions(
#     model2,
#     model2_idata,
#     ['signif_evac_orders', 'gender_cat'])
# plt.ylim(-.075, 1.075)

# model2 = bmb.Model('evacuation ~ signif_evac_orders * gender_cat',
#                    dat_discretized)
# model2.plot_priors
# model2.build()
# model2.graph()
# model2_idata = model2.fit()
# bmb.interpret.plot_predictions(model2, model2_idata, 'signif_evac_orders')
# plt.ylim(-.075, 1.075)

# ax = bmb.interpret.plot_predictions(
#     model2,
#     model2_idata,
#     ['signif_evac_orders', 'gender_cat'])
# plt.ylim(-.075, 1.075)

#####################################################################################
#####################################################################################

# 10. bayesian linear model predictions
X_pred = x_plot.values  # design matrix from your earlier linear/logit plots

# extract posterior samples
beta_samples_lin = (
    idata_lin.posterior['beta']
    .stack(sample=('chain', 'draw'))
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
plt.scatter(X_jittered, y_jittered, label='Data (jittered)', color='gray', marker='.', alpha=.5)
plt.fill_between(x_range, y_lin_lower, y_lin_upper, alpha=0.2, label='Bayesian 95% CI')
plt.plot(x_range, y_lin_mean, label='Bayesian (pymc)', linewidth=1, color='b', alpha=0.8)
plt.plot(x_range, y_linear_pred, linestyle=':', label='Frequentist (OLS)', linewidth=2, color='r', alpha=0.8)
plt.xlabel('Significance of Evacuation Orders')
plt.ylabel('Evacuation Probability')
plt.title('Frequentist vs Bayesian Linear Regression')
plt.ylim(-.075, 1.075)
plt.legend()
plt.show()

# 11. bayesian logistic regression with PyMC
# start a PyMC model
with pm.Model() as bayes_logit_model:
    # priors on coefficients (including intercept)
    # beta[0] = intercept (for 'const'), beta[1] = slope for 'signif_evac_orders'
    beta = pm.Normal('beta', mu=0, sigma=5, shape=X.values.shape[1])
    # linear predictor
    eta = pm.math.dot(X.values, beta)
    # logistic link
    p = pm.Deterministic('p', pm.math.sigmoid(eta))
    # likelihood
    y_obs = pm.Bernoulli('y_obs', p=p, observed=y.values)
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
print(az.summary(idata, var_names=['beta'], round_to=2))

# trace plots for coefficients
az.plot_trace(idata, var_names=['beta'])

# 13. posterior predictive / fitted curve over x_range
# extract posterior samples for beta
beta_samples = (
    idata.posterior['beta']
    .stack(sample=('chain', 'draw'))
    .values.T          # shape: (n_params, n_samples)
)                      # or (n_samples, n_params) depending on your preference

# ensure shape is (n_samples, n_params)
beta_samples = beta_samples.T if beta_samples.shape[0] < beta_samples.shape[1] else beta_samples
# beta_samples.shape = (n_samples, n_params)

# compute predicted probabilities for each posterior draw
# logits: (n_points, n_samples)
logits = x_plot.values @ beta_samples.T
p_samples = 1 / (1 + np.exp(-logits))  # logistic transform

# posterior mean and 95% credible interval at each x
p_mean = p_samples.mean(axis=1)
p_lower = np.percentile(p_samples, 2.5, axis=1)
p_upper = np.percentile(p_samples, 97.5, axis=1)

# 14. compare frequentist and Bayesian fits visually
plt.figure(figsize=(8, 5))
# jittered actual data (from previous section)
plt.scatter(X_jittered, y_jittered,
            label='Data (jittered)',
            color='gray', marker='.', alpha=0.6)
# bayesian posterior mean curve
plt.plot(x_range, p_mean,
    label='Bayesian Logit (pymc)',
    color='b', linewidth=1, alpha=0.8,)
# 95% credible band
plt.fill_between(x_range, p_lower, p_upper,
                alpha=0.2, label='Bayesian 95% CI')
# frequentist logistic curve
plt.plot(x_range, y_logit_pred, linestyle=':', linewidth=2, alpha=0.8,
         label='Logistic (statsmodel)', color='r')
plt.xlabel('Significance of Evacuation Orders')
plt.ylabel('Evacuation Probability')
plt.title('Frequentist vs Bayesian Logistic Regression')
plt.ylim(-.075, 1.075)
plt.legend()

#####################################################################################
#####################################################################################
# # hierarchical (multilevel) logistic regression model
# structured in a classic hierarchical form with:
# Hyperpriors (population-level priors) for intercept and slope
# Group-level parameters that vary by gender
# Individual-level likelihood
#####################################################################################
#####################################################################################
# 13. prep data
# extract gender vector as integer array
gender_vec = dat_discretized['gender'].values.astype(int)
print(np.unique(gender_vec), gender_vec[:10])

# predictor and outcome arrays
X_mat = X.values              # should contain ['const', 'signif_evac_orders']
y_vec = y.values.astype(int)  # evacuation vector

# 14. define and run hierarchical model
with pm.Model() as bayes_logit_hierarchical:
    # ---- hyperpriors (population-level) ----
    mu_alpha = pm.Normal('mu_alpha', mu=0, sigma=5) # mean intercept
    sigma_alpha = pm.HalfNormal('sigma_alpha', sigma=5)
    mu_beta = pm.Normal('mu_beta', mu=0, sigma=5) # mean slope
    sigma_beta = pm.HalfNormal('sigma_beta', sigma=5)
    # ---- group-level coefficients (by gender: 0=female, 1=male) ----
    alpha_group = pm.Normal('alpha_group', mu=mu_alpha, sigma=sigma_alpha, shape=2)
    beta_group = pm.Normal('beta_group', mu=mu_beta, sigma=sigma_beta, shape=2)
    # ---- linear predictor ----
    eta = alpha_group[gender_vec] + beta_group[gender_vec] * X_mat[:, 1]
    # ---- logistic link ----
    p = pm.Deterministic('p', pm.math.sigmoid(eta))
    # ---- likelihood ----
    y_obs = pm.Bernoulli('y_obs', p=p, observed=y_vec)
    # ---- sampling ----
    idata = pm.sample(
        draws=2000,
        tune=1000,
        target_accept=0.9,
        chains=4,
        random_seed=123,
        return_inferencedata=True)

# visual model structure
pm.model_to_graphviz(bayes_logit_hierarchical)

# 15. posterior summary
print(az.summary(idata, var_names=[
    'mu_alpha', 'sigma_alpha',
    'mu_beta', 'sigma_beta',
    'alpha_group', 'beta_group'], round_to=2))

# trace plots
az.plot_trace(idata, var_names=[
    'mu_alpha', 'sigma_alpha',
    'mu_beta', 'sigma_beta',
    'alpha_group', 'beta_group'])

# 16. gender-specific logistic curves
# extract posterior samples
alpha_samples = idata.posterior['alpha_group'].stack(sample=('chain', 'draw')).values
beta_samples = idata.posterior['beta_group'].stack(sample=('chain', 'draw')).values

# x range for model predictions
x_range = np.linspace(
    dat_discretized['signif_evac_orders'].min(),
    dat_discretized['signif_evac_orders'].max(),
    100)

# compute posterior predictive probability curves
p_samples_male = 1 / (1 + np.exp(-(alpha_samples[1] + beta_samples[1] * x_range[:, None])))
p_samples_female = 1 / (1 + np.exp(-(alpha_samples[0] + beta_samples[0] * x_range[:, None])))

# credible interval helper function
def ci(samples):
    return (
        samples.mean(axis=1),
        np.percentile(samples, 2.5, axis=1),
        np.percentile(samples, 97.5, axis=1))

p_male_mean, p_male_low, p_male_high = ci(p_samples_male)
p_female_mean, p_female_low, p_female_high = ci(p_samples_female)

# plot
plt.figure(figsize=(8, 5))
plt.plot(x_range, p_male_mean, linestyle=':', color='b', label='Male (posterior)')
plt.plot(x_range, p_female_mean, linestyle=':', color='r', label='Female (posterior)')
plt.fill_between(x_range, p_male_low, p_male_high, color='b', alpha=0.2, label='Male 95% CI')
plt.fill_between(x_range, p_female_low, p_female_high, color='r', alpha=0.2, label='Female 95% CI')
plt.xlabel('Significance of Evacuation Orders')
plt.ylabel('Evacuation Probability')
plt.title('Gender-Specific Logistic Curves (Hierarchical Model)')
plt.ylim(0, 1)
plt.legend()
#####################################################################################
#####################################################################################