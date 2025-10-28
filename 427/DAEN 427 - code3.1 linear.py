# pyright: reportMissingImports=false

import pandas as pd
import arviz as az
import numpy as np
import pymc as pm
import matplotlib.pyplot as plt

#### DATA ####
# penguins = pd.read_csv('/Users/alexander/Library/Mobile Documents/com~apple~CloudDocs/Python/BMCP/data/penguins.csv')
# penguins = pd.read_csv('/Users/abuabara/Library/Mobile Documents/com~apple~CloudDocs/TAMU/DAEN/Fall 2025/427/My_files/Code/data/penguins.csv')
penguins = pd.read_csv('/Users/abuabara/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Teaching/2025 Fall/427/My_files/Code/data/penguins.csv')
missing_data = penguins[[
    'body_mass_g', 'flipper_length_mm', 'bill_length_mm', 'sex'
    ]].isnull().any(axis=1)
penguins = penguins.loc[~missing_data]
penguins.head()

#penguins_filtered = (
    # filter
#    penguins[penguins['species'].isin(['Adelie', 'Chinstrap'])]
    # mutate
#    .assign(
#        species_code=lambda df: df['species'].apply(lambda x: 1 if x == 'Chinstrap' else 0),
#        bill_length_mm=lambda df: pd.to_numeric(df['bill_length_mm'], errors='coerce')
#    )
    # select
#    [['species_code', 'bill_length_mm', 'body_mass_g']]
    # drop NA
#    .dropna(subset=['species_code', 'bill_length_mm', 'body_mass_g'])
#)
#print(penguins_filtered)

# summary stats
(penguins.loc[:, ['body_mass_g']]
         .agg(['mean', 'std', 'count'])
         .round(3).T)

# empirical mean and standard deviation of penguin mass
# and observed number of penguins per species
(penguins.loc[:, ['species', 'body_mass_g']]
         .groupby('species')
         .agg(['mean', 'std', 'count'])
         .round(3))

(penguins.loc[:, ['sex', 'body_mass_g']]
         .groupby('sex')
         .agg(['mean', 'std', 'count'])
         .round(3))

(penguins.loc[:, ['species', 'sex', 'body_mass_g']]
         .groupby(['species', 'sex'])
         .agg(['mean', 'std', 'count'])
         .round(3))

############
plt.figure()
az.plot_kde(penguins['body_mass_g'].values, rug=True)
plt.ylim(0)
plt.title('Penguin Mass KDE')
plt.xlabel('(g)')
plt.yticks([0], alpha=0)

#### ADELIE MASS ####
adelie_mask = (penguins['species'] == 'Adelie')
adelie_mass_obs = penguins.loc[adelie_mask, 'body_mass_g'].values

with pm.Model() as model_adelie_penguin_mass:
    μ = pm.Normal('μ', 4000, 3000)
    σ = pm.HalfStudentT('σ', 100, 2000)
    mass = pm.Normal('mass', mu=μ, sigma=σ, observed=adelie_mass_obs)
    idata_adelie_mass = pm.sample(chains=4)
    idata_adelie_mass.extend(pm.sample_prior_predictive(samples=5000))

pm.model_to_graphviz(model_adelie_penguin_mass)

# samples from the prior
axes = az.plot_posterior(idata_adelie_mass.prior, var_names=['μ', 'σ'], figsize=(6, 3))
axes[0].axvline(az.summary(idata_adelie_mass.prior).loc['μ', 'mean'], linestyle='--', c='lightblue')
axes[0].axvline(0, linestyle='--', c='lightgray')
axes[1].axvline(az.summary(idata_adelie_mass.prior).loc['σ', 'mean'], linestyle='--', c='lightblue')
axes[1].axvline(0, linestyle='--', c='lightgray')

# KDE and rank plot for posterior estimates of parameters
az.plot_trace(idata_adelie_mass, divergences='bottom', kind='rank_bars', figsize=(6, 6))

# summary stats of the Bayesian model
az.summary(idata_adelie_mass)

#############
# az.plot_joint(idata_adelie_mass, kind='kde', fill_last=False)
az.plot_pair(
    idata_adelie_mass,
    kind='kde',
    marginals=True,
    var_names=['μ', 'σ'] # Replace with variable names
)

# posterior plot of the Bayesian model
axes = az.plot_posterior(idata_adelie_mass, hdi_prob=.94, figsize=(6, 3))
axes[0].axvline(az.summary(idata_adelie_mass).loc['μ', 'mean'], linestyle='--', c='lightblue')
axes[1].axvline(az.summary(idata_adelie_mass).loc['σ', 'mean'], linestyle='--', c='lightblue')

#### COMPARING GROUPS ####
# pd.categorical makes it easy to index species below
all_species = pd.Categorical(penguins['species'])
coords = {'species': all_species.categories}

with pm.Model(coords=coords) as model_penguin_mass_all_species:
    μ = pm.Normal('μ', 4000, 3000, dims='species')
    σ = pm.HalfStudentT('σ', 100, 2000, dims='species')
    # note the addition of the shape parameter
    mass = pm.Normal('mass',
                     mu=μ[all_species.codes],
                     sigma=σ[all_species.codes],
                     observed=penguins['body_mass_g'])
    idata_penguin_mass_all_species = pm.sample()

pm.model_to_graphviz(model_penguin_mass_all_species)

# KDE and rank plot for posterior estimates of parameters
az.plot_trace(idata_penguin_mass_all_species,
              compact=False,
              divergences='bottom',
              kind='rank_bars',
              figsize=(6, 18))

# forest plot of the mean of mass of each species group
# each line represents one chain in the sampler,
# the dot is a point estimate, in this case the mean,
# the thin line is the interquartile range from 25% to 75% of the posterior,
# and the thick line is the 94% Highest Density Interval (HDI)
axes = az.plot_forest(idata_penguin_mass_all_species,
                      var_names=['μ'],
                      combined=True,
                      figsize=(6, 2))
axes[0].set_title('μ Mass Estimate: 94% HDI')

axes = az.plot_forest(idata_penguin_mass_all_species,
                      var_names=['σ'],
                      combined=True,
                      figsize=(6, 2))
axes[0].set_title('σ Mass Estimate: 94% HDI')

#### LINEAR REGRESSION ####
# simple linear regression
adelie_flipper_length_obs = penguins.loc[adelie_mask, 'flipper_length_mm']

with pm.Model() as model_adelie_flipper_regression:
    # pm.Data allows to change the underlying value in a later code block
    adelie_flipper_length = pm.Data('adelie_flipper_length', adelie_flipper_length_obs)
    β_0 = pm.Normal('β_0', 0, 4000)
    β_1 = pm.Normal('β_1', 0, 4000)
    σ = pm.HalfStudentT('σ', 100, 2000)
    μ = pm.Deterministic('μ', β_0 + β_1 * adelie_flipper_length)
    mass = pm.Normal('mass', mu=μ, sigma=σ, observed=adelie_mass_obs)
    idata_adelie_flipper_regression = pm.sample(return_inferencedata=True)

pm.model_to_graphviz(model_adelie_flipper_regression)

# summary stats
az.summary(idata_adelie_mass)
az.summary(idata_adelie_flipper_regression, var_names=['β_0','β_1','σ'])

az.plot_forest([idata_adelie_mass,
                idata_adelie_flipper_regression],
                model_names=['mass_only', 'flipper_regression'],
                var_names=['σ'],
                combined=True,
                figsize=(6, 2))

# estimates of the parameter value distributions of the linear regression coefficient
# from model_adelie_flipper_regression
axes = az.plot_posterior(idata_adelie_flipper_regression, var_names = ['β_0', 'β_1'], figsize=(6, 3))
axes[0].axvline(az.summary(idata_adelie_flipper_regression).loc['β_0', 'mean'], linestyle='--', c='lightblue')
axes[0].axvline(0, linestyle='--', c='lightgray')
axes[1].axvline(az.summary(idata_adelie_flipper_regression).loc['β_1', 'mean'], linestyle='--', c='lightblue')

#traditional ols model####################################
# from sklearn.linear_model import LinearRegression
# model = LinearRegression()
# model.fit(pd.DataFrame(adelie_flipper_length_obs), adelie_mass_obs)
# # print(f"Intercept: {model.intercept_}")
# # print(f"Slope: {model.coef_[0]}")
# y_pred = model.predict(pd.DataFrame(adelie_flipper_length_obs))
# plt.subplots(figsize=(6, 3))
# plt.scatter(adelie_flipper_length_obs, adelie_mass_obs,
#             color='blue', label='Actual Data')
# plt.plot(adelie_flipper_length_obs,
#          y_pred, color='red', label='Regression Line')
# plt.xlabel('bill length (mm)')
# plt.title('')
# plt.legend()
# plt.show()
# 
# import statsmodels.api as sm
# X = sm.add_constant(adelie_flipper_length_obs)
# model = sm.OLS(adelie_mass_obs, X).fit()
# print(model.summary())
# #                         coef    std err          t      P>|t|      [0.025      0.975]
# # const             -2508.0877    986.911     -2.541      0.012   -4458.792    -557.383
# # flipper_length_mm    32.6889      5.188      6.300      0.000      22.434      42.944
# # 
# from sklearn.metrics import mean_squared_error
# mean_squared_error(adelie_mass_obs, y_pred)
# # 163752.53896355242
# # --> sqrt(0.163752) ≈ 404.664
##########################################################

beta0_m          = idata_adelie_flipper_regression.posterior['β_0'].mean().item()
beta1_m          = idata_adelie_flipper_regression.posterior['β_1'].mean().item()
flipper_length   = np.linspace(adelie_flipper_length_obs.min(), adelie_flipper_length_obs.max(), 100)
adelie_mass_mean = beta0_m + beta1_m * flipper_length

fig, ax = plt.subplots(figsize=(6, 4))
ax.scatter(adelie_flipper_length_obs, adelie_mass_obs, alpha=.6)
ax.plot(flipper_length, adelie_mass_mean, c='red', linewidth=3, label=f'y = {beta0_m:.2f} + {beta1_m:.2f} * x')
az.plot_hdi(adelie_flipper_length_obs, idata_adelie_flipper_regression.posterior['μ'], hdi_prob=0.94, color='grey', ax=ax)
plt.title('Penguin Mass by Flipper Length')
ax.set_xlabel('Length (mm)')
ax.set_ylabel('Mass (g)')

# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_linear1_mass_lenght.png', dpi=300, bbox_inches='tight')

# prediction at adelie_flipper_length_obs.mean() = 190.10
with model_adelie_flipper_regression:
    pm.set_data({'adelie_flipper_length': [adelie_flipper_length_obs.mean()]})
    posterior_predictions = pm.sample_posterior_predictive(
        idata_adelie_flipper_regression.posterior, var_names=['mass','μ'])

mass_mean = posterior_predictions.posterior_predictive['mass'].mean().round(3).item()
mu_mean = posterior_predictions.posterior_predictive['μ'].mean().round(3).item()

fig, ax = plt.subplots(figsize=(6, 4))
ax.set_yticks([])
ax.set_xlim(2900, 4500)
ax.set_xlabel('Mass (g)')
az.plot_dist(posterior_predictions.posterior_predictive['mass'], label='Posterior Predictive of \nIndividual Penguin Mass', ax=ax)
ax.axvline(posterior_predictions.posterior_predictive['mass'].mean(), linestyle='--', c='lightblue', linewidth=4)
ax.text(mass_mean + 20, 0.00115, f'{mass_mean:.1f}', color='blue', fontsize=10, rotation=0, va='bottom')
ax.axvline(posterior_predictions.posterior_predictive['μ'].mean(), linestyle='--', c='pink', linewidth=2)
ax.text(mu_mean + 20, 0.0125, f'{mu_mean:.1f}', color='red', fontsize=10, rotation=0, va='bottom')
az.plot_dist(posterior_predictions.posterior_predictive['μ'], label='Posterior Predictive of μ', color='red', ax=ax)
ax.legend(loc=2)

# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_linear1_mass_mean.png', dpi=300, bbox_inches='tight')

# centering to make a meaningful β_0
# center = adelie_flipper_length_obs.min()
center = adelie_flipper_length_obs.mean()

adelie_flipper_length_obs_c = (adelie_flipper_length_obs - center)
adelie_flipper_length_obs_c

with pm.Model() as model_adelie_flipper_regression:
    # pm.Data allows us to change the underlying value in a later code block
    adelie_flipper_length = pm.Data('adelie_flipper_length', adelie_flipper_length_obs_c)
    β_0 = pm.Normal('β_0', 0, 4000)
    β_1 = pm.Normal('β_1', 0, 4000)
    σ = pm.HalfStudentT('σ', 100, 2000)
    μ = pm.Deterministic('μ', β_0 + β_1 * adelie_flipper_length)
    mass = pm.Normal('mass', mu=μ, sigma=σ, observed=adelie_mass_obs)
    idata_adelie_flipper_regression = pm.sample(return_inferencedata=True)

pm.model_to_graphviz(model_adelie_flipper_regression)

axes = az.plot_posterior(idata_adelie_flipper_regression, var_names = ['β_0', 'β_1'], figsize=(6, 3))
axes[0].axvline(az.summary(idata_adelie_flipper_regression).loc['β_0','mean'], linestyle='--', c='lightblue')
axes[1].axvline(az.summary(idata_adelie_flipper_regression).loc['β_1','mean'], linestyle='--', c='lightblue')

# summary stats
az.summary(idata_adelie_mass)
az.summary(idata_adelie_flipper_regression, var_names=['β_0','β_1','σ'])

alpha_m = idata_adelie_flipper_regression.posterior['β_0'].mean().item()
beta_m = idata_adelie_flipper_regression.posterior['β_1'].mean().item()
flipper_length_mean = alpha_m + beta_m * (flipper_length - center)

# fig, ax = plt.subplots(figsize=(6, 4))
# ax.scatter(adelie_flipper_length_obs, adelie_mass_obs, c='black', alpha=0.6)
# ax.plot(flipper_length, flipper_length_mean, c='red', linewidth=3, label=f'y = {alpha_m:.2f} + {beta_m:.2f} * x')
# az.plot_hdi(adelie_flipper_length_obs, idata_adelie_flipper_regression.posterior['μ'], hdi_prob=0.94, color='grey', ax=ax)
# plt.title('Penguin Mass by Flipper Length')
# ax.set_xlabel('Length (mm)')
# ax.set_ylabel('Mass (g)')
# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_linear1_mass_lenght_c.png', dpi=300, bbox_inches='tight')

# multiple linear regression
# binary encoding of the categorical predictor
sex_obs = penguins.loc[adelie_mask ,'sex'].replace({'male':0,'female':1}).astype('float64')

with pm.Model() as model_penguin_mass_categorical:
    σ = pm.HalfStudentT('σ', 100, 2000)
    β_0 = pm.Normal('β_0', 0, 3000)
    β_1 = pm.Normal('β_1', 0, 3000)
    β_2 = pm.Normal('β_2', 0, 3000)
    μ = pm.Deterministic('μ', β_0 + β_1 * adelie_flipper_length_obs + β_2 * sex_obs)
    mass = pm.Normal('mass', mu=μ, sigma=σ, observed=adelie_mass_obs)
    idata_adele_mass_categorical = pm.sample(target_accept=.9, return_inferencedata=True)

pm.model_to_graphviz(model_penguin_mass_categorical)

# summary stats
az.summary(idata_adelie_mass)
az.summary(idata_adelie_flipper_regression, var_names=['β_0','β_1','σ'])
az.summary(idata_adele_mass_categorical, var_names=['β_0','β_1','β_2','σ'])

az.plot_forest(
    [idata_adelie_mass,
     idata_adelie_flipper_regression,
     idata_adele_mass_categorical],
     model_names=['mass_only', 'flipper_regression', 'flipper_sex_regression'],
     var_names=['σ'],
     combined=True,
     figsize=(6, 2))

axes = az.plot_posterior(idata_adele_mass_categorical, var_names =['β_0', 'β_1', 'β_2'], figsize=(9, 3))
axes[0].axvline(az.summary(idata_adele_mass_categorical).loc['β_0','mean'], linestyle='--', c='lightblue')
axes[1].axvline(az.summary(idata_adele_mass_categorical).loc['β_1','mean'], linestyle='--', c='lightblue')
axes[2].axvline(az.summary(idata_adele_mass_categorical).loc['β_2','mean'], linestyle='--', c='lightblue')

# plot 1
beta0_m = idata_adele_mass_categorical.posterior['β_0'].mean().item()
beta1_m = idata_adele_mass_categorical.posterior['β_1'].mean().item()
beta2_m = idata_adele_mass_categorical.posterior['β_2'].mean().item()
adelie_mass_mean_male = beta0_m + beta1_m * flipper_length
adelie_mass_mean_female = beta0_m + beta1_m * flipper_length + beta2_m

fig, ax = plt.subplots(figsize=(6, 4))
ax.scatter(adelie_flipper_length_obs, adelie_mass_obs, c=[{0:'blue',1:'red'}[code] for code in sex_obs.values], alpha=.6)
ax.plot(flipper_length, adelie_mass_mean_male, c='blue', linewidth=3, label=f'y = {beta0_m:.2f} + {beta1_m:.2f} * x')
ax.plot(flipper_length, adelie_mass_mean_female, c='red', linewidth=3, label=f'y = {beta0_m:.2f} + {beta1_m:.2f} * x + {beta2_m:.2f} * x')
plt.title('Penguin Mass by Flipper Length and Sex')
ax.set_xlabel('Length (mm)')
ax.set_ylabel('Mass (g)')
ax.legend(loc=2)

# plot 2
β_0_samples = idata_adele_mass_categorical.posterior['β_0'].stack(draws=('chain', 'draw')).values
β_1_samples = idata_adele_mass_categorical.posterior['β_1'].stack(draws=('chain', 'draw')).values
β_2_samples = idata_adele_mass_categorical.posterior['β_2'].stack(draws=('chain', 'draw')).values
mass_male_samples   = np.array([β_0_samples + β_1_samples * fl for fl in flipper_length])
mass_female_samples = np.array([β_0_samples + β_1_samples * fl + β_2_samples for fl in flipper_length])
mass_male_mean      = mass_male_samples.mean(axis=1)
mass_female_mean    = mass_female_samples.mean(axis=1)
mass_male_hdi       = az.hdi(mass_male_samples.T, hdi_prob=0.94)
mass_female_hdi     = az.hdi(mass_female_samples.T, hdi_prob=0.94)

fig, ax = plt.subplots(figsize=(6, 4))
ax.scatter(adelie_flipper_length_obs, adelie_mass_obs, c=[{0: 'blue', 1: 'red'}[code] for code in sex_obs.values], alpha=.6)
ax.plot(flipper_length, mass_male_mean, color='blue', label='Male mean')
ax.fill_between(flipper_length, mass_male_hdi[:, 0], mass_male_hdi[:, 1], color='blue', alpha=0.3)
ax.plot(flipper_length, mass_female_mean, color='red', label='Female mean')
ax.fill_between(flipper_length, mass_female_hdi[:, 0], mass_female_hdi[:, 1], color='red', alpha=0.3)
plt.title('Penguin Mass by Flipper Length and Sex (94% HDI)')
ax.set_xlabel('Length (mm)')
ax.set_ylabel('Mass (g)')
ax.legend(loc=2)

# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_linear2_mass_lenght_gender.png', dpi=300, bbox_inches='tight')

#### alternative plot ####
# alpha_1 = idata_adele_mass_categorical.posterior['β_0'].mean().item()
# beta_1  = idata_adele_mass_categorical.posterior['β_1'].mean().item()
# beta_2  = idata_adele_mass_categorical.posterior['β_2'].mean().item()

# mass_mean_male = alpha_1 + beta_1 * flipper_length
# mass_mean_female = alpha_1 + beta_1 * flipper_length + beta_2

# fig, ax = plt.subplots()
# ax.plot(flipper_length, mass_mean_male, c='blue', label='Male')
# ax.plot(flipper_length, mass_mean_female, c='red', label='Female')
# ax.scatter(adelie_flipper_length_obs, adelie_mass_obs,
           # c=[{0:'blue',1:'red'}[code] for code in sex_obs.values])
# ax.set_xlabel('Length (mm)')
# ax.set_ylabel('Mass')
# ax.legend(loc=2)

#############################################################################