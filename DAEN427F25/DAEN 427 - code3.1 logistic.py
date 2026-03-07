import pandas as pd
import arviz as az
import numpy as np
import pymc as pm
import matplotlib.pyplot as plt
from scipy import special

#### DATA ####
# penguins = pd.read_csv('/Users/alexander/Library/Mobile Documents/com~apple~CloudDocs/Python/BMCP/data/penguins.csv')
# penguins = pd.read_csv('/Users/abuabara/Library/Mobile Documents/com~apple~CloudDocs/TAMU/DAEN/Fall 2025/427/My_files/Code/data/penguins.csv')
penguins = pd.read_csv('/Users/abuabara/Documents/GitHub/abuabara.github.io/DAEN427F25/penguins.csv')
missing_data = penguins[[
    'body_mass_g',
    'flipper_length_mm',
    'bill_length_mm',
    'sex'
    ]].isnull().any(axis=1)
penguins = penguins.loc[~missing_data]
penguins.head()

(penguins.loc[penguins['species']
         .isin(['Adelie', 'Chinstrap']), ['species', 'bill_length_mm']]
         .groupby('species')
         .agg(['mean', 'std', 'count'])
         .round(3))

(penguins.loc[penguins['species']
         .isin(['Adelie', 'Chinstrap']), ['species', 'body_mass_g']]
         .groupby('species')
         .agg(['mean', 'std', 'count'])
         .round(3))

####
plt.figure(figsize=(6, 4))
data = penguins[penguins['species'].isin(['Adelie', 'Chinstrap'])]
x = data['bill_length_mm']
y = data['body_mass_g']
plt.scatter(x, y, color='black', alpha=0.4)
coeffs = np.polyfit(x, y, deg=1)
y_fit = np.polyval(coeffs, x)
plt.plot(x, y_fit, color='red', linestyle='-', linewidth=2)
plt.title('Adelie & Chinstrap Penguins')
plt.xlabel('Bill Length (mm)')
plt.ylabel('Body Mass (g)')

####
plt.figure(figsize=(6, 4))
colors = {'Adelie': 'blue', 'Chinstrap': 'red'}
species_list = ['Adelie', 'Chinstrap']
for species in species_list:
    data = penguins[penguins['species'] == species]
    x = data['bill_length_mm']
    y = data['body_mass_g']
    plt.scatter(x, y, label=species, color=colors[species], alpha=0.6)
    coeffs = np.polyfit(x, y, deg=1)  # linear fit
    y_fit = np.polyval(coeffs, x)
    plt.plot(x, y_fit, color=colors[species], linestyle='--', linewidth=2)
plt.title('Adelie & Chinstrap Penguins')
plt.xlabel('Bill Length (mm)')
plt.ylabel('Body Mass (g)')
plt.legend(loc=4)

# plt.savefig('/Users/alexander/Desktop/plot_linear3_mass_bill_lenght.png', dpi=300, bbox_inches='tight')

#### LINEAR REGRESSION ####
# import seaborn as sns
# penguins = sns.load_dataset('penguins')
# Load penguins data: penguins = sns.load_dataset('penguins')

# Define variables
data = penguins['species'].isin(['Adelie', 'Chinstrap'])
bill_length_obs = penguins.loc[data, 'bill_length_mm'].values
species = pd.Categorical(penguins.loc[data, 'species']) # species_code = species.codes  # 0 = Adelie, 1 = Chinstrap

#traditional ols model####################################
# from sklearn.linear_model import LinearRegression
# model = LinearRegression()
# model.fit(bill_length_obs.reshape(-1, 1), species.codes)
# # print(f"Intercept: {model.intercept_}")
# # print(f"Slope: {model.coef_[0]}")
# y_pred = model.predict(bill_length_obs.reshape(-1, 1))
# plt.subplots(figsize=(6, 3))
# plt.scatter(bill_length_obs.reshape(-1, 1), species.codes,
#             color='blue', label='Actual Data')
# plt.plot(bill_length_obs.reshape(-1, 1),
#          y_pred, color='red', label='Regression Line')
# plt.ylim(-0.1,1.1)
# plt.xlabel('bill length (mm)')
# plt.title('penguins species 0 = Adelie, 1 = Chinstrap')
# plt.legend()
# plt.show()

# import statsmodels.api as sm
# X = sm.add_constant(bill_length_obs.reshape(-1, 1))
# model = sm.OLS(species.codes, X).fit()
# print(model.summary())
# #                  coef    std err          t      P>|t|      [0.025      0.975]
# # const         -2.7190      0.130    -20.919      0.000      -2.975      -2.463
# # x1             0.0723      0.003     23.562      0.000       0.066       0.078

# from sklearn.metrics import mean_squared_error
# mean_squared_error(species.codes, y_pred)
# # 0.0599
# # --> sqrt(0.0599) ≈ 0.2448
##########################################################

with pm.Model() as model_linear_penguins:
    # priors for intercept and slope
    β_0 = pm.Normal('β_0', mu=0, sigma=10)
    β_1 = pm.Normal('β_1', mu=0, sigma=10)
    # linear predictor
    μ = β_0 + β_1 * bill_length_obs
    # noise (residual SD)
    σ = pm.HalfNormal('σ', sigma=1)
    # likelihood: species code is modeled as continuous (even though it's 0/1)
    yl = pm.Normal('yl', mu=μ, sigma=σ, observed=species.codes)
    # sampling
    idata_linear_penguins = pm.sample(5000, chains=2, random_seed=0, idata_kwargs={'log_likelihood': True})
    # prior and posterior predictive checks
    idata_linear_penguins.extend(pm.sample_prior_predictive(samples=1000))
    idata_linear_penguins.extend(pm.sample_posterior_predictive(idata_linear_penguins))

graphviz = pm.model_to_graphviz(model_linear_penguins, )
graphviz
# graphviz.graph_attr.update(dpi='300')
# graphviz.render('/Users/alexander/Desktop/Logistic_model_1', format='png')

az.summary(idata_linear_penguins, round_to=2)

#        mean	  sd	hdi_3%	hdi_97%	mcse_mean	mcse_sd	ess_bulk	ess_tail	r_hat
# β_0	-2.72	0.13	-2.97	-2.48	0.0	         0.0	3678.44	     4054.28	1.0
# β_1	0.07	0.00	0.07	0.08	0.0	         0.0	3738.58	     4001.34	1.0
# σ	    0.25	0.01	0.23	0.27	0.0	         0.0	4675.75	     4156.81	1.0

az.plot_trace(idata_linear_penguins)

# extract posterior samples
posterior = idata_linear_penguins.posterior
β_0_samples = posterior['β_0'].stack(draws=('chain', 'draw')).values
β_1_samples = posterior['β_1'].stack(draws=('chain', 'draw')).values

# plot settings
x_vals = np.linspace(bill_length_obs.min(), bill_length_obs.max(), 100)
colors = ['blue', 'red']
mean_β0 = β_0_samples.mean()
mean_β1 = β_1_samples.mean()
fig, ax = plt.subplots(figsize=(6, 4))
plt.ylim(-0.075, 1.075)
# plot observed data
for i, (label, marker) in enumerate(zip(species.categories, ('.', 's'))):
    _filter = (species.codes == i)
    x = bill_length_obs[_filter]
    np.random.seed(42)
    y = np.random.normal(i, 0.02, size=_filter.sum(), )
    ax.scatter(bill_length_obs[_filter], y, label=label, alpha=.5, s = 20, color=colors[i])
# plot a few posterior predictive regression lines
for i in range(100): # random draws
    y_vals = β_0_samples[i] + β_1_samples[i] * x_vals
    plt.plot(x_vals, y_vals, color='purple', alpha=0.1, lw=.3)
# plot the mean regression line
plt.plot(x_vals, mean_β0 + mean_β1 * x_vals, color='black', lw=1.5, linestyle='--', label='linear model')
plt.xlabel('Bill length (mm)')
ax.set_ylabel('θ', rotation=0)
ax.legend(loc=5)

plt.savefig('/Users/alexander/Desktop/plot_linear4_specie_bill_lenght.png', dpi=300, bbox_inches='tight')

#### LOGISTIC REGRESSION ####
# 1 - simple logistic regression - penguins bill length
# data = penguins['species'].isin(['Adelie', 'Chinstrap'])
# bill_length_obs = penguins.loc[data, 'bill_length_mm'].values
# species = pd.Categorical(penguins.loc[data, 'species'])

with pm.Model() as model_logistic_penguins_bill_length:
    β_0 = pm.Normal('β_0', mu=0, sigma=10)
    β_1 = pm.Normal('β_1', mu=0, sigma=10)
    μ = β_0 + pm.math.dot(bill_length_obs, β_1)
    # application of sigmoid link function
    θ = pm.Deterministic('θ', pm.math.sigmoid(μ))
    # useful for plotting the decision boundary later
    bd = pm.Deterministic('bd', -β_0/β_1)
    # note the change in likelihood
    yl = pm.Bernoulli('yl', p=θ, observed=species.codes)
    idata_logistic_penguins_bill_length = pm.sample(5000, chains=2, random_seed=0, idata_kwargs={'log_likelihood':True})
    idata_logistic_penguins_bill_length.extend(pm.sample_prior_predictive(samples=10000))
    idata_logistic_penguins_bill_length.extend(pm.sample_posterior_predictive(idata_logistic_penguins_bill_length))

graphviz = pm.model_to_graphviz(model_logistic_penguins_bill_length)
graphviz
graphviz.graph_attr.update(dpi='300')
graphviz.render('/Users/alexander/Desktop/Logistic_model_2', format='png')

fig, ax = plt.subplots(figsize=(6, 3))
az.plot_dist(idata_logistic_penguins_bill_length.prior_predictive['yl'], ax=ax, color='grey')
ax.set_xticklabels(['Adelie: 0', 'Chinstrap: 1'] )

az.plot_trace(idata_logistic_penguins_bill_length, var_names=['β_0', 'β_1'], kind='rank_bars', figsize=(6, 4))

# summary stats
az.summary(idata_logistic_penguins_bill_length, var_names=['β_0', 'β_1'], kind='stats')

# plot
colors = ['blue', 'red']
fig, ax = plt.subplots(figsize=(6, 4))
plt.ylim(-0.075, 1.075)
theta = idata_logistic_penguins_bill_length.posterior['θ'].mean(('chain', 'draw'))
idx = np.argsort(bill_length_obs)
ax.vlines(idata_logistic_penguins_bill_length.posterior['bd'].values.mean(), 0, 1, color='black')
bd_hpd = az.hdi(idata_logistic_penguins_bill_length.posterior['bd'].values.flatten(), ax=ax)
plt.fill_betweenx([0, 1], bd_hpd[0], bd_hpd[1], color='green', alpha=.2, label='decision boundary')
for i, (label, marker) in enumerate(zip(species.categories, ('.', 's'))):
    _filter = (species.codes == i)
    x = bill_length_obs[_filter]
    np.random.seed(42)
    y = np.random.normal(i, 0.02, size=_filter.sum())
    ax.scatter(bill_length_obs[_filter], y, label=label, alpha=.5, s = 20, color=colors[i])
ax.plot(bill_length_obs[idx], theta[idx], color='purple', zorder=10, label='logistic model')
az.plot_hdi(bill_length_obs, idata_logistic_penguins_bill_length.posterior['θ'].values, color='C4', ax=ax, plot_kwargs={'zorder':10})
ax.set_xlabel('Bill Length (mm)')
ax.set_ylabel('θ', rotation=0)
ax.legend(loc=5)

plt.savefig('/Users/alexander/Desktop/plot_logit1_specie_bill_lenght.png', dpi=300, bbox_inches='tight')

plt.plot(x_vals, mean_β0 + mean_β1 * x_vals, color='black', lw=.5, linestyle='--', label='linear model')
ax.legend(loc=5)

plt.savefig('/Users/alexander/Desktop/plot_logit2_specie_bill_lenght.png', dpi=300, bbox_inches='tight')

# 2 - simple logistic regression - body mass
mass_obs = penguins.loc[data, 'body_mass_g'].values

with pm.Model() as model_logistic_penguins_mass:
    β_0 = pm.Normal('β_0', mu=0, sigma=10)
    β_1 = pm.Normal('β_1', mu=0, sigma=10)
    μ = β_0 + pm.math.dot(mass_obs, β_1)
    θ = pm.Deterministic('θ', pm.math.sigmoid(μ))
    bd = pm.Deterministic('bd', -β_0/β_1)
    yl = pm.Bernoulli('yl', p=θ, observed=species.codes)
    idata_logistic_penguins_mass = pm.sample(5000, chains=2, 
                                             target_accept=.9, random_seed=0, 
                                             idata_kwargs={'log_likelihood':True})
    idata_logistic_penguins_mass.extend(pm.sample_posterior_predictive(idata_logistic_penguins_mass))

graphviz = pm.model_to_graphviz(model_logistic_penguins_mass)
graphviz
graphviz.graph_attr.update(dpi='300')
graphviz.render('/Users/alexander/Desktop/Logistic_model_3', format='png')

az.plot_trace(idata_logistic_penguins_mass, var_names=['β_0', 'β_1'], kind='rank_bars', figsize=(6, 4))

# summary stats
az.summary(idata_logistic_penguins_mass, var_names=['β_0', 'β_1', 'bd'], kind='stats')

# plot
theta = idata_logistic_penguins_mass.posterior['θ'].mean(('chain', 'draw'))
bd = idata_logistic_penguins_mass.posterior['bd']
fig, ax = plt.subplots(figsize=(6, 4))
plt.ylim(-0.075, 1.075)
idx = np.argsort(mass_obs)
ax.plot(mass_obs[idx], theta[idx], color='purple', lw=3)
az.plot_hdi(mass_obs, idata_logistic_penguins_mass.posterior['θ'], color='C4', ax=ax)
for i, (label, marker) in enumerate(zip(species.categories, ('.', 's'))):
    _filter = (species.codes == i)
    x = mass_obs[_filter]
    np.random.seed(42)
    y = np.random.normal(i, 0.02, size=_filter.sum())
    ax.scatter(mass_obs[_filter], y, label=label, alpha=.5, s = 20, color=colors[i])
ax.set_xlabel('Mass (g)')
ax.set_ylabel('θ', rotation=0)
ax.legend(loc=5)

plt.savefig('/Users/alexander/Desktop/plot_logit3_specie_mass.png', dpi=300, bbox_inches='tight')

# 3 - multiple logistic regression - penguins bill length and body mass
X = penguins.loc[data, ['bill_length_mm', 'body_mass_g']]
# add a column of 1s for the intercept
X.insert(0, 'Intercept', value=1)
X = X.values

with pm.Model() as model_logistic_penguins_bill_length_mass:
    β = pm.Normal('β', mu=0, sigma=20, shape=3)
    μ = pm.math.dot(X, β)
    θ = pm.Deterministic('θ', pm.math.sigmoid(μ))
    bd = pm.Deterministic('bd', -β[0]/β[2] - β[1]/β[2] * X[:,1])
    yl = pm.Bernoulli('yl', p=θ, observed=species.codes)
    idata_logistic_penguins_bill_length_mass = pm.sample(5000, chains=2,
                                                         random_seed=0, 
                                                         target_accept=.9,
                                                         idata_kwargs={'log_likelihood':True})
    idata_logistic_penguins_bill_length_mass.extend(pm.sample_posterior_predictive(idata_logistic_penguins_bill_length_mass))

graphviz = pm.model_to_graphviz(model_logistic_penguins_bill_length_mass)
graphviz
graphviz.graph_attr.update(dpi='300')
graphviz.render('/Users/alexander/Desktop/Logistic_model_4', format='png')

az.plot_trace(idata_logistic_penguins_bill_length_mass, compact=False, var_names=['β'], kind='rank_bars', figsize=(6, 6))

# summary stats
az.summary(idata_logistic_penguins_bill_length_mass, var_names=['β'])

# plot
fig, ax = plt.subplots(figsize=(6, 4))
idx = np.argsort(X[:,1])
bd = idata_logistic_penguins_bill_length_mass.posterior['bd'].mean(('chain', 'draw'))[idx]
species_filter = species.codes.astype(bool)
ax.plot(X[:,1][idx], bd, color='purple', label='decision boundary')
az.plot_hdi(X[:,1], idata_logistic_penguins_bill_length_mass.posterior['bd'], color='C4', ax=ax)
ax.scatter(X[~species_filter,1], X[~species_filter,2], alpha=.5, color='blue', label='Adelie', zorder=10)
ax.scatter(X[species_filter,1], X[species_filter,2], alpha=.5, color='red', label='Chinstrap', zorder=10)
ax.set_ylabel('Mass (g)')
ax.set_xlabel('Bill Length (mm)')
ax.legend(loc=2)

# plt.savefig('/Users/alexander/Desktop/plot_logit4_specie_multiple.png', dpi=300, bbox_inches='tight')

# compare
az.compare({'mass': idata_logistic_penguins_mass,
            'bill': idata_logistic_penguins_bill_length,
            'mass & bill': idata_logistic_penguins_bill_length_mass}).round(1)

models = {'bill': idata_logistic_penguins_bill_length,
          'mass': idata_logistic_penguins_mass,
          'mass & bill': idata_logistic_penguins_bill_length_mass}

fig, axes = plt.subplots(3, 1, figsize=(6, 3), sharey=True)
for (label, model), ax in zip(models.items(), axes):
    az.plot_separation(model, 'yl', ax=ax, color='C4')
    ax.set_title(label)
plt.savefig('/Users/alexander/Desktop/plot_multiple_models.png', dpi=300)

penguins.loc[:,'species'].value_counts()

counts = penguins['species'].value_counts()
adelie_count = counts['Adelie'],
chinstrap_count = counts['Chinstrap']
adelie_count / (adelie_count+chinstrap_count)
chinstrap_count / (adelie_count+chinstrap_count)
adelie_count / chinstrap_count

β_0 = idata_logistic_penguins_bill_length.posterior['β_0'].mean().item()
β_1 = idata_logistic_penguins_bill_length.posterior['β_1'].mean().item()

bill_length = 45
val_1 = β_0 + β_1 * bill_length
val_2 = β_0 + β_1 * (bill_length+1)
f'Probability change from 45mm Bill Length to 46mm: {(special.expit(val_2) - special.expit(val_1))*100:.0f}%'

bill_length = np.array([30, 45])
val_1 = β_0 + β_1 * bill_length
val_2 = β_0 + β_1 * (bill_length+1)
prob_change = (special.expit(val_2) - special.expit(val_1)) * 100
for bl, change in zip(bill_length, prob_change):
    print(f'Probability change from {bl}mm to {bl+1}mm Bill Length: {change:.0f}%')

#############################################################################

# # get posterior samples of beta
# posterior = idata_logistic_penguins_bill_length_mass.posterior
# β_samples = posterior['β'].stack(samples=('chain', 'draw')).values  # shape: (3, n_samples)
# # get mean coefficients
# β_mean = β_samples.mean(axis=1)
# # plotting range
# x_bill = np.linspace(X[:,1].min(), X[:,1].max(), 100)
# # decision boundary: solve for mass when θ = 0.5 (i.e., μ = 0)
# # bd: mass = -(β0 + β1 * bill) / β2
# bd_mean = -(β_mean[0] + β_mean[1] * x_bill) / β_mean[2]
# # plot data points
# plt.figure(figsize=(8,6))
# colors = ['tab:blue', 'tab:orange']
# for s in [0, 1]:
#     idx = species.codes == s
#     plt.scatter(X[idx,1], X[idx,2], label=f'Species {s}', alpha=0.6, color=colors[s])
# # plot decision boundary (mean)
# plt.plot(x_bill, bd_mean, color='k', label='Decision Boundary (mean)', linewidth=2)
# # optional: plot multiple boundaries from samples for uncertainty visualization
# for i in range(0, β_samples.shape[1], 100):  # Every 100th sample
#     β = β_samples[:, i]
#     bd_sample = -(β[0] + β[1] * x_bill) / β[2]
#     plt.plot(x_bill, bd_sample, color='gray', alpha=0.1)
# plt.xlabel('Bill Length')
# plt.ylabel('Mass')
# plt.title('Logistic Regression Decision Boundary')
# plt.grid(True)
# plt.legend()
# plt.show()

#############################################################################