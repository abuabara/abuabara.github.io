import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pymc as pm
import arviz as az

# penguins = pd.read_csv("penguins.csv")
penguins = pd.read_csv('/Users/abuabara/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Teaching/2025:2026/2025 Fall/427/My_files/Code/data/penguins.csv')
penguins = penguins.dropna(axis=0, how='any', inplace=False)
penguins.head()

adelie_mask = (penguins["species"] == "Adelie")
adelie_mass_obs = penguins.loc[adelie_mask, "body_mass_g"].values
adelie_flipper_length_obs = penguins.loc[adelie_mask, "flipper_length_mm"]

with pm.Model() as model_adelie_flipper_regression:
    adelie_flipper_length = pm.Data("adelie_flipper_length",
                                    adelie_flipper_length_obs)
    sigma = pm.HalfStudentT("sigma", 100, 2000)
    beta_0 = pm.Normal("beta_0", 0, 4000)
    beta_1 = pm.Normal("beta_1", 0, 4000)
    mu = pm.Deterministic("mu", beta_0 + beta_1 * adelie_flipper_length)

    mass = pm.Normal("mass", mu=mu, sigma=sigma, observed=adelie_mass_obs)

    inf_data_adelie_flipper_regression = pm.sample(return_inferencedata=True)

graphviz = pm.model_to_graphviz(model_adelie_flipper_regression)
graphviz

axes = az.plot_posterior(inf_data_adelie_flipper_regression,
                         var_names=["beta_0", "beta_1"], textsize=20)
# plt.savefig("adelie_coefficient_posterior_plots")

beta0_m = inf_data_adelie_flipper_regression.posterior.mean().to_dict()["data_vars"]["beta_0"]["data"]
beta1_m = inf_data_adelie_flipper_regression.posterior.mean().to_dict()["data_vars"]["beta_1"]["data"]

flipper_length = np.linspace(adelie_flipper_length_obs.min(),
                            adelie_flipper_length_obs.max(), 100)

flipper_length_mean = beta0_m + beta1_m * flipper_length

fig, ax = plt.subplots()
ax.plot(flipper_length, flipper_length_mean, c='C4',
        label=f'y = {beta0_m:.2f} + {beta1_m:.2f} * x')
ax.scatter(adelie_flipper_length_obs, adelie_mass_obs)
az.plot_hdi(adelie_flipper_length_obs,
            inf_data_adelie_flipper_regression.posterior['mu'],
            hdi_prob=0.94, color='r', ax=ax)
ax.set_xlabel('Flipper Length')
ax.set_ylabel('Mass')

species_filter = penguins["species"].isin(["Adelie", "Chinstrap"])
bill_length_obs = penguins.loc[species_filter, "bill_length_mm"].values
species = pd.Categorical(penguins.loc[species_filter, "species"])

with pm.Model() as model_logistic_penguins_bill_length:
    beta_0 = pm.Normal("beta_0", mu=0, sigma=10)
    beta_1 = pm.Normal("beta_1", mu=0, sigma=10)

    mu = beta_0 + pm.math.dot(bill_length_obs, beta_1)
    theta = pm.Deterministic("theta", pm.math.sigmoid(mu))

    yl = pm.Bernoulli("yl", p=theta, observed=species.codes)
    
    bd = pm.Deterministic("bd", -beta_0/beta_1)
    idata_logistic_penguins_bill_length = pm.sample(
        5000, chains=2, idata_kwargs={"log_likelihood": True})
    idata_logistic_penguins_bill_length.extend(
        pm.sample_prior_predictive(samples=10000))
    idata_logistic_penguins_bill_length.extend(
        pm.sample_posterior_predictive(idata_logistic_penguins_bill_length))

graphviz = pm.model_to_graphviz(model_logistic_penguins_bill_length)
graphviz

colors = ['blue', 'red']
fig, ax = plt.subplots(figsize=(6, 4))
plt.ylim(-0.075, 1.075)
theta = idata_logistic_penguins_bill_length.posterior['theta'].mean(('chain', 'draw'))
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
az.plot_hdi(bill_length_obs, idata_logistic_penguins_bill_length.posterior['theta'].values,
            color='C4', ax=ax, plot_kwargs={'zorder':10})
ax.set_xlabel('Bill Length (mm)')
ax.set_ylabel('θ', rotation=0)
ax.legend(loc=5)