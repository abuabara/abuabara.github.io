# pyright: reportMissingImports=false

import numpy as np
import matplotlib.pyplot as plt
import arviz as az
import pymc as pm
from scipy import stats

# simulate data
np.random.seed(42)
n = 20
θ_real = 0.7
# Y = stats.bernoulli(θ_real).rvs(n)
Y = np.random.binomial(1, θ_real, size=n)
Y

# posterior function
def post(θ, Y, α = 1, β = 1):
    if 0 <= θ <= 1:
        prior = stats.beta(α, β).pdf(θ)           # belief about θ before seeing data
        like  = stats.bernoulli(θ).pmf(Y).prod()  # likelihood of the data Y at this θ
        prob  = like * prior                      # un-normalised posterior
    else:
        prob = -np.inf                            # impossible θ values
    return prob

# metropolis-hastings
n_iters = 1000                                    # how many draws for the Markov chain
can_sd  = 0.05                                    # proposal std-dev
α = β = 1                                         # Beta prior hyper-parameters
θ = 0.5                                           # starting guess
trace = {'θ': np.zeros(n_iters)}                  # to store draws
p2 = post(θ, Y, α, β)                             # posterior probability at current θ

for iter in range(n_iters):
    θ_can = stats.norm(θ, can_sd).rvs()           # propose a new θ (little random nudge)
    p1 = post(θ_can, Y, α, β)                     # posterior at candidate
    pa = p1 / p2                                  # acceptance ratio
    if pa > stats.uniform(0, 1).rvs():            # accept with probability pa
        θ = θ_can
        p2 = p1                                   # update reference probability
    trace['θ'][iter] = θ                          # record current draw

az.summary(trace, kind='stats', round_to=2)

az.plot_posterior(trace)

# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_posterior_DIY.png', dpi=300, bbox_inches='tight')

# trace plot + histogram
fig, axes = plt.subplots(1, 2, sharey=True, figsize=(10, 4))

# trace plot (θ over iterations)
axes[0].plot(trace['θ'], '0.5', c='#bda8a8')
axes[0].axhline(np.mean(trace['θ']), color='red', linestyle='--', linewidth=1.5)
axes[0].set_ylabel('θ', rotation=0, labelpad=15)
axes[0].set_ylim(0, 1)
axes[0].set_title('Trace Plot of θ')

# histogram of sampled θ values
axes[1].hist(trace['θ'], color='#bda8a8', edgecolor='black',
        orientation='horizontal', density=True)
axes[1].axhline(np.mean(trace['θ']), color='red', linestyle='--', linewidth=1.5)
axes[1].set_xticks([])
axes[1].set_title('Frequency')

# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_trace_theta_DIY.png', dpi=300, bbox_inches='tight')

# PyMC model
with pm.Model() as model:
    # prior for θ
    θ = pm.Beta('θ', alpha=1, beta=1)
    # likelihood
    Y_obs = pm.Bernoulli('Y_obs', p=θ, observed=Y)
    # inference using MCMC (NUTS, which is more efficient than MH)
    # trace = pm.sample(1000, tune=500, return_inferencedata=True, random_seed=42)
    trace = pm.sample(draws=1000, tune=0, return_inferencedata=True, chains=1, random_seed=42)
    idata = pm.sample(1000, return_inferencedata=True)

# graphviz = pm.model_to_graphviz(model)
# graphviz
# graphviz.graph_attr.update(dpi='300')
# graphviz.render('/Users/alexander/Desktop/DAEN 427/BetaBinomModelGraphViz', format='png')

# Declare a model in PyMC3
with pm.Model() as model:
    # Specify the prior distribution of unknown parameter
    θ = pm.Beta("θ", alpha=1, beta=1)

    # Specify the likelihood distribution and condition on the observed data
    y_obs = pm.Binomial("y_obs", n=1, p=θ, observed=Y)

    # Sample from the posterior distribution
    idata = pm.sample(1000)

# graphviz = pm.model_to_graphviz(model)
# graphviz

pred_dists = (pm.sample_prior_predictive(1000, model).prior_predictive["y_obs"].values,
              pm.sample_posterior_predictive(idata, model).posterior_predictive["y_obs"].values)

fig, axes = plt.subplots(4, 1, figsize=(9, 9))

for idx, n_d, dist in zip((1, 3), ("Prior", "Posterior"), pred_dists):
    az.plot_dist(dist.sum(-1), 
                 hist_kwargs={"color":"0.5", "bins":range(0, 22)},
                 ax=axes[idx])
    axes[idx].set_title(f"{n_d} predictive distribution", fontweight='bold')
    axes[idx].set_xlim(-1, 21)
    axes[idx].set_ylim(0, 0.15)
    axes[idx].set_xlabel("number of success")

az.plot_dist(pm.draw(θ, 1000), plot_kwargs={"color":"0.5"},
             fill_kwargs={'alpha':1}, ax=axes[0])

axes[0].set_title("Prior distribution", fontweight='bold')
axes[0].set_xlim(0, 1)
axes[0].set_ylim(0, 4)
axes[0].tick_params(axis='both', pad=7)
axes[0].set_xlabel("θ")

az.plot_dist(idata.posterior["θ"], plot_kwargs={"color":"0.5"},
             fill_kwargs={'alpha':1}, ax=axes[2])

axes[2].set_title("Posterior distribution", fontweight='bold')
axes[2].set_xlim(0, 1)
axes[2].set_ylim(0, 5)
axes[2].tick_params(axis='both', pad=7)
axes[2].set_xlabel("θ")

# plt.savefig("img/chp01/Bayesian_quartet_distributions.png")

predictions = (stats.binom(n=1, p=idata.posterior["θ"].mean()).rvs((4000, len(Y))), pred_dists[1])

for d, c, l in zip(predictions, ("C0", "C4"), ("posterior mean", "posterior predictive")):
    ax = az.plot_dist(d.sum(-1),
                      label=l,
                      figsize=(10, 5),
                      hist_kwargs={"alpha": 0.5, "color":c, "bins":range(0, 22)})
    ax.set_yticks([])
    ax.set_xlabel("number of success")

# plt.savefig("img/chp01/predictions_distributions.png")

# summary and posterior plot
print(az.summary(trace, kind='stats', round_to=2))

az.plot_posterior(trace, var_names=['θ'])

# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_posterior_PYMC.png', dpi=300, bbox_inches='tight')

# extract θ samples from posterior
θ_samples = trace.posterior['θ'].values.flatten()

# trace plot + histogram
fig, axes = plt.subplots(1, 2, sharey=True, figsize=(10, 4))

# Trace plot (θ over iterations)
axes[0].plot(θ_samples, color='#bda8a8')
axes[0].axhline(np.mean(θ_samples), color='red', linestyle='--', linewidth=1.5)
axes[0].set_ylabel('θ', rotation=0, labelpad=15)
axes[0].set_ylim(0, 1)
axes[0].set_title('Trace Plot of θ')

# Histogram of sampled θ values
axes[1].hist(θ_samples, color='#bda8a8', edgecolor='black',
             orientation='horizontal', density=True)
axes[1].axhline(np.mean(θ_samples), color='red', linestyle='--', linewidth=1.5)
axes[1].set_xticks([])
axes[1].set_title('Frequency')

# plt.savefig('/Users/alexander/Desktop/DAEN 427/plot_trace_theta_PYMC.png', dpi=300, bbox_inches='tight')