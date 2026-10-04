import numpy as np
import matplotlib.pyplot as plt
from scipy import stats

C_PRIOR, C_LIK, C_POST, C_GRID, C_GREY = "#4C78A8", "#F58518", "#54A24B", "#E45756", "#777777"

plt.rcParams.update({
    "font.size": 12, "axes.spines.top": False, "axes.spines.right": False,
    "axes.titlesize": 13, "axes.labelsize": 12, "legend.frameon": False,
    "figure.dpi": 150,
})

# ---- Running example: failure probability pi of a component ----
a0, b0 = 2, 8                 # prior Beta(2, 8)
n, y = 10, 5                  # 5 failures out of 10 tested units
a1, b1 = a0 + y, b0 + n - y   # exact posterior Beta(7, 13)
THR = 0.3                     # decision threshold
C_FAIL, C_REPL = 100, 30

post = stats.beta(a1, b1)

stat = {}
stat["post_mean"] = post.mean()
stat["p_gt_thr"] = 1 - post.cdf(THR)
stat["ci"] = post.ppf([0.025, 0.975])

def show():
    plt.tight_layout()
    plt.show()

# 1. prior, likelihood, posterior
x = np.linspace(0, 1, 500)

fig, ax = plt.subplots(figsize=(6.2, 3.6))
ax.plot(x, stats.beta(a0, b0).pdf(x), color=C_PRIOR, lw=2.5, label="Prior Beta(2, 8)")
ax.plot(x, stats.beta(y + 1, n - y + 1).pdf(x), color=C_LIK, lw=2.5, ls="--",
        label="Likelihood (rescaled)")
ax.plot(x, post.pdf(x), color=C_POST, lw=2.5, label="Posterior Beta(7, 13)")
ax.axvline(THR, color="k", lw=1, ls=":")
ax.text(THR + 0.01, ax.get_ylim()[1] * 0.93, "threshold 0.3", fontsize=10)
ax.set_xlabel(r"failure probability $\pi$"); ax.set_ylabel("density")
ax.legend(loc="upper right")
show()

# 2. grid with 6 points
g6 = np.linspace(0, 1, 6)
unnorm = stats.beta(a0, b0).pdf(g6) * stats.binom(n, g6).pmf(y)
norm6 = unnorm / unnorm.sum()
stat["grid6_unnorm"] = unnorm
stat["grid6_norm"] = norm6

fig, axs = plt.subplots(1, 2, figsize=(7.4, 3.3))
axs[0].vlines(g6, 0, unnorm, color=C_GRID, lw=3)
axs[0].plot(g6, unnorm, "o", color=C_GRID)
axs[0].set_title("prior × likelihood (unnormalized)")
axs[0].set_xlabel(r"$\pi$"); axs[0].set_ylabel("value")
axs[1].vlines(g6, 0, norm6, color=C_GRID, lw=3)
axs[1].plot(g6, norm6, "o", color=C_GRID, label="grid posterior (sums to 1)")
axs[1].set_title("after dividing by the sum")
axs[1].set_xlabel(r"$\pi$"); axs[1].set_ylabel("probability")
axs[1].legend(loc="upper left", fontsize=9)
plt.tight_layout()
show()

# 3. sampling from the grid: 6 vs 100 points
rng = np.random.default_rng(84735)

def grid_post(m):
    g = np.linspace(0, 1, m)
    u = stats.beta(a0, b0).pdf(g) * stats.binom(n, g).pmf(y)
    return g, u / u.sum()

fig, axs = plt.subplots(1, 2, figsize=(7.4, 3.3), sharey=True)
for ax, m in zip(axs, (6, 101)):
    g, p = grid_post(m)
    draws = rng.choice(g, size=10000, replace=True, p=p)
    ax.hist(draws, bins=np.linspace(-0.01, 1.01, 52) if m > 6 else np.linspace(-0.05, 1.05, 12),
            density=True, color=C_GRID, alpha=0.6, label="10,000 grid draws")
    ax.plot(x, post.pdf(x), color=C_POST, lw=2.5, label="exact")
    ax.set_title(f"grid of {m-1 if m > 6 else m} values" if m > 6 else "grid of 6 values")
    ax.set_xlabel(r"$\pi$")
axs[0].set_ylabel("density"); axs[0].legend(fontsize=9, loc="upper right")
plt.tight_layout()
show()

# 4. risk and decision from the 100-point grid
g, p = grid_post(101)

stat["grid_pgt"] = p[g > THR].sum()
stat["grid_mean"] = (g * p).sum()

fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.4), gridspec_kw={"width_ratios": [1.5, 1]})
axs[0].plot(x, post.pdf(x), color=C_POST, lw=2.5)
xs = x[x > THR]
axs[0].fill_between(xs, post.pdf(xs), color=C_GRID, alpha=0.5)
axs[0].text(0.5, 1.0, f"P(π > 0.3 | data)\n≈ {stat['p_gt_thr']:.2f}", color=C_GRID, fontsize=11)
axs[0].axvline(THR, color="k", ls=":", lw=1)
axs[0].set_xlabel(r"$\pi$"); axs[0].set_ylabel("posterior density")
keep = C_FAIL * post.mean()
axs[1].bar(["Keep design", "Replace"], [keep, C_REPL], color=[C_GRID, C_PRIOR], width=0.55)
for i, v in enumerate([keep, C_REPL]):
    axs[1].text(i, v + 1, f"{v:.0f}", ha="center")
axs[1].set_ylabel("expected cost per unit")
axs[1].set_ylim(0, 45)
plt.tight_layout()
show()

stat["keep_cost"] = keep

# 5. Gamma-Poisson grid
yy = np.array([2, 4, 3])
lam = np.linspace(0, 12, 200)
lik = np.array([np.prod(stats.poisson(l).pmf(yy)) for l in lam])
u = stats.gamma(3, scale=1).pdf(lam) * lik
pl = u / u.sum()
dl = lam[1] - lam[0]

fig, ax = plt.subplots(figsize=(6.2, 3.4))
ax.vlines(lam, 0, pl / dl, color=C_GRID, lw=1.2, alpha=0.8, label="grid approximation")
ax.plot(lam, stats.gamma(12, scale=1 / 4).pdf(lam), color=C_POST, lw=2.5, label="exact Gamma(12, 4)")
ax.set_xlabel(r"failure rate $\lambda$ (failures / month)"); ax.set_ylabel("density")
ax.legend()
show()

# 6. curse of dimensionality
d = np.arange(1, 8)

fig, ax = plt.subplots(figsize=(6.0, 3.3))
ax.bar(d, 100.0 ** d, color=C_PRIOR, width=0.6)
ax.set_yscale("log")
ax.set_xlabel("number of unknown parameters"); ax.set_ylabel("grid points needed")
ax.set_ylim(10, 1e15)
for i, v in zip(d, 100.0 ** d):
    ax.text(i, v * 1.6, f"$10^{{{2*i}}}$", ha="center", fontsize=10)
show()

# ---- Metropolis sampler (same logic as the R function on the slides) ----
def target(pi):
    if pi < 0 or pi > 1:
        return 0.0
    return stats.beta(a0, b0).pdf(pi) * stats.binom(n, pi).pmf(y)

def metropolis(N, w, start, seed):
    r = np.random.default_rng(seed)
    out = np.empty(N)
    cur = start
    fcur = target(cur)
    acc = 0
    for i in range(N):
        prop = r.uniform(cur - w, cur + w)
        fprop = target(prop)
        alpha = min(1.0, fprop / fcur)
        if r.uniform() < alpha:
            cur, fcur = prop, fprop
            acc += 1
        out[i] = cur
    return out, acc / N

# 7. one Metropolis step
cur, prop = 0.30, 0.48
fc, fp = target(cur), target(prop)
xx = x
tgt = np.array([target(v) for v in xx])

fig, ax = plt.subplots(figsize=(6.2, 3.5))
ax.plot(xx, tgt, color=C_POST, lw=2.5, label="prior × likelihood")
ax.plot([cur], [fc], "o", color=C_PRIOR, ms=9)
ax.plot([prop], [fp], "s", color=C_GRID, ms=9)
ax.vlines(cur, 0, fc, color=C_PRIOR, ls="--"); ax.vlines(prop, 0, fp, color=C_GRID, ls="--")
ax.annotate("current\n" + r"$\pi=0.30$", (cur, fc), xytext=(0.08, fc * 0.97), color=C_PRIOR, fontsize=10)
ax.annotate("proposal\n" + r"$\pi'=0.48$", (prop, fp), xytext=(0.55, fp * 2.2), color=C_GRID, fontsize=10)
ax.set_xlabel(r"$\pi$"); ax.set_ylabel("unnormalized posterior")
ax.set_ylim(0, tgt.max() * 1.25)
show()

stat["step_alpha"] = min(1, fp / fc)
stat["step_fc"], stat["step_fp"] = fc, fp

# 8. a single chain
N = 5000
chain, ar = metropolis(N, 0.2, 0.5, 84735)
stat["accept_rate"] = ar

fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.2), gridspec_kw={"width_ratios": [1.6, 1]})
axs[0].plot(np.arange(1, 1001), chain[:1000], color=C_POST, lw=0.8)
axs[0].set_xlabel("iteration"); axs[0].set_ylabel(r"$\pi$"); axs[0].set_title("trace plot (first 1000)")
axs[1].hist(chain, bins=40, density=True, color=C_POST, alpha=0.5, orientation="horizontal",
            label="MCMC")
axs[1].plot(post.pdf(x), x, color="k", lw=1.5, label="exact")
axs[1].set_xlabel("density"); axs[1].set_title("histogram"); axs[1].legend(fontsize=9)
axs[1].set_yticks([])
plt.tight_layout()
show()

# 9. four good chains
starts = [0.05, 0.3, 0.6, 0.9]
cols = [C_PRIOR, C_LIK, C_POST, C_GRID]
good = [metropolis(N, 0.2, s, 100 + i)[0] for i, s in enumerate(starts)]

fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.2), gridspec_kw={"width_ratios": [1.6, 1]})
for c, col in zip(good, cols):
    axs[0].plot(np.arange(1, 501), c[:500], color=col, lw=0.8, alpha=0.9)
    axs[1].hist(c, bins=40, density=True, histtype="step", color=col, lw=1.4)
axs[0].set_xlabel("iteration"); axs[0].set_ylabel(r"$\pi$"); axs[0].set_title("4 chains, different starts")
axs[1].set_xlabel(r"$\pi$"); axs[1].set_title("densities overlay")
plt.tight_layout()
show()

def rhat(chains):
    m, nn = chains.shape
    W = chains.var(axis=1, ddof=1).mean()
    B = nn * chains.mean(axis=1).var(ddof=1)
    return np.sqrt(((nn - 1) / nn * W + B / nn) / W)

def acf(v, lag):
    v = v - v.mean()
    c0 = (v * v).sum()
    return np.array([(v[:len(v) - k] * v[k:]).sum() / c0 for k in range(lag + 1)])

def ess(v):
    r = acf(v, 400)
    s = 0.0
    for k in range(1, len(r)):
        if r[k] < 0.05:
            break
        s += r[k]
    return len(v) / (1 + 2 * s)

good_arr = np.array([c[500:] for c in good])
stat["rhat_good"] = rhat(good_arr)

# 10. bad chains: tiny step size
bad = [metropolis(N, 0.01, s, 200 + i)[0] for i, s in enumerate(starts)]
bad_arr = np.array([c[500:] for c in bad])
stat["rhat_bad"] = rhat(bad_arr)

fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.2), sharey=True)
for c, col in zip(good, cols):
    axs[0].plot(c[:1500], color=col, lw=0.7)
for c, col in zip(bad, cols):
    axs[1].plot(c[:1500], color=col, lw=0.9)
axs[0].set_title(r"healthy: step size $w=0.2$")
axs[1].set_title(r"unhealthy: $w=0.01$ (tiny steps)")
for a in axs:
    a.set_xlabel("iteration")
axs[0].set_ylabel(r"$\pi$")
plt.tight_layout()
show()

# 11. autocorrelation & effective sample size
g_chain, b_chain = good[0][500:], bad[0][500:]

fig, axs = plt.subplots(1, 2, figsize=(7.4, 3.1), sharey=True)
for ax, v, t, col in zip(axs, (g_chain, b_chain), ("healthy chain", "unhealthy chain"), (C_POST, C_GRID)):
    r = acf(v, 40)
    ax.bar(np.arange(41), r, color=col, width=0.7)
    ax.axhline(0, color="k", lw=0.8)
    ax.set_title(t); ax.set_xlabel("lag")
axs[0].set_ylabel("autocorrelation")
plt.tight_layout()
show()

stat["ess_good"] = np.mean([ess(c[500:]) for c in good])
stat["ess_bad"] = np.mean([ess(c[500:]) for c in bad])
stat["N_post"] = N - 500

# 12. decision from MCMC draws
allgood = good_arr.ravel()

stat["mc_mean"] = allgood.mean()
stat["mc_pgt"] = (allgood > THR).mean()
stat["mc_ci"] = np.quantile(allgood, [0.025, 0.975])

fig, ax = plt.subplots(figsize=(6.2, 3.4))
ax.hist(allgood, bins=np.linspace(0, 0.8, 50), color=C_POST, alpha=0.5, density=True)
hist_x = np.linspace(THR, 0.8, 100)
ax.fill_between(hist_x, post.pdf(hist_x), color=C_GRID, alpha=0.5)
ci = stat["mc_ci"]
ax.hlines(0.25, ci[0], ci[1], color="k", lw=3)
ax.text(ci.mean(), 0.45, "95% credible interval", ha="center", fontsize=10)
ax.axvline(THR, color="k", ls=":")
ax.set_xlabel(r"$\pi$"); ax.set_ylabel("density")
show()

print("---- STATS ----")
for k, v in stat.items():
    print(k, np.round(v, 4))

# """Figures for lecture 7"""
# import os
# import numpy as np
# import matplotlib
# matplotlib.use("Agg")
# import matplotlib.pyplot as plt
# from scipy import stats

# OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "figs")
# os.makedirs(OUT, exist_ok=True)

# C_PRIOR, C_LIK, C_POST, C_GRID, C_GREY = "#4C78A8", "#F58518", "#54A24B", "#E45756", "#777777"
# plt.rcParams.update({
#     "font.size": 12, "axes.spines.top": False, "axes.spines.right": False,
#     "axes.titlesize": 13, "axes.labelsize": 12, "legend.frameon": False,
#     "figure.dpi": 150, "savefig.bbox": "tight",
# })

# # ---- Running example: failure probability pi of a component ----
# a0, b0 = 2, 8        # prior Beta(2, 8)
# n, y = 10, 5         # 5 failures out of 10 tested units
# a1, b1 = a0 + y, b0 + n - y   # exact posterior Beta(7, 13)
# THR = 0.3            # decision threshold
# C_FAIL, C_REPL = 100, 30

# post = stats.beta(a1, b1)
# stat = {}
# stat["post_mean"] = post.mean()
# stat["p_gt_thr"] = 1 - post.cdf(THR)
# stat["ci"] = post.ppf([0.025, 0.975])


# def save(name):
#     plt.savefig(os.path.join(OUT, name))
#     plt.close()


# # 1. prior, likelihood, posterior
# x = np.linspace(0, 1, 500)
# fig, ax = plt.subplots(figsize=(6.2, 3.6))
# ax.plot(x, stats.beta(a0, b0).pdf(x), color=C_PRIOR, lw=2.5, label="Prior Beta(2, 8)")
# ax.plot(x, stats.beta(y + 1, n - y + 1).pdf(x), color=C_LIK, lw=2.5, ls="--",
#         label="Likelihood (rescaled)")
# ax.plot(x, post.pdf(x), color=C_POST, lw=2.5, label="Posterior Beta(7, 13)")
# ax.axvline(THR, color="k", lw=1, ls=":")
# ax.text(THR + 0.01, ax.get_ylim()[1] * 0.93, "threshold 0.3", fontsize=10)
# ax.set_xlabel(r"failure probability $\pi$"); ax.set_ylabel("density")
# ax.legend(loc="upper right")
# save("f01_prior_lik_post.png")

# # 2. grid with 6 points
# g6 = np.linspace(0, 1, 6)
# unnorm = stats.beta(a0, b0).pdf(g6) * stats.binom(n, g6).pmf(y)
# norm6 = unnorm / unnorm.sum()
# stat["grid6_unnorm"] = unnorm
# stat["grid6_norm"] = norm6
# fig, axs = plt.subplots(1, 2, figsize=(7.4, 3.3))
# axs[0].vlines(g6, 0, unnorm, color=C_GRID, lw=3)
# axs[0].plot(g6, unnorm, "o", color=C_GRID)
# axs[0].set_title("prior × likelihood (unnormalized)")
# axs[0].set_xlabel(r"$\pi$"); axs[0].set_ylabel("value")
# axs[1].vlines(g6, 0, norm6, color=C_GRID, lw=3)
# axs[1].plot(g6, norm6, "o", color=C_GRID, label="grid posterior (sums to 1)")
# axs[1].set_title("after dividing by the sum")
# axs[1].set_xlabel(r"$\pi$"); axs[1].set_ylabel("probability")
# axs[1].legend(loc="upper left", fontsize=9)
# plt.tight_layout()
# save("f02_grid6.png")

# # 3. sampling from the grid: 6 vs 100 points
# rng = np.random.default_rng(84735)


# def grid_post(m):
#     g = np.linspace(0, 1, m)
#     u = stats.beta(a0, b0).pdf(g) * stats.binom(n, g).pmf(y)
#     return g, u / u.sum()


# fig, axs = plt.subplots(1, 2, figsize=(7.4, 3.3), sharey=True)
# for ax, m in zip(axs, (6, 101)):
#     g, p = grid_post(m)
#     draws = rng.choice(g, size=10000, replace=True, p=p)
#     ax.hist(draws, bins=np.linspace(-0.01, 1.01, 52) if m > 6 else np.linspace(-0.05, 1.05, 12),
#             density=True, color=C_GRID, alpha=0.6, label="10,000 grid draws")
#     ax.plot(x, post.pdf(x), color=C_POST, lw=2.5, label="exact")
#     ax.set_title(f"grid of {m-1 if m > 6 else m} values" if m > 6 else "grid of 6 values")
#     ax.set_xlabel(r"$\pi$")
# axs[0].set_ylabel("density"); axs[0].legend(fontsize=9, loc="upper right")
# plt.tight_layout()
# save("f03_grid_compare.png")

# # 4. risk and decision from the 100-point grid
# g, p = grid_post(101)
# stat["grid_pgt"] = p[g > THR].sum()
# stat["grid_mean"] = (g * p).sum()
# fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.4), gridspec_kw={"width_ratios": [1.5, 1]})
# axs[0].plot(x, post.pdf(x), color=C_POST, lw=2.5)
# xs = x[x > THR]
# axs[0].fill_between(xs, post.pdf(xs), color=C_GRID, alpha=0.5)
# axs[0].text(0.5, 1.0, f"P(π > 0.3 | data)\n≈ {stat['p_gt_thr']:.2f}", color=C_GRID, fontsize=11)
# axs[0].axvline(THR, color="k", ls=":", lw=1)
# axs[0].set_xlabel(r"$\pi$"); axs[0].set_ylabel("posterior density")
# keep = C_FAIL * post.mean()
# axs[1].bar(["Keep design", "Replace"], [keep, C_REPL], color=[C_GRID, C_PRIOR], width=0.55)
# for i, v in enumerate([keep, C_REPL]):
#     axs[1].text(i, v + 1, f"{v:.0f}", ha="center")
# axs[1].set_ylabel("expected cost per unit")
# axs[1].set_ylim(0, 45)
# plt.tight_layout()
# save("f04_risk_grid.png")
# stat["keep_cost"] = keep

# # 5. Gamma-Poisson grid
# yy = np.array([2, 4, 3])
# lam = np.linspace(0, 12, 200)
# u = stats.gamma(3, scale=1).pdf(lam) * np.prod([stats.poisson(l).pmf(yy) for l in lam], axis=1) \
#     if False else None
# lik = np.array([np.prod(stats.poisson(l).pmf(yy)) for l in lam])
# u = stats.gamma(3, scale=1).pdf(lam) * lik
# pl = u / u.sum()
# dl = lam[1] - lam[0]
# fig, ax = plt.subplots(figsize=(6.2, 3.4))
# ax.vlines(lam, 0, pl / dl, color=C_GRID, lw=1.2, alpha=0.8, label="grid approximation")
# ax.plot(lam, stats.gamma(12, scale=1 / 4).pdf(lam), color=C_POST, lw=2.5, label="exact Gamma(12, 4)")
# ax.set_xlabel(r"failure rate $\lambda$ (failures / month)"); ax.set_ylabel("density")
# ax.legend()
# save("f05_gamma_poisson.png")

# # 6. curse of dimensionality
# d = np.arange(1, 8)
# fig, ax = plt.subplots(figsize=(6.0, 3.3))
# ax.bar(d, 100.0 ** d, color=C_PRIOR, width=0.6)
# ax.set_yscale("log")
# ax.set_xlabel("number of unknown parameters"); ax.set_ylabel("grid points needed")
# ax.set_ylim(10, 1e15)
# for i, v in zip(d, 100.0 ** d):
#     ax.text(i, v * 1.6, f"$10^{{{2*i}}}$", ha="center", fontsize=10)
# save("f06_dimension.png")


# # ---- Metropolis sampler (same logic as the R function on the slides) ----
# def target(pi):
#     if pi < 0 or pi > 1:
#         return 0.0
#     return stats.beta(a0, b0).pdf(pi) * stats.binom(n, pi).pmf(y)


# def metropolis(N, w, start, seed):
#     r = np.random.default_rng(seed)
#     out = np.empty(N)
#     cur = start
#     fcur = target(cur)
#     acc = 0
#     for i in range(N):
#         prop = r.uniform(cur - w, cur + w)
#         fprop = target(prop)
#         alpha = min(1.0, fprop / fcur)
#         if r.uniform() < alpha:
#             cur, fcur = prop, fprop
#             acc += 1
#         out[i] = cur
#     return out, acc / N


# # 7. one Metropolis step
# cur, prop = 0.30, 0.48
# fc, fp = target(cur), target(prop)
# xx = x
# tgt = np.array([target(v) for v in xx])
# fig, ax = plt.subplots(figsize=(6.2, 3.5))
# ax.plot(xx, tgt, color=C_POST, lw=2.5, label="prior × likelihood")
# ax.plot([cur], [fc], "o", color=C_PRIOR, ms=9)
# ax.plot([prop], [fp], "s", color=C_GRID, ms=9)
# ax.vlines(cur, 0, fc, color=C_PRIOR, ls="--"); ax.vlines(prop, 0, fp, color=C_GRID, ls="--")
# ax.annotate("current\n" + r"$\pi=0.30$", (cur, fc), xytext=(0.08, fc * 0.97), color=C_PRIOR, fontsize=10)
# ax.annotate("proposal\n" + r"$\pi'=0.48$", (prop, fp), xytext=(0.55, fp * 2.2), color=C_GRID, fontsize=10)
# ax.set_xlabel(r"$\pi$"); ax.set_ylabel("unnormalized posterior")
# ax.set_ylim(0, tgt.max() * 1.25)
# save("f07_metropolis_step.png")
# stat["step_alpha"] = min(1, fp / fc)
# stat["step_fc"], stat["step_fp"] = fc, fp

# # 8. a single chain
# N = 5000
# chain, ar = metropolis(N, 0.2, 0.5, 84735)
# stat["accept_rate"] = ar
# fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.2), gridspec_kw={"width_ratios": [1.6, 1]})
# axs[0].plot(np.arange(1, 1001), chain[:1000], color=C_POST, lw=0.8)
# axs[0].set_xlabel("iteration"); axs[0].set_ylabel(r"$\pi$"); axs[0].set_title("trace plot (first 1000)")
# axs[1].hist(chain, bins=40, density=True, color=C_POST, alpha=0.5, orientation="horizontal",
#             label="MCMC")
# axs[1].plot(post.pdf(x), x, color="k", lw=1.5, label="exact")
# axs[1].set_xlabel("density"); axs[1].set_title("histogram"); axs[1].legend(fontsize=9)
# axs[1].set_yticks([])
# plt.tight_layout()
# save("f08_metropolis_chain.png")

# # 9. four good chains
# starts = [0.05, 0.3, 0.6, 0.9]
# cols = [C_PRIOR, C_LIK, C_POST, C_GRID]
# good = [metropolis(N, 0.2, s, 100 + i)[0] for i, s in enumerate(starts)]
# fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.2), gridspec_kw={"width_ratios": [1.6, 1]})
# for c, col in zip(good, cols):
#     axs[0].plot(np.arange(1, 501), c[:500], color=col, lw=0.8, alpha=0.9)
#     axs[1].hist(c, bins=40, density=True, histtype="step", color=col, lw=1.4)
# axs[0].set_xlabel("iteration"); axs[0].set_ylabel(r"$\pi$"); axs[0].set_title("4 chains, different starts")
# axs[1].set_xlabel(r"$\pi$"); axs[1].set_title("densities overlay")
# plt.tight_layout()
# save("f09_four_chains.png")


# def rhat(chains):
#     m, nn = chains.shape
#     W = chains.var(axis=1, ddof=1).mean()
#     B = nn * chains.mean(axis=1).var(ddof=1)
#     return np.sqrt(((nn - 1) / nn * W + B / nn) / W)


# def acf(v, lag):
#     v = v - v.mean()
#     c0 = (v * v).sum()
#     return np.array([(v[:len(v) - k] * v[k:]).sum() / c0 for k in range(lag + 1)])


# def ess(v):
#     r = acf(v, 400)
#     s = 0.0
#     for k in range(1, len(r)):
#         if r[k] < 0.05:
#             break
#         s += r[k]
#     return len(v) / (1 + 2 * s)


# good_arr = np.array([c[500:] for c in good])
# stat["rhat_good"] = rhat(good_arr)

# # 10. bad chains: tiny step size
# bad = [metropolis(N, 0.01, s, 200 + i)[0] for i, s in enumerate(starts)]
# bad_arr = np.array([c[500:] for c in bad])
# stat["rhat_bad"] = rhat(bad_arr)
# fig, axs = plt.subplots(1, 2, figsize=(7.6, 3.2), sharey=True)
# for c, col in zip(good, cols):
#     axs[0].plot(c[:1500], color=col, lw=0.7)
# for c, col in zip(bad, cols):
#     axs[1].plot(c[:1500], color=col, lw=0.9)
# axs[0].set_title(r"healthy: step size $w=0.2$")
# axs[1].set_title(r"unhealthy: $w=0.01$ (tiny steps)")
# for a in axs:
#     a.set_xlabel("iteration")
# axs[0].set_ylabel(r"$\pi$")
# plt.tight_layout()
# save("f10_bad_chains.png")

# # 11. autocorrelation & effective sample size
# g_chain, b_chain = good[0][500:], bad[0][500:]
# fig, axs = plt.subplots(1, 2, figsize=(7.4, 3.1), sharey=True)
# for ax, v, t, col in zip(axs, (g_chain, b_chain), ("healthy chain", "unhealthy chain"), (C_POST, C_GRID)):
#     r = acf(v, 40)
#     ax.bar(np.arange(41), r, color=col, width=0.7)
#     ax.axhline(0, color="k", lw=0.8)
#     ax.set_title(t); ax.set_xlabel("lag")
# axs[0].set_ylabel("autocorrelation")
# plt.tight_layout()
# save("f11_acf.png")
# stat["ess_good"] = np.mean([ess(c[500:]) for c in good])
# stat["ess_bad"] = np.mean([ess(c[500:]) for c in bad])
# stat["N_post"] = N - 500

# # 12. decision from MCMC draws
# allgood = good_arr.ravel()
# stat["mc_mean"] = allgood.mean()
# stat["mc_pgt"] = (allgood > THR).mean()
# stat["mc_ci"] = np.quantile(allgood, [0.025, 0.975])
# fig, ax = plt.subplots(figsize=(6.2, 3.4))
# ax.hist(allgood, bins=np.linspace(0, 0.8, 50), color=C_POST, alpha=0.5, density=True)
# hist_x = np.linspace(THR, 0.8, 100)
# ax.fill_between(hist_x, post.pdf(hist_x), color=C_GRID, alpha=0.5)
# ci = stat["mc_ci"]
# ax.hlines(0.25, ci[0], ci[1], color="k", lw=3)
# ax.text(ci.mean(), 0.45, "95% credible interval", ha="center", fontsize=10)
# ax.axvline(THR, color="k", ls=":")
# ax.set_xlabel(r"$\pi$"); ax.set_ylabel("density")
# save("f12_mcmc_decision.png")

# print("---- STATS ----")
# for k, v in stat.items():
#     print(k, np.round(v, 4))
