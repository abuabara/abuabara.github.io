import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

plt.rcParams.update({"font.size": 13, "axes.spines.top": False, "axes.spines.right": False})
BLUE, ORANGE, GRAY = "#2B6CB0", "#DD6B20", "#718096"
rng = np.random.default_rng(84735)
logit_inv = lambda z: 1 / (1 + np.exp(-z))
out = "/home/claude/deck/"

# 1. Why not a straight line? Simulated 0/1 rain data + line vs S-curve
h = rng.uniform(15, 100, 160)
y = rng.binomial(1, logit_inv(-4.6 + 0.048 * h))
x = np.linspace(0, 100, 300)
b = np.polyfit(h, y, 1)
fig, ax = plt.subplots(figsize=(6.4, 3.6))
ax.scatter(h, y + rng.normal(0, 0.025, len(y)), s=14, alpha=.45, color=GRAY, label="days (0 = dry, 1 = rain)")
ax.plot(x, np.polyval(b, x), color=ORANGE, lw=2.5, ls="--", label="straight line")
ax.plot(x, logit_inv(-4.6 + 0.048 * x), color=BLUE, lw=3, label="logistic S-curve")
ax.axhspan(-0.3, 0, color=ORANGE, alpha=.08); ax.axhspan(1, 1.3, color=ORANGE, alpha=.08)
ax.set_ylim(-0.25, 1.55); ax.set_yticks([0,0.5,1]); ax.set_xlabel("9 a.m. humidity (%)"); ax.set_ylabel("rain tomorrow?")
ax.legend(frameon=False, fontsize=10, loc="upper center", ncol=3)
fig.tight_layout(); fig.savefig(out + "line_vs_s.pdf"); plt.close()

# 2. Three scales: probability, odds, log-odds
p = np.linspace(0.01, 0.99, 300)
fig, axs = plt.subplots(1, 2, figsize=(6.6, 2.9))
axs[0].plot(p, p / (1 - p), color=BLUE, lw=2.5); axs[0].set_ylim(0, 20)
axs[0].set_xlabel(r"probability $\pi$"); axs[0].set_title("odds: 0 to $\\infty$", fontsize=12)
axs[1].plot(p, np.log(p / (1 - p)), color=ORANGE, lw=2.5); axs[1].axhline(0, color=GRAY, lw=.8)
axs[1].set_xlabel(r"probability $\pi$"); axs[1].set_title("log-odds: $-\\infty$ to $\\infty$", fontsize=12)
for a in axs: a.axvline(.5, color=GRAY, lw=.8, ls=":")
fig.tight_layout(); fig.savefig(out + "scales.pdf"); plt.close()

# 3. Prior simulation: curves from priors (centered intercept at humidity ~ 60)
xc = 60
fig, ax = plt.subplots(figsize=(6.4, 3.4))
for _ in range(100):
    b0c = rng.normal(-1.4, 0.7); b1 = rng.normal(0.07, 0.035)
    ax.plot(x, logit_inv(b0c + b1 * (x - xc)), color=BLUE, alpha=.15, lw=1)
ax.plot(x, logit_inv(-1.4 + 0.07 * (x - xc)), color=ORANGE, lw=3, label="prior mean model")
ax.set_xlabel("9 a.m. humidity (%)"); ax.set_ylabel("P(rain tomorrow)"); ax.set_ylim(0, 1)
ax.legend(frameon=False, loc="upper left")
fig.tight_layout(); fig.savefig(out + "prior_curves.pdf"); plt.close()

# 4. Posterior curves (approximate: posterior centered near reported 80% CIs)
fig, ax = plt.subplots(figsize=(6.4, 3.4))
for _ in range(100):
    b1 = rng.normal(0.0482, 0.0052)
    b0c = rng.normal(-4.61 + 0.0482 * xc, 0.09)      # well-pinned at average humidity
    ax.plot(x, logit_inv(b0c + b1 * (x - xc)), color=BLUE, alpha=.12, lw=1)
ax.plot(x, logit_inv(-4.61 + 0.0482 * x), color=ORANGE, lw=3, label="posterior median model")
ax.scatter([99], [logit_inv(-4.61 + 0.0482 * 99)], s=70, color="black", zorder=5)
ax.annotate("99% humidity:\n$\\pi \\approx 0.54$", (99, 0.54), (66, 0.72), fontsize=11,
            arrowprops=dict(arrowstyle="->", color="black"))
ax.set_xlabel("9 a.m. humidity (%)"); ax.set_ylabel("P(rain tomorrow)"); ax.set_ylim(0, 1)
ax.legend(frameon=False, loc="upper left")
fig.tight_layout(); fig.savefig(out + "posterior_curves.pdf"); plt.close()

# 5. Cutoff trade-off (book's numbers)
fig, ax = plt.subplots(figsize=(6.4, 3.0))
labels = ["Accuracy", "Sensitivity", "Specificity"]
c50 = [81.7, 7.53, 98.65]; c20 = [69.9, 63.98, 71.25]
ix = np.arange(3); w = .36
b1_ = ax.bar(ix - w/2, c50, w, color=GRAY, label="cutoff $c = 0.5$")
b2_ = ax.bar(ix + w/2, c20, w, color=BLUE, label="cutoff $c = 0.2$")
for bars in (b1_, b2_):
    for r in bars:
        ax.text(r.get_x() + r.get_width()/2, r.get_height() + 2, f"{r.get_height():.0f}%", ha="center", fontsize=11)
ax.set_xticks(ix, labels); ax.set_ylim(0, 130); ax.set_yticks([])
ax.spines["left"].set_visible(False)
ax.legend(frameon=False, loc="upper center", ncol=2)
fig.tight_layout(); fig.savefig(out + "cutoff.pdf"); plt.close()
print("done")
