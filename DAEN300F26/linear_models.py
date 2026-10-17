# 1. DATA
import numpy as np
x = np.array([1, 2, 3, 4, 5, 6, 7, 8], dtype=float)
y = np.array([2, 3, 5, 4, 6, 8, 7, 9], dtype=float)

# 2. NUMPY — LEAST SQUARES
# Regression equation:
# y_hat = b0 + b1*x

# Matrix: column of ones for the intercept
X = np.column_stack((np.ones(len(x)), x))

# Solve the least-squares problem
beta = np.linalg.lstsq(X, y, rcond=None)[0]

intercept, slope = beta

print("Intercept:", intercept)
print("Slope:", slope)

# Predictions and residuals
y_pred = X @ beta
residuals = y - y_pred

# 3. MODEL EVALUATION
# Sum of Squared Errors
sse = np.sum(residuals**2)

# Total Sum of Squares
sst = np.sum((y - np.mean(y))**2)

# Residual Mean Squared Error (n - 2 degrees of freedom)
mse_residual = sse / (len(x) - 2)

# R-squared
r_squared = 1 - sse / sst

print("\nMODEL EVALUATION")
print("SSE:", sse)
print("SST:", sst)
print("Residual MSE:", mse_residual)
print("R-squared:", r_squared)

# 4. PANDAS
import pandas as pd

df = pd.DataFrame({
    "x": x,
    "y": y
})

check = pd.DataFrame({
    "x": x,
    "Observed": y,
    "Predicted": y_pred,
    "Residual": residuals
})

print("\nOBSERVED AND PREDICTED VALUES")
print(check.round(3))

# 5. STATSMODELS — STATISTICAL INFERENCE
import statsmodels.api as sm

# X already contains the intercept column
model_sm = sm.OLS(y, X).fit()

print("\nSTATSMODELS — MATRIX INTERFACE")
print(model_sm.summary())

# Formula interface (similar to R)
import statsmodels.formula.api as smf

model_formula = smf.ols("y ~ x", data=df).fit()

print("\nSTATSMODELS — FORMULA INTERFACE")
print(model_formula.summary())

# 6. HOW CERTAIN IS THE ESTIMATED SLOPE?
n = len(x)

# Residual standard error
s = np.sqrt(sse / (n - 2))

# Standard error of the estimated slope
se_slope = s / np.sqrt(
    np.sum((x - np.mean(x))**2)
)

print("\nSLOPE UNCERTAINTY")
print("Residual standard error:", s)
print("Slope standard error:", se_slope)

# Compare with statsmodels
print("Statsmodels slope SE:", model_sm.bse[1])

# 95% confidence interval for the slope
ci_slope = model_sm.conf_int(alpha=0.05)[1]

print("95% confidence interval:", ci_slope)

# 7. SCIKIT-LEARN — MACHINE LEARNING
from sklearn.linear_model import LinearRegression

# Reshape x to (observations, features)
X_sklearn = x.reshape(-1, 1)

# Initialize and fit model
lr_model = LinearRegression(fit_intercept=True)
lr_model.fit(X_sklearn, y)

print("Intercept:", lr_model.intercept_)
print("Slope:", lr_model.coef_[0])
print("Training R-squared:", lr_model.score(X_sklearn, y))

# Predictions
pred_sklearn = lr_model.predict(X_sklearn)

# 8. REGRESSION VISUALIZATION
import seaborn as sns
import matplotlib.pyplot as plt

# Scatterplot, fitted regression line,
# and 95% confidence band for mean response

sns.lmplot(
    data=df,
    x="x",
    y="y",
    ci=95,
    height=5,
    aspect=1.2
)

plt.xlabel("Hours")
plt.ylabel("Output (hundreds of units)")
plt.title("Simple Linear Regression")
plt.show()

# 9. CVXPY — REGRESSION AS CONVEX OPTIMIZATION
# %pip install cvxpy
import cvxpy as cp

# Decision variables: intercept and slope
beta_cvx = cp.Variable(2)

# Objective: minimize sum of squared errors
objective = cp.Minimize(
    cp.sum_squares(X @ beta_cvx - y)
)

# Define optimization problem
problem = cp.Problem(objective)

# Solve the problem
problem.solve()

print("Status:", problem.status)
print("Intercept:", beta_cvx.value[0])
print("Slope:", beta_cvx.value[1])
print("SSE:", problem.value)

# 10. FROM LEAST SQUARES TO GRADIENT DESCENT
# All approaches minimize squared prediction errors.
# NumPy:
#   Uses numerical linear algebra to solve least squares.
#
# CVXPY:
#   Defines an optimization objective and calls a solver.
#
# PyTorch:
#   Uses gradients to update the parameters repeatedly.
#
# The PyTorch loss is MSE = SSE / n.
# Dividing by n does not change the optimal coefficients.
#
# Gradient descent update:
#   parameter = parameter - learning_rate * gradient
#
# The goal is still to estimate:
#   y_hat = intercept + slope*x

# 11. PYTORCH / GRADIENT DESCENT
import torch
import torch.nn as nn

# Reproducible initialization
torch.manual_seed(42)

# Convert NumPy arrays into PyTorch tensors
X_tensor = torch.tensor(
    x.reshape(-1, 1),
    dtype=torch.float32
)

Y_tensor = torch.tensor(
    y.reshape(-1, 1),
    dtype=torch.float32
)

# Step 1: Define linear model
# One input feature and one output
# y_hat = weight*x + bias

linear_layer = nn.Linear(1, 1)

# Step 2: Define loss function
criterion = nn.MSELoss()

# Step 3: Define optimizer
optimizer = torch.optim.SGD(
    linear_layer.parameters(),
    lr=0.01
)

# Step 4: Training loop
epochs = 2000
loss_history = []

for epoch in range(epochs):
    # Forward pass
    predictions = linear_layer(X_tensor)

    # Calculate MSE
    loss = criterion(predictions, Y_tensor)

    # Reset gradients
    optimizer.zero_grad()

    # Backpropagation
    loss.backward()

    # Update model parameters
    optimizer.step()

    # Save loss for visualization
    loss_history.append(loss.item())

# Step 5: Extract trained parameters
intercept_torch = linear_layer.bias.item()
slope_torch = linear_layer.weight.item()

# Evaluate loss after the final parameter update
with torch.no_grad():
    final_loss = criterion(
        linear_layer(X_tensor), Y_tensor
    ).item()

print("\nPYTORCH")
print("Intercept:", intercept_torch)
print("Slope:", slope_torch)
print("Final MSE:", final_loss)

# 12. VISUALIZE GRADIENT DESCENT CONVERGENCE
plt.figure(figsize=(7, 4))
plt.plot(loss_history)
plt.xlabel("Epoch")
plt.ylabel("Mean Squared Error")
plt.title("Gradient Descent: Training Loss")
plt.grid(True, alpha=0.3)
plt.show()

# 13. COMPARE ALL REGRESSION METHODS
results = pd.DataFrame({
    "Method": [
        "NumPy",
        "statsmodels",
        "statsmodels Formula",
        "scikit-learn",
        "CVXPY",
        "PyTorch (SGD)"
    ],
    "Intercept": [
        beta[0],
        model_sm.params[0],
        model_formula.params["Intercept"],
        lr_model.intercept_,
        beta_cvx.value[0],
        intercept_torch
    ],
    "Slope": [
        beta[1],
        model_sm.params[1],
        model_formula.params["x"],
        lr_model.coef_[0],
        beta_cvx.value[1],
        slope_torch
    ]
})

print(results.round(4))

# 14. REGRESSION DIAGNOSTICS
# 1. Residuals vs. fitted values plot
# 2. Q-Q plot of the regression residuals
# Use the textbook, statsmodels documentation, or other online resources.
# Explain what each diagnostic plot tells you about the regression assumptions.