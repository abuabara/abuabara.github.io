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
# logistic regression
# 8. prediction / visualization
# 9. bayesian linear regression with PyMC
# 10. bayesian linear model predictions
# 11. bayesian logistic regression with PyMC
# 12. posterior summaries and diagnostics
# 13. posterior predictive / fitted curve over x_range
# 14. compare frequentist and Bayesian fits visually