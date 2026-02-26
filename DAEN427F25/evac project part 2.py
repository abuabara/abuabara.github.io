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
    os.chdir('/Users/abuabara/GitHub/abuabara.github.io/DAEN427F25')

os.chdir('/Users/abuabara/Documents/GitHub/abuabara.github.io/DAEN427F25')
# /Users/abuabara/Library/Mobile Documents/com~apple~CloudDocs/TAMU/Teaching/2025:2026/2025 Fall/427/My_files/Project

survey = pd.read_csv('survey_short.csv')

# confirm load
print(survey.head())
print(type(survey))

# in case it's not a dataframe:
# dat_dicretized = pd.DataFrame(dat_dicretized)

# 3. data prep
# recoding categorical variables into numeric
a