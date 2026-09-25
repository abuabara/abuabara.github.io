import pandas as pd
import numpy as np

# https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
# home
# df = pd.read_csv('/Users/alexander/Downloads/DisasterDeclarationsSummaries.csv')

# tamu
df = pd.read_csv('/Users/abuabara/Downloads/DisasterDeclarationsSummaries.csv')

df.info()
# <class 'pandas.DataFrame'>
# RangeIndex: 70405 entries, 0 to 70404
# Data columns (total 29 columns):
#  #   Column                    Non-Null Count  Dtype
# ---  ------                    --------------  -----
#  0   femaDeclarationString     70405 non-null  str  
#  1   disasterNumber            70405 non-null  int64
#  2   state                     70405 non-null  str  
#  3   declarationType           70405 non-null  str  
#  4   declarationDate           70405 non-null  str  
#  5   fyDeclared                70405 non-null  int64
#  6   incidentType              70405 non-null  str  
#  7   declarationTitle          70405 non-null  str  
#  8   ihProgramDeclared         70405 non-null  int64
#  9   iaProgramDeclared         70405 non-null  int64
#  10  paProgramDeclared         70405 non-null  int64
#  11  hmProgramDeclared         70405 non-null  int64
#  12  incidentBeginDate         70405 non-null  str  
#  13  incidentEndDate           69786 non-null  str  
#  14  disasterCloseoutDate      52642 non-null  str  
#  15  tribalRequest             70405 non-null  int64
#  16  fipsStateCode             70405 non-null  int64
#  17  fipsCountyCode            70405 non-null  int64
#  18  placeCode                 70405 non-null  int64
#  19  designatedArea            70405 non-null  str  
#  20  declarationRequestNumber  70405 non-null  int64
#  21  declarationRequestDate    70395 non-null  str  
#  22  lastIAFilingDate          19783 non-null  str  
#  23  incidentId                70405 non-null  int64
#  24  region                    70405 non-null  int64
#  25  designatedIncidentTypes   22593 non-null  str  
#  26  lastRefresh               70405 non-null  str  
#  27  hash                      70405 non-null  str  
#  28  id                        70405 non-null  str  
# dtypes: int64(13), str(16)
# memory usage: 34.9 MB

# df[['lastRefresh', "hash"]]

df = df.drop(columns=["lastRefresh", "hash", "id"])

df.info()

# df.head(3)
# df.sample(3)

# df.columns
# ['femaDeclarationString', 'disasterNumber', 'state', 'declarationType',
#        'declarationDate', 'fyDeclared', 'incidentType', 'declarationTitle',
#        'ihProgramDeclared', 'iaProgramDeclared', 'paProgramDeclared',
#        'hmProgramDeclared', 'incidentBeginDate', 'incidentEndDate',
#        'disasterCloseoutDate', 'tribalRequest', 'fipsStateCode',
#        'fipsCountyCode', 'placeCode', 'designatedArea',
#        'declarationRequestNumber', 'declarationRequestDate',
#        'lastIAFilingDate', 'incidentId', 'region', 'designatedIncidentTypes',
#        'lastRefresh', 'hash', 'id']

(
    df
    .groupby("declarationType")
    .size()
    .reset_index(name="count")
    .sort_values("count", ascending=False)
    .reset_index(drop=True)
)

# declarationType	count
# 0	DR	46675
# 1	EM	21568
# 2	FM	2162

(
    df
    .query("declarationType == 'DR'")
    .groupby(["state"])
    .size()
    .reset_index(name="count")
    .sort_values("count", ascending=False)
    .reset_index(drop=True)
    .head(11)
)

# 	 state	count
# 0	 TX	    3083
# 1	 KY	    2565
# 2	 MO	    2100
# 3	 OK	    1791
# 4	 VA	    1759
# 5	 IA	    1691
# 6	 FL	    1629
# 7	 KS	    1586
# 8	 TN	    1556
# 9	 MS	    1535
# 10 LA	    1518

(
    df
    .query("declarationType == 'DR'")
    .groupby(["declarationTitle"])
    .size()
    .reset_index(name="count")
    .sort_values("count", ascending=False)
    .reset_index(drop=True)
    .head(10)
)

# 	declarationTitle	                               count
# 0	COVID-19 PANDEMIC	                               4165 <-----
# 1	SEVERE STORMS AND FLOODING	                       3883
# 2	SEVERE STORMS & FLOODING	                       3381
# 3	SEVERE STORMS, TORNADOES, AND FLOODING	           2049
# 4	SEVERE WINTER STORM	                               1424
# 5	FLOODING	                                       1317
# 6	SEVERE WINTER STORMS	                           1077
# 7	SEVERE STORMS, TORNADOES, STRAIGHT-LINE WINDS,...  1072
# 8	SEVERE STORMS, TORNADOES & FLOODING	                971
# 9	SEVERE STORMS, STRAIGHT-LINE WINDS, TORNADOES,...	855

(
    df
    .query("declarationType == 'DR'")
    .groupby(["incidentType"])
    .size()
    .reset_index(name="count")
    .sort_values("count", ascending=False)
    .reset_index(drop=True)
)

#     	incidentType		count
# 0		Severe Storm		17951
# 1		Flood				10865
# 2		Hurricane			5993
# 3		Biological			4165 <----- COVID
# 4		Severe Ice Storm	1793
# 5		Snowstorm			1572
# 6		Tornado				1518
# 7		Fire				1076
# 8		Winter Storm		361
# 9		Tropical Storm		330
# 10	Freezing			301
# 11	Coastal Storm		177
# 12	Earthquake			146
# 13	Typhoon				119
# 14	Drought				87
# 15	Volcanic Eruption	51
# 16	Straight-Line Winds	49
# 17	Mud/Landslide		43
# 18	Fishing Losses		34 <-----
# 19	Other				14 <-----
# 20	Tsunami				9
# 21	Dam/Levee Break		8
# 22	Tropical Depression	7
# 23	Toxic Substances	3 <-----
# 24	Human Cause			2 <-----
# 25	Terrorist			1 <-----

excluded_incidents = [
    "Biological",
    "Fishing Losses",
    "Other",
    "Toxic Substances",
    "Human Cause",
    "Terrorist"
]

(
    df
    .query("declarationType == 'DR'"
           "and incidentType not in @excluded_incidents")
    .groupby(["incidentType"])    
    .size()
    .reset_index(name="count")
    .sort_values("count", ascending=False)
    .reset_index(drop=True)
)

(
    df
    .query("declarationType == 'DR'"
           "and incidentType not in @excluded_incidents")

    # .groupby(["designatedArea","state"])

    .assign(
            designatedArea_state=lambda x:
                x["designatedArea"].astype(str) + ", " + x["state"].astype(str)
        )
    .groupby(["designatedArea_state"])
    
    .size()
    .reset_index(name="count")
    .sort_values("count", ascending=False)
    .reset_index(drop=True)
)

#       designatedArea_state	count
# 0	    Owsley (County), KY	    41
# 1	    Magoffin (County), KY	39
# 2	    Perry (County), KY	    39
# 3	    Breathitt (County), KY	39
# 4	    Lawrence (County), KY	39
# ...	...	...
# 3490	Nondalton (ANV/ANVSA), AK	            1
# 3491	Nome (Census Area), AK	                1
# 3492	Niihau (CCD), HI	                    1
# 3493	Grand Portage Indian Reservation, MN    1
# 3494	Lake and Peninsula (Borough), AK	    1

(
    df
    .query("declarationType == 'DR'"
           "and incidentType not in @excluded_incidents")["designatedArea"]
    .str.lower()
    .str.split()
    .explode()
    .value_counts()
    .head(50)
)

# designatedArea
# (county)       38061 <-----
# (parish)        1425 <-----
# (municipio)     1214 <-----
# st.              532
# jefferson        426
# washington       405
# franklin         353
# jackson          351
# lincoln          321
# reservation      299
# san              298
# (county)(in      298
# clay             283
# madison          280
# indian           276
# montgomery       247
# marion           239
# monroe           236
# union            215
# wayne            211

included_areas = "county|parish|municipio"

(
    df
    .query(
        "declarationType == 'DR' "
        "and incidentType not in @excluded_incidents "
        "and designatedArea.str.contains(@included_areas, case=False, na=False)",
        )
    [["designatedArea", "state"]]
    .groupby(["designatedArea", "state"])
    .size()
    .reset_index(name="count")
    .sort_values("count", ascending=False)
    .reset_index(drop=True)
)

# 	    designatedArea	    state	count
# 0	    Owsley (County)	    KY	    41
# 1	    Breathitt (County)	KY	    39
# 2	    Perry (County)	    KY	    39
# 3	    Lawrence (County)	KY	    39
# 4	    Magoffin (County)	KY	    39
# ...	...	...	...
# 3166	Carbon (County)	    WY	    1
# 3167	Summit (County)	    CO	    1
# 3168	Ebon (County-equivalent) MH	1
# 3169	Caribou (County)	ID	    1
# 3170	Lincoln (County)	WY	    1

(
    df
    .query(
        "declarationType == 'DR' "
        "and incidentType not in @excluded_incidents "
        "and designatedArea.str.contains(@included_areas, case=False, na=False)",
        )
    .groupby(["fyDeclared"])
    .size()
)
# fyDeclared
# 1959       1
# 1965     539
# 1966     121
# 1967     193
# 1968     149
#         ... 
# 2022     702
# 2023     872
# 2024    1195
# 2025     827
# 2026     631

(
    df
    .query(
        "declarationType == 'DR' "
        "and incidentType not in @excluded_incidents "
        "and designatedArea.str.contains(@included_areas, case=False, na=False)",
        )
    ["fyDeclared"].value_counts().sort_index()
)

df_clean = (
    df
    .query(
        "declarationType == 'DR' "
        "and incidentType not in @excluded_incidents "
        "and designatedArea.str.contains(@included_areas, case=False, na=False)"
        "and fyDeclared != 2026 ",
        engine="python"
    )
    .assign(
        year_month=lambda x: pd.to_datetime(x["declarationDate"]).dt.strftime("%Y-%m"),
        year=lambda x: pd.to_datetime(x["declarationDate"]).dt.year,
        month=lambda x: pd.to_datetime(x["declarationDate"]).dt.month
    )
)

df_clean["fyDeclared"].value_counts().sort_index()

df_clean["year"].value_counts().sort_index()

df_clean["year"].value_counts().reset_index(name="count").sort_index(ascending=True)

df_clean["month"].value_counts().reset_index(name="count").sort_index()

######################

annual = (
    df_clean["year"]
    .value_counts()
    .sort_index()
    .rename_axis("year")
    .reset_index(name="disasterDeclarations")
)

annual.plot(
    x="year",
    y="disasterDeclarations",
    # kind="scatter",
    kind="line"
    )

annual.plot(
    x="year",
    y="disasterDeclarations",
    # kind="scatter",
    kind="line",
    marker="o",
    # figsize=(10, 5),
    label="Observed",
    xlabel="Year",
    ylabel="Number of disasters"
).legend(loc="upper left")

# OR

import matplotlib.pyplot as plt

plt.cla()

plt.scatter(
    annual["year"],
    annual["disasterDeclarations"],
    label="Observed"
)

plt.xlabel("Year")
plt.ylabel("Number of disasters")
plt.legend(loc="upper left")
plt.show()

######################

x = annual["year"]
y = annual["disasterDeclarations"]

m = ((x - x.mean()) * (y - y.mean())).sum() / ((x - x.mean())**2).sum()
b = y.mean() - m * x.mean()

plt.cla()

plt.scatter(x, y, label="Observed")
plt.plot(x, m*x + b, label="Trend", color="red")

plt.xlabel("Year")
plt.ylabel("Number of disaster declarations")
plt.legend(loc="upper left")
plt.show()

######################

import statsmodels.api as sm

x2 = sm.add_constant(x)
model = sm.OLS(y, x2).fit()
print(model.summary())

# sklearn      → prediction/modeling
# statsmodels  → statistical inference and detailed summaries

######################

from sklearn.linear_model import LinearRegression

X = annual[["year"]]

model = LinearRegression()
model.fit(X, y)

print("Intercept:", model.intercept_)
print("Slope:", model.coef_[0])
# Slope: 15.656102336136374

print("R²:", model.score(X, y))
# R²: 0.4397437924228781

######################

annual["predicted"] = model.predict(X)

plt.cla()

plt.scatter(
    annual["year"],
    annual["disasterDeclarations"],
    label="Observed"
)

plt.plot(
    annual["year"],
    annual["predicted"],
    label="Linear trend",
    color="red"
)

plt.xlabel("Year")
plt.ylabel("Number of disaster declarations")
plt.legend(loc="upper left")
plt.show()

######################

annual = (
    df_clean
    .groupby("year")["disasterNumber"]
    .nunique()
    .reset_index(name="disasterCount")
    .sort_values("year")
)

plt.cla()

plt.scatter(
    annual["year"],
    annual["disasterCount"],
    label="Observed"
)

plt.xlabel("Year")
plt.ylabel("Number of disasters")
plt.legend(loc="upper left")
plt.show()

######################

X = annual[["year"]]
y = annual["disasterCount"]

model = LinearRegression()
model.fit(X, y)

print("Slope:", model.coef_[0])
# Slope: 0.7414749983446598

print("R²:", model.score(X, y))
# R²: 0.484755140752112

######################

annual["predicted"] = model.predict(X)

plt.plot(
    annual["year"],
    annual["predicted"],
    label="Linear trend",
    color="red"
)

plt.ylabel("Number of disaster events")
plt.legend(loc="upper left")
plt.show()

######################

month = (
    df_clean
    .groupby("year_month")["disasterNumber"]
    .nunique()
    .reset_index(name="disasterCount")
    .sort_values("year_month")
)

plt.cla()

plt.scatter(
    month["year_month"],
    month["disasterCount"],
    label="Observed"
)

plt.xlabel("Year-Month")
plt.ylabel("Number of disasters")
plt.legend(loc="upper left")
plt.show()

######################

month["date"] = pd.to_datetime(month["year_month"], format="%Y-%m")
# numeric representation of time for regression
x = month["date"].map(pd.Timestamp.toordinal)
y = month["disasterCount"]

# Fit linear regression
coef = np.polyfit(x, y, 1)
y_fit = np.polyval(coef, x)

plt.cla()

plt.scatter(
    month["date"],
    y,
    label="Observed"
)

plt.plot(
    month["date"],
    y_fit,
    label="Linear trend",
    color="red"
)

plt.xlabel("Year-Month")
plt.ylabel("Number of disasters")
plt.legend(loc="upper left")

plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

x = x.to_numpy().reshape(-1, 1)
model = LinearRegression()
model.fit(x, y)

print("Slope:", model.coef_[0])
# Slope: 0.0001380490783180925

print("R²:", model.score(x, y))
# R²: 0.08601714474313993

######################

"""
Should the trend be modeled using monthly data or annual data?
Which time scale provides the more appropriate estimate of the long-term increase?

A more statistical version would be:
"Is it more appropriate to estimate the temporal trend using monthly observations or annual aggregates?"

To focus on model validity rather than preference:
Does a regression on monthly observations provide a more appropriate estimate of the trend than a regression on yearly aggregated observations?

The important methodological issue is that monthly and yearly regressions answer slightly different questions:
- Monthly data preserve more information and can reveal seasonality, but consecutive observations may be autocorrelated, which violates the independence assumption of ordinary linear regression.
- Annual aggregation reduces seasonality and short-term variability, but you lose observations and statistical power.

For disaster declarations specifically, I would frame the analysis around these questions:
- Long-term trend: Is the average number of disaster declarations increasing over time?
- Seasonality: Are some months systematically associated with more declarations?
- Dependence: Are declaration counts in one month related to nearby months?

The estimated monthly trend can be converted it into an approximate annual trend by multiplying by 12.
PS. pd.Timestamp.toordinal converts each date to an integer number of days.

0.00013805 x 365.25 = 0.0504

The correct interpretation is: one year later, the model predicts about 0.0504 more disasters in a given month.

12 x 0.0504 = 0.605

Note: Slope 0.605 disasters/year (month analysis) < 0.7415 disasters/year (year analysis)
      and, R²: 0.08601714474313993 << R²: 0.484755140752112

The key question is usually not simply "monthly or yearly?"
What temporal resolution preserves the information needed to estimate the long-term trend while properly accounting for seasonality and autocorrelation?
"""

######################
# Autocorrelation
######################

x1 = annual["year"]
x1 = sm.add_constant(x1)
y1 = annual["disasterCount"]

model1 = sm.OLS(y1, x1).fit()
print(model1.summary())

x2 = month["date"]
x2 = month["date"].map(pd.Timestamp.toordinal)
x2 = sm.add_constant(x2)
y2 = month["disasterCount"]

model2 = sm.OLS(y2, x2).fit()
print(model2.summary())

from statsmodels.stats.stattools import durbin_watson

residuals1 = y1 - model1.predict(x1)
dw1 = durbin_watson(residuals1)
print(dw1)
# 1.3340019374852194

residuals2 = y2 - model2.predict(x2)
dw2 = durbin_watson(residuals2)
print(dw2)
# 1.58420999874454

# Interpretation is roughly:
# DW ≈ 2   -> little/no first-order autocorrelation
# DW < 2   -> positive autocorrelation
# DW > 2   -> negative autocorrelation

# DW ≈ 0   strong positive autocorrelation
# DW ≈ 1   positive autocorrelation
# DW ≈ 2   little/no first-order autocorrelation
# DW ≈ 3   negative autocorrelation
# DW ≈ 4   strong negative autocorrelation