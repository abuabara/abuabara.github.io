import geopandas as gpd
import numpy as np
from pygris import states, counties, tracts
import matplotlib.pyplot as plt

# U.S. states (non-generalized boundaries)
us = states(cb=False, year=2022)

# Texas counties (non-generalized boundaries)
texas = counties(state="TX", cb=False, year=2022)

# Brazos County census tracts
brazos = tracts(state="TX", county="Brazos", cb=False, year=2022)

# Get the CRS (coordinate reference system)
brazos.crs

# Ensure we are working on a copy
brazos = brazos.copy()

# Native area (current CRS)
brazos.geometry.area

# Project to equal-area CRS first
brazos_3083 = brazos.to_crs(epsg=3083)

# Then compute area
brazos["area_3083"] = brazos_3083.geometry.area

brazos["diff"] = brazos["ALAND"] + brazos["AWATER"] - brazos["area_3083"]
brazos["diff"].sum()
# ~ -71.9094 (depending on floating-point and pyproj version)

# EPSG:4203 (NAD83 geographic)
brazos_4203 = brazos.to_crs(epsg=4203)
brazos["area_4203"] = brazos_4203.geometry.area

brazos["diff"] = brazos["ALAND"] + brazos["AWATER"] - brazos["area_4203"]
brazos["diff"].sum()
# ~ -1531301187.8559

# EPSG:32139 — NAD83 / Texas Central (Lambert Conformal Conic)
# NAD83 / Texas Central (FIPS 4203) --> EPSG: 32139
brazos_32139 = brazos.to_crs(epsg=32139)
brazos["area_32139"] = brazos_32139.geometry.area

brazos["diff"] = brazos["ALAND"] + brazos["AWATER"] - brazos["area_32139"]
brazos["diff"].sum()
# ~ 298551.3601

bias_pct = (
    brazos["diff"].sum() /
    (brazos["ALAND"] + brazos["AWATER"]).sum()
) * 100

bias_pct

# ~ 0.0195

# EPSG:32139 preserves shape/angles, not area
# Systematic area bias of ~0.02% across Brazos County
# Not acceptable for scientific/statistical area work

# EPSG:3083 — Texas Centric Albers (equal-area)
brazos_3083 = brazos.to_crs(epsg=3083)
brazos["area_3083"] = brazos_3083.geometry.area

brazos["diff"] = brazos["ALAND"] + brazos["AWATER"] - brazos["area_3083"]
brazos["diff"].sum()
# ~ -71.91

bias_pct = (
    brazos["diff"].sum() /
    (brazos["ALAND"] + brazos["AWATER"]).sum()
) * 100

bias_pct

# -4.6960e-06

brazos_nogeo = brazos.drop(columns="geometry")

fig, axes = plt.subplots(1, 3, figsize=(18, 6), constrained_layout=True)

# Original (EPSG:4269 likely)
brazos.plot(ax=axes[0], edgecolor="red", facecolor="lightgray", linewidth=1.5)
axes[0].set_title("Original (4269)")
# axes[0].set_axis_off()

# 32139 for Shape
brazos_32139.plot(ax=axes[1], edgecolor="red", facecolor="lightgray", linewidth=1.5)
axes[1].set_title("32139 for Shape")
# axes[1].set_axis_off()

# 3083 for Area
brazos_3083.plot(ax=axes[2], edgecolor="red", facecolor="lightgray", linewidth=1.5)
axes[2].set_title("3083 for Area")
# axes[2].set_axis_off()

plt.show()

def area_in_crs(gdf, epsg):
    return gdf.to_crs(epsg=epsg).geometry.area

brazos["area_3083"] = area_in_crs(brazos, 3083)
brazos["area_32139"] = area_in_crs(brazos, 32139)

# Conceptual mapping logic
# - Geographic CRS → never acceptable for area
# - Conformal CRS (32139) → shape OK, area biased
# - Equal-area CRS (3083) → correct for area
