# Libraries
import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
import rasterio.plot
import matplotlib.pyplot as plt

# Data
world = gpd.read_file('/Users/alexander/GitHub/abuabara.github.io/DAEN489S26/pydata/world.gpkg')
elev = rasterio.open('/Users/alexander/GitHub/abuabara.github.io/DAEN489S26/pydata/elev.tif')
grain = rasterio.open('/Users/alexander/GitHub/abuabara.github.io/DAEN489S26/pydata/grain.tif')
multi_rast = rasterio.open('/Users/alexander/GitHub/abuabara.github.io/DAEN489S26/pydata/landsat.tif')

# Vectors
world.head()

idx_small = world['area_km2'] < 10000
small_countries = world[idx_small]
small_countries

asia_small = world[
    (world['continent'] == 'Asia') &
    (world['area_km2'] < 10000)
]
asia_small[['name_long', 'continent', 'area_km2']]

asia_subset = world[world['continent'] == 'Asia'] \
    .loc[:, ['name_long', 'continent']] \
    .iloc[0:5, :]
asia_subset

world_pop = world.groupby('continent')[['pop']].sum().reset_index()
world_pop

world_agg = world[['continent','pop','geometry']] \
    .dissolve(by='continent', aggfunc='sum') \
    .reset_index()
world_agg.plot(column='pop', legend=True)

coffee_data = pd.read_csv('/Users/abuabara/Downloads/DAEN489_data/pydata/coffee_data.csv')
coffee_data.head()

world_coffee = pd.merge(
    world, coffee_data,
    on='name_long', how='left'
)
world_coffee[['name_long', 'coffee_production_2016']]

coffee_inner = pd.merge(
    world, coffee_data,
    on='name_long', how='inner'
)
coffee_inner

world2 = world.copy()
world2['pop_density'] = world2['pop'] / world2['area_km2']
world2[['name_long','pop_density']]

world2 = world2.drop('geometry', axis=1)
world2 = pd.DataFrame(world2)
world2.head()

# Rasters

plt.close('all')
rasterio.plot.show(elev)

elev = src_elev.read(1)
elev

np.mean(elev)

elev_float = elev.astype('float64')
elev_float[0,2] = np.nan
np.nanmean(elev_float)

plt.close('all')
rasterio.plot.show(grain)

grain = src_grain.read(1)
freq = np.unique(grain, return_counts=True)

plt.close('all')
plt.bar(*freq)
plt.show()

""" 
src = rasterio.open('/Users/abuabara/Downloads/DAEN489_data/pydata/srtm.tif')
src

src.read(1)

elev = np.arange(1, 37, dtype=np.uint8).reshape(6, 6)
elev

v = [
  1, 0, 1, 2, 2, 2, 
  0, 2, 0, 0, 2, 1, 
  0, 2, 2, 0, 0, 2, 
  0, 0, 1, 1, 1, 1, 
  1, 1, 1, 2, 1, 1, 
  2, 1, 2, 2, 0, 2
]

grain = np.array(v, dtype=np.uint8).reshape(6, 6)
grain

new_transform = rasterio.transform.from_origin(
    west=-1.5, 
    north=1.5, 
    xsize=0.5, 
    ysize=0.5
)
new_transform

rasterio.plot.show(elev, transform=new_transform)
rasterio.plot.show(grain, transform=new_transform)

new_dataset = rasterio.open(
    '/Users/abuabara/Downloads/DAEN489_data/pydata/elev.tif', 'w', 
    driver='GTiff',
    height=elev.shape[0],
    width=elev.shape[1],
    count=1,
    dtype=elev.dtype,
    crs=4326,
    transform=new_transform
)
new_dataset.write(elev, 1)
new_dataset.close()

new_dataset = rasterio.open(
    '/Users/abuabara/Downloads/DAEN489_data/pydata/grain.tif', 'w', 
    driver='GTiff',
    height=grain.shape[0],
    width=grain.shape[1],
    count=1,
    dtype=grain.dtype,
    crs=4326,
    transform=new_transform
)
new_dataset.write(grain, 1)
new_dataset.close()
 """