
# GEEMAP 

See https://geemap.org/installation/

## Setup

install geemap using the following command:


```    
    conda install geemap -c conda-forge
```

The geemap package has an optional dependency - geopandas, which can be challenging to install on some computers, especially Windows. It is highly recommended that you create a fresh conda environment to install geopandas and geemap. Follow the commands below to set up a conda env and install geopandas, xarray_leaflet, and geemap.

```
    conda create -n gee python=3.9
    conda activate gee
    conda install geopandas
    conda install mamba -c conda-forge
    mamba install geemap localtileserver -c conda-forge
    conda install jupyter_contrib_nbextensions -c conda-forge

    # then update
    mamba update -c conda-forge geemap
    mamba update -c conda-forge geopandas
    mamba update -c conda-forge fiona  # problematic, you'll get a warning
```


## Interacting with GEEMAP

Launch Jupyter notebook
```
    conda activate gee
    jupyter notebook
```

Import libraries

```
    import ee
    import geemap
    Create an interactive map¶

    Map = geemap.Map(center=(40, -100), zoom=4)
    Map
```
Add Earth Engine data¶

```
    # Add Earth Engine dataset
    dem = ee.Image('USGS/SRTMGL1_003')
    landcover = ee.Image("ESA/GLOBCOVER_L4_200901_200912_V2_3").select('landcover')
    landsat7 = ee.Image('LE7_TOA_5YEAR/1999_2003')
states = ee.FeatureCollection("TIGER/2018/States")
```
Set visualization parameters¶

dem_vis = {
'min': 0,
'max': 4000,
'palette': ['006633', 'E5FFCC', '662A00', 'D8D8D8', 'F5F5F5']}

landsat_vis = {
    'min': 20,
    'max': 200,
    'bands': ['B4', 'B3', 'B2']
}
Display data on the map¶

Map.addLayer(dem, dem_vis, 'SRTM DEM', True, 0.5)
Map.addLayer(landcover, {}, 'Land cover')
Map.addLayer(landsat7, landsat_vis, 'Landsat 7')
Map.addLayer(states, {}, "US States")
Interact with the map¶"# GwrGeemapGreenspaceAndCrime" 
