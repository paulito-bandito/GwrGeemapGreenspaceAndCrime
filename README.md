
# GWR, Geemap, Greenspace, and Crime
This project is a simple one that is going to use either a Spatial Weighted Regression (SAR) or Geospatially Weighted Regression (GWR) to evaluate if it's true that the more greenspace, controlling for issues like poverty and social deprivation, the less crime there is. 

# Geemap

Amazing tool to use Google Earth Engine from your Desktop computer with Jupyter Notebook.

See https://geemap.org/installation/

## Setup

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


## Run the Notebook

Launch Jupyter notebook
```
    conda activate gee
    jupyter notebook

```
