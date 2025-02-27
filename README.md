# Course Contribution Repruction Manual
A repository containing the source code of "Assessing the Response of Satellite-Derived Solar-Induced Chlorophyll Fluorescence to Meteorological Drought"

### 1. Data Preprocessing
  1. in the folder data_preprocessing all scripts for data preparation are found.
  2. 01_dwd_grid_download.R downloads all nessesary climate data.
  3. With the scripts starting with 02 all drought indices and AI can be calculated.
  4. The DEM has to be downloaded from here: https://daten.gdz.bkg.bund.de/produkte/dgm/dgm200/aktuell/dgm200.utm32s.gridascii.zip.
  5. The topographic variables can then be calculated according to 03_topographics.R.
  6. The tree cover density can be downloaded from here: https://land.copernicus.eu/en/products/high-resolution-layer-tree-cover-density/tree-cover-density-2018#download
     The landcover can be downloaded from here: https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download
     Both layer are being reprojected, cropped and resampled with script 04_landcover_tree_cover_density.R
  7. The python script 05_download_troposif.py is used to download the SIF data and script 06_sif_anomaly_calculation.R calulates its anomalies.
  8. Jupyter Notebook 07_modis_reflectance_ndvi_nirv.ipynb is used to download modis reflactance data and calculate NDVI and NIRv for each month using Microsofts planetary computer.
  9. 
