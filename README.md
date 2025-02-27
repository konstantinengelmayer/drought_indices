# Course Contribution Reproduction Manual

This repository contains the source code for "Assessing the Response of Satellite-Derived Solar-Induced Chlorophyll Fluorescence to Meteorological Drought.". This study is a course project for the Climate Impact Research course 2025 by Boris Thies and was also done for learning purposes of the author. As a result, the scripts are written across various platforms, including Google Earth Engine (JavaScript), R, Python, and Jupyter Notebooks.

## 1. Data Preprocessing
Scripts in the `data_preprocessing` folder prepare all input data:

- **Climate Data**:  
  `01_dwd_grid_download.R` downloads the necessary climate data.

- **Drought Indices & AI**:  
  Scripts starting with `02_` calculate drought indices and Agricultural Indices (AI).

- **Digital Elevation Model (DEM)**:  
  Download DEM from [BKG](https://daten.gdz.bkg.bund.de/produkte/dgm/dgm200/aktuell/dgm200.utm32s.gridascii.zip).

- **Topographic Variables**:  
  `03_topographics.R` computes topographic variables using the DEM.

- **Land Cover & Tree Cover Density**:  
  Download tree cover density from [here](https://land.copernicus.eu/en/products/high-resolution-layer-tree-cover-density/tree-cover-density-2018#download) and land cover data from [here](https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download).  
  `04_landcover_tree_cover_density.R` reprojects, crops, and resamples these datasets.

- **Solar-Induced Fluorescence (SIF)**:  
  `05_download_troposif.py` downloads SIF data, and `06_sif_anomaly_calculation.R` computes its anomalies.

- **MODIS Reflectance & NDVI/NIRv**:  
  `07_modis_reflectance_ndvi_nirv.ipynb` downloads MODIS reflectance data and calculates monthly NDVI and NIRv via Microsoft's Planetary Computer. 

- **PAR Data**:  
  `08_par_download.js` (run in Google Earth Engine) downloads monthly aggregated PAR data.

- **NDVI, NIRv, and PAR Anomalies**:  
  `09_nirv_ndvi_par.R` calculates anomalies for NDVI, NIRv, and NIRvP.

## 2. Random Forest Model
Scripts in the `random_forest_model` folder build and evaluate the model:

- **Data Preparation**:  
  `01_model_data_preparation.R` prepares predictor data.

- **Model Training**:  
  `02_model_training.R` trains the Random Forest model and visualizes the results.

## 3. Final Correlation Analysis
- `correlation_each_pixel_each_aoi.R` (in the `final_correlation_analysis` folder) calculates pixel-wise correlations between SIF (or NDVI/NIRvP) anomalies and drought indices. Adjust plot titles and file paths as needed.

## Additional Analysis
- **Compare Study Areas**:  
  `compare_study_areas.R` (JavaScript) compares climate variables across study areas.

- **Compare SIF, NIRvP, and SPI06**:  
  `sif_nirvp_spi06.R` creates plots comparing SIF and NIRvP anomaly responses to SPI06.

