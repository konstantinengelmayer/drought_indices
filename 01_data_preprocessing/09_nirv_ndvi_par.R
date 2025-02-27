# R Script for Calculating NDVI and NIRv Anomalies and Their Interactions with PAR

# Description:
# This script processes NDVI and NIRv data to calculate anomalies based on monthly means and 
# their interaction with photosynthetically active radiation (PAR).
# It follows these main steps:
# 1. Load and extract NDVI and NIRv data from a NetCDF file.
# 2. Subset the data for the desired date range (2018-05 to 2024-09).
# 3. Compute NDVI anomalies
# 5. Interact NIRv with PAR data by element-wise multiplication.
# 6. Compute anomalies for the NIRv-PAR interaction and save the results.

# Data Requirements:
# - NetCDF file containing aggregated NDVI and NIRv data (monthly layers).
# - Monthly PAR raster stack for the period 2018-2024.
#
# Outputs:
# - NDVI anomaly raster for the period 2018-05 to 2024-09.
# - NIRv-PAR interaction anomaly raster for the same period.

# Load the necessary library
library(terra)

# Set Your Project Route Directory here
setwd("C:/Users/konst/Documents/")

# Load the NetCDF file containing NDVI and NIRv data
nc <- rast("data/vegetation_indices/aggregated_monthly_ndvi_nirv_optimized.nc")

# Extract the first 299 layers for NDVI (months 1-299)
ndvi <- nc[[1:299]]

# Rename the layers with corresponding dates (starting from 2000-02-01 to 2024-12-01)
names(ndvi) <- seq(from = as.Date("2000-02-01"), to = as.Date("2024-12-01"), by = "month")

# Subset NDVI to match the desired date range (2018-05-01 to 2024-09-01)
ndvi <- ndvi[[as.character(seq(from = as.Date("2018-05-01"), to = as.Date("2024-09-01"), by = "month"))]]

# Extract dates from layer names (assuming format "YYYY-MM-DD")
layer_dates <- as.Date(names(ndvi), format = "%Y-%m-%d")

# Extract the numeric month from these dates
layer_months <- as.numeric(format(layer_dates, "%m"))

# Compute the multi-year mean for each calendar month using 'tapp' function
# This will group layers by month and compute the mean for each month
ndvi_monthly_means <- tapp(ndvi, layer_months, mean, na.rm = TRUE)

# Create the NDVI anomaly stack by subtracting the monthly mean from each layer
ndvi_anomalies <- ndvi - ndvi_monthly_means[[paste0("X", as.character(layer_months))]]

# Save the NDVI anomalies to a GeoTIFF file
writeRaster(ndvi_anomalies, "data/vegetation_indices/ndvi_anom_2018-05_2024-09.tif")

# Load the PAR raster stack and set the names to dates (2018-01-01 to 2024-06-01)
par <- rast("data/par/MODIS_PAR_Monthly_Aggregated_2018_2024.tif")
names(par) <- seq(from = as.Date("2018-01-01"), to = as.Date("2024-06-01"), by = "month")

# Crop PAR and NIRv to the same extent
par <- crop(par, nirv[[1]], mask = TRUE)
nirv <- crop(nirv, par[[1]], mask = TRUE)

# Perform element-wise multiplication between NIRv and PAR for each month
nirv_par_product <- nirv[[intersect(names(nirv), names(par))]] * par[[intersect(names(nirv), names(par))]]

# Replace infinite values with zero to handle possible invalid results
nirv_par_product <- ifel(is.infinite(nirv_par_product), 0, nirv_par_product)

# Compute the multi-year mean for NIRv-PAR interaction anomalies
layer_dates <- as.Date(names(nirv_par_product), format = "%Y-%m-%d")
layer_months <- as.numeric(format(layer_dates, "%m"))

# Compute monthly means for the NIRv-PAR interaction anomaly stack
nirv_monthly_means <- tapp(nirv_par_product, layer_months, mean, na.rm = TRUE)

# Create the NIRv-PAR interaction anomaly stack by subtracting the monthly mean from each layer
nirv_anomalies <- nirv_par_product - nirv_monthly_means[[paste0("X", as.character(layer_months))]]

# Plot the NIRv anomaly raster for visualization
plot(nirv_anomalies)

# Save the NIRv anomalies to a GeoTIFF file
writeRaster(nirv_anomalies, "data/vegetation_indices/nirv_par_anom_2018-05_2024-09.tif")
