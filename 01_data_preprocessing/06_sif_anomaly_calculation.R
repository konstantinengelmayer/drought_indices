# R Script for Processing and Analyzing Solar-Induced Fluorescence (SIF) Data

# Description:
# This script processes the Solar-Induced Fluorescence (SIF) data from the TROPOMI satellite 
# for the period 2018–2024, calculates monthly anomalies, and saves the results as a raster file.
# Specifically, it:
# - Loads a NetCDF file containing combined SIF data.
# - Selects the relevant SIF layers based on their names.
# - Reprojects the data to a specified coordinate reference system (EPSG:31467).
# - Computes the monthly average for each calendar month (May to December).
# - Creates and calculates the anomaly stack by subtracting the monthly means from each layer.
#
# Workflow:
# 1. Load the TROPOMI SIF data.
# 2. Extract the relevant SIF layers based on their names.
# 3. Reproject the SIF data to the required coordinate reference system.
# 4. Extract dates from the layer names (assuming the format "YYYY-MM-DD").
# 5. Calculate the monthly mean for each calendar month (1 to 12).
# 6. Compute the SIF anomalies by subtracting the monthly mean from each layer.
# 7. Save the SIF anomalies as a raster file.
#
# Data Requirements:
# - A NetCDF file containing combined SIF data with layers named using the format "YYYY-MM-DD" for each date.
# - The script assumes the coordinate system is reprojected to EPSG:31467.
#
# Outputs:
# - A raster file representing the anomalies of SIF for the period 2018–2024.

library(terra)

# 1. Load the TROPOMI SIF data from a NetCDF file
troposif <- rast("data/sif/troposif/combined_data.nc")

# 2. Select the relevant SIF layers based on a pattern in the layer names
# The grep function identifies layers with names matching the pattern of "solar_induced_fluorescence_<number>"
sif_layer_index <- grep("^solar_induced_fluorescence_\\d{1,2}$", names(troposif))

# Select only the layers that match the specified pattern
troposif <- troposif[[sif_layer_index]] 

# 3. Reproject the SIF raster to EPSG:31467, if necessary
troposif <- project(troposif, "EPSG:31467")

# 4. Generate a sequence of dates starting from May 2018, with monthly intervals
# This will be used to match the layers to their respective dates
sif_dates <- seq(as.Date("2018-05-01"), by = "month", length.out = nlyr(troposif))

# 5. Extract the dates from the layer names, assuming format "YYYY-MM-DD"
# Convert the layer names (dates) into a Date format
layer_dates <- as.Date(names(troposif), format = "%Y-%m-%d")

# 6. Extract the numeric month (1 to 12) from the extracted dates
layer_months <- as.numeric(format(layer_dates, "%m"))

# 7. Calculate the multi-year mean for each calendar month (1 to 12)
# The 'tapp' function groups the layers by month and calculates the mean for each group
sif_monthly_means <- tapp(troposif, layer_months, mean, na.rm = TRUE)
# This results in a SpatRaster with 12 layers, each representing the mean for a specific month

# 8. Compute the SIF anomalies:
# The anomaly for each layer is calculated by subtracting the monthly mean for that month
# from the actual value for that month
sif_anomalies <- troposif - sif_monthly_means[[paste0("X", as.character(layer_months))]]

# 9. Save the SIF anomalies as a raster file for future analysis or visualization
writeRaster(sif_anomalies, "data/sif/troposif/sif_anomaly_2018_2024.tif")

