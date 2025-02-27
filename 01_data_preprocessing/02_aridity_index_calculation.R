# R Script for Calculating Long-Term Aridity Index (1991-2024)

# Description:
# This script calculates the long-term Aridity Index (AI) for the period 1991–2024.
# It uses:
# - Monthly Precipitation (P) raster data.
# - Monthly Potential Evapotranspiration (PET) raster data.
#
# Workflow:
# 1. Load raster datasets for precipitation and potential evapotranspiration.
# 2. Compute long-term annual averages for each raster dataset (P and PET).
# 3. Calculate the AI as the ratio of average precipitation to average PET 
#    (AI = P / PET) on a per-pixel basis.
# 4. Handle division by zero and missing data.
# 5. Save the AI raster to a file for further analysis or visualization.
#
# Data Requirements:
# - Precipitation raster stack: Contains monthly precipitation data from 1991–2024.
# - Potential Evapotranspiration raster stack: Contains monthly PET data for the same period.
# - Both raster stacks must have consistent spatial resolution, extent, and layer naming conventions.

# Outputs:
# - A single raster representing the long-term Aridity Index for the study period.

# Load necessary libraries

library(terra)
library(stringr)
# Set Your Project Route Directory here
setwd("C:/Users/konst/Documents")

# Load all merged PET (evapotranspiration) raster files
evapo_p_files <- list.files("data/climate/evapo_p/", pattern = ".*merged\\.tif$", full.names = TRUE)
evapo_p <- rast(evapo_p_files)

# Load all merged precipitation raster files
precip_files <- list.files("data/climate/precipitation/", pattern = ".*merged\\.tif$", full.names = TRUE)
precip <- rast(precip_files)

# Calculate long-term annual averages for precipitation and PET
mean_precip <- app(precip, fun = mean, na.rm = TRUE)  # Long-term average precipitation
mean_evapo_p <- app(evapo_p, fun = mean, na.rm = TRUE)  # Long-term average PET

# Calculate the Aridity Index (AI = P / PET) based on long-term averages
long_term_ai <- mean_precip / mean_evapo_p

# Handle division by zero or missing data
long_term_ai[is.infinite(long_term_ai)] <- NA

# Plot the long-term AI raster
plot(long_term_ai)

# Extract years from the filename
years <- str_extract(evapo_p_files[1], "\\d{4}_\\d{4}")  # Extract "1991_2024" from filename

# Define output filename dynamically
output_filename <- paste0("data/climate/drought_indices/long_term_aridity_index_", years, ".tif")

# Save the computed long-term aridity index raster stack
writeRaster(long_term_ai, filename = output_filename, overwrite = TRUE)
