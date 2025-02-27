# R Script for Calculating PDSI from Climate and Soil Data
#
# Description:
# This script calculates the Palmer Drought Severity Index (PDSI) using precipitation, 
# potential evapotranspiration (PET), and soil available water capacity (AWC) data. 
# It processes raster datasets to compute PDSI values for each pixel over a specified time period.
#
# Workflow Overview:
# 1. **Library Loading**:
#    - Imports required libraries (`terra`, `parallel`, `stringr`, `devtools`, `scPDSI`) 
#      for data manipulation and PDSI computation.
#
# 2. **Setup**:
#    - Sets the working directory, creates necessary directories, and initializes parallel processing.
#
# 3. **Data Preparation**:
#    - Loads precipitation and PET raster data.
#    - Downloads and processes soil AWC data.
#
# 4. **Function Definition**:
#    - Defines a custom function to compute PDSI per pixel, handling missing data.
#
# 5. **PDSI Calculation**:
#    - Combines AWC, precipitation, and PET data into a single raster stack.
#    - Applies the PDSI computation function across all pixels using parallel processing.
#
# 6. **Output**:
#    - Saves the computed PDSI rasters as GeoTIFF files for further analysis.
#
# Notes:
# - Ensure all required input data (precipitation, PET, AWC) is available in the specified directories.
# - Adjust the number of cores (`cores`) as per system capability to optimize performance.

# Load required libraries
library(terra)
library(parallel)
library(stringr)
library(devtools)

# Check and install GitHub package if not already installed
if (!"scPDSI" %in% installed.packages()) {
  devtools::install_github("Sibada/scPDSI")
  library(scPDSI)
}

#############################Adjust here#############################
setwd("C:/Users/konst/Documents/")
cores <- 15
#####################################################################

# -------------------------------
# Create output directories if they do not exist
# -------------------------------
# Check if the PDSI output folder exists; if not, create it (including any parent directories)
if (!dir.exists("data/climate/drought_indices/PDSI")) {
  dir.create("data/climate/drought_indices/PDSI", recursive = TRUE)
}

# Check if the soil data folder exists; if not, create it
if (!dir.exists("data/soil/")) {
  dir.create("data/soil", recursive = TRUE)
}

# -------------------------------
# Load input data: Precipitation and PET
# -------------------------------
# Load precipitation data by listing all .tif files in the precipitation folder and creating a raster stack.
precip_rasters <- rast(list.files("data/climate/precipitation", pattern = "\\.tif$", full.names = TRUE))

# Extract years from the filenames using a regular expression (matches any four-digit number)
years <- str_extract_all(sources(precip_rasters), "\\d{4}")[[1]]
# Get the names of each raster layer (assumed to be date strings)
dates <- names(precip_rasters)

# Load PET (Potential Evapotranspiration) data as a raster stack from .tif files.
evapo_rasters <- rast(list.files("data/climate/evapo_p", pattern = "\\.tif$", full.names = TRUE))

# -------------------------------
# Download and process AWC (Available Water Capacity) data
# -------------------------------
# Check if the AWC zip file is already present. If not, download and unzip it.
if (!file.exists("data/soil/NFKWe1000_250.zip")) {
  download.file("https://download.bgr.de/bgr/Boden/NFKWE1000/geotiff/NFKWe1000_250.zip", "data/soil/NFKWe1000_250.zip")
  unzip("data/soil/NFKWe1000_250.zip", exdir = "data/soil/", unzip = "unzip")
} else {
  message("File already exists, skipping download and extraction.")
}

# Load the AWC data from the extracted .tif file as a raster layer.
awc <- rast("data/soil/NFKWe1000_250.tif")
# Replace any missing values (NA) in the AWC raster with a default value of 100.
awc <- ifel(is.na(awc), 100, awc)
# Reproject AWC to match the coordinate system of the first precipitation raster.
awc <- project(awc, precip_rasters[[1]])
# Resample AWC to match the resolution of the precipitation data using bilinear interpolation.
awc <- resample(awc, precip_rasters[[1]], method = "bilinear")
# Crop AWC to the spatial extent of the precipitation raster and apply a mask.
awc <- crop(awc, precip_rasters[[1]], mask = TRUE)
# Plot the processed AWC raster for a visual check.
plot(awc)

# -------------------------------
# Stack AWC, Precipitation, and PET rasters together
# -------------------------------
# Combine the AWC raster with precipitation and PET rasters into a single multi-layer raster.
combined_raster <- c(awc, precip_rasters, evapo_rasters)

# Determine the number of time steps based on the number of precipitation layers.
n_time <- nlyr(precip_rasters)

# Assign layer names to the combined raster:
# - First layer: AWC
# - Next n_time layers: Precipitation time series (named P_1, P_2, …)
# - Following n_time layers: PET time series (named PE_1, PE_2, …)
names(combined_raster) <- c("AWC", paste0("P_", 1:n_time), paste0("PE_", 1:n_time))
# Plot the combined raster stack to verify the layers.
plot(combined_raster)

# -------------------------------
# Define function to compute PDSI (Palmer Drought Severity Index) per pixel
# -------------------------------
compute_pdsi <- function(v, n_time) {
  # 'v' is a vector containing values for one pixel from all layers:
  # v[1]: AWC value
  # v[2] to v[1+n_time]: Precipitation time series
  # v[(2+n_time)] to v[(1+2*n_time)]: PET time series
  
  AWC_value <- v[1]  # Extract the AWC value
  # If AWC is missing, return a vector of NAs with length equal to the number of time steps.
  if (is.na(AWC_value)) return(rep(NA, n_time))
  
  # Extract the precipitation time series
  P_ts <- v[2:(1 + n_time)]
  # Extract the PET time series
  PE_ts <- v[(2 + n_time):(1 + 2 * n_time)]
  
  # If any values in the precipitation or PET series are missing, return NAs.
  if (any(is.na(P_ts)) || any(is.na(PE_ts))) return(rep(NA, n_time))
  
  # Compute the PDSI using the scPDSI package:
  # - Convert AWC_value to an integer.
  # - 'start = 1' indicates the starting index.
  # - 'sc = TRUE' enables the self-calibrating PDSI.
  result <- scPDSI::pdsi(P_ts, PE_ts, as.integer(AWC_value), start = 1, sc = TRUE)
  
  # Convert the result matrix (result$X) to a vector.
  pdsi_values <- as.vector(t(result$X))
  # Return only the first n_time values to match the original time series length.
  return(pdsi_values[1:n_time])
}

# -------------------------------
# Compute PDSI for all pixels using parallel processing
# -------------------------------
# 'app()' applies the compute_pdsi function to each pixel in the combined raster.
# 'n_time' is passed as an argument, and 'cores' (assumed to be defined elsewhere) sets the number of parallel cores.
pdsi_rasters <- app(combined_raster, compute_pdsi, n_time = n_time, cores = cores)

# Assign time metadata to the resulting PDSI raster stack using the previously extracted dates.
names(pdsi_rasters) <- dates
time(pdsi_rasters) <- as.Date(dates, format = "%Y-%m-%d")
# Plot the PDSI raster stack for visual inspection.
plot(pdsi_rasters)

# -------------------------------
# Save the computed PDSI raster stack to disk
# -------------------------------
# Construct an output filename using the first and last year.
output_filename <- paste0("data/climate/drought_indices/PDSI/pdsi_", years[1], "_", years[2], ".tif")
# Write the PDSI raster stack to a GeoTIFF file, overwriting any existing file with the same name.
writeRaster(pdsi_rasters, filename = output_filename, overwrite = TRUE)
