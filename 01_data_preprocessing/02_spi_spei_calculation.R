# R Script for Calculating SPI and SPEI Indices from Climate Data

# Description:
# This script computes Standardized Precipitation Index (SPI) and Standardized Precipitation-Evapotranspiration Index (SPEI) 
# for given precipitation and potential evapotranspiration (PET) data over specified temporal scales.
# The outputs are saved as raster files for further analysis and visualization.

# Workflow Overview:
# 1. **Library Loading**:
#    - Imports required libraries (`terra`, `SPEI`, `stringr`, `parallel`) for raster data manipulation and computations.

# 2. **Setup**:
#    - Sets the working directory and defines required scales, cores for parallel processing, and output folders.
#    - Creates output folders for SPI and SPEI indices if they do not exist.

# 3. **Data Preparation**:
#    - Loads precipitation and PET data as raster stacks.
#    - Computes the climatic water balance (P - PET).

# 4. **Function Definitions**:
#    - Defines functions to compute SPEI and SPI values for each cell, handling missing values appropriately.

# 5. **Index Calculation and Export**:
#    - Loops through specified temporal scales, calculates SPEI and SPI, and writes the outputs as GeoTIFF files.
#    - Provides feedback on successful file creation.

# Notes:
# - Set your working directory
# - Scales controls how many month to consider for drought indices calculation
# - Parallel processing is used to speed up computations; adjust `cores` if needed.

# Load necessary libraries
library(terra)
library(SPEI)
library(stringr)
library(parallel)

#############################Adjust here#############################
# Set Your Project Route Directory here
setwd("C:/Users/konst/Documents/")

# Define scales and number of cores for parallel processing
scales <- c(1, 2, 3, 4, 5, 6, 9, 12)
cores <- 15
####################################################################
# Create the main output directory for drought indices if it doesn't exist.
if (!dir.exists("data/climate/drought_indices/")) {
  # 'recursive = TRUE' ensures that any necessary parent directories are created.
  dir.create("data/climate/drought_indices/", recursive = TRUE)
}

# Create a subdirectory for SPEI (Standardized Precipitation-Evapotranspiration Index) outputs.
if (!dir.exists("data/climate/drought_indices/SPEI/")) {
  dir.create("data/climate/drought_indices/SPEI/", recursive = TRUE)
}

# Create a subdirectory for SPI (Standardized Precipitation Index) outputs.
if (!dir.exists("data/climate/drought_indices/SPI/")) {
  dir.create("data/climate/drought_indices/SPI/", recursive = TRUE)
}

# -------------------------------------------
# Load and prepare input data
# -------------------------------------------

# Load precipitation data:
# - List all .tif files in the "data/climate/precipitation" folder,
# - Create a raster stack from these files using the 'rast()' function.
precip_rasters <- rast(list.files("data/climate/precipitation", pattern = "\\.tif$", full.names = TRUE))

# Extract the years from the filenames of the precipitation data.
# This uses a regular expression to find any four-digit number in the source strings.
years <- str_extract_all(sources(precip_rasters), "\\d{4}")[[1]]

# Retrieve the names of each raster layer, which are assumed to represent dates.
dates <- names(precip_rasters)

# Load PET (Potential Evapotranspiration) data similarly from its folder.
evapo_rasters <- rast(list.files("data/climate/evapo_p", pattern = "\\.tif$", full.names = TRUE))

# Compute the climatic water balance by subtracting PET from precipitation.
# A positive value indicates a water surplus, while a negative value indicates a deficit.
water_balance_rasters <- precip_rasters - evapo_rasters

# -------------------------------------------
# Define functions for drought index calculations
# -------------------------------------------

# Function to compute SPEI (Standardized Precipitation-Evapotranspiration Index)
compute_spei <- function(x, scale) {
  # If all values in the input vector 'x' are missing (NA), return a vector of NAs.
  if (all(is.na(x))) {
    return(rep(NA, length(x)))
  } else {
    # Otherwise, compute the SPEI using the 'spei' function from the SPEI package.
    # 'na.rm = TRUE' ignores missing values during computation.
    # The '$fitted' component extracts the fitted index values.
    spei_values <- SPEI::spei(x, scale = scale, na.rm = TRUE)$fitted
    return(spei_values)
  }
}

# Function to compute SPI (Standardized Precipitation Index)
compute_spi <- function(x, scale) {
  # If all values in the input vector 'x' are missing, return a vector of NAs.
  if (all(is.na(x))) {
    return(rep(NA, length(x)))
  } else {
    # Otherwise, compute the SPI using the 'spi' function from the SPEI package.
    # The '$fitted' component extracts the fitted index values.
    spi_values <- SPEI::spi(x, scale = scale)$fitted
    return(spi_values)
  }
}

# -------------------------------------------
# Loop through various temporal scales to compute and save indices
# -------------------------------------------

# Loop over each scale in the pre-defined 'scales' vector.
# 'cores' is assumed to be defined elsewhere to specify the number of CPU cores for parallel processing.
for (i in scales) {
  
  # --- SPEI Computation and Saving ---
  
  # Apply the 'compute_spei' function over the water balance raster stack.
  # 'app()' applies a function to each cell across the raster stack.
  # The 'scale = i' parameter defines the time scale (e.g., 1-month, 3-month) for the SPEI calculation.
  spei_rasters <- app(water_balance_rasters, compute_spei, scale = i, cores = cores)
  
  # Set the time attribute of the SPEI raster stack using the extracted dates.
  time(spei_rasters) <- as.Date(dates, format = "%Y-%m-%d")
  
  # Set the layer names of the raster stack to the corresponding dates.
  names(spei_rasters) <- dates
  
  # Plot the SPEI raster stack for visual inspection.
  plot(spei_rasters)
  
  # Construct the output filename for the SPEI raster.
  # The filename includes the first and last year and the current scale value.
  spei_file <- paste0("data/climate/drought_indices/SPEI/SPEI_", years[1], "_", years[2], "_", i, "_month.tif")
  
  # Save the SPEI raster stack as a GeoTIFF file, overwriting any existing file.
  writeRaster(spei_rasters, spei_file, overwrite = TRUE)
  
  # Print a message to the console indicating successful writing.
  cat("SPEI raster written to:", spei_file, "\n")
  
  # --- SPI Computation and Saving ---
  
  # Apply the 'compute_spi' function over the precipitation raster stack.
  # Note: Although a 'compute_spi' function is defined, the code below uses 'compute_spei'
  # for SPI calculation. This may be an oversight and might need to be corrected to 'compute_spi'.
  spi_rasters <- app(precip_rasters, compute_spei, scale = i, cores = cores)
  
  # Set the time attribute for the SPI raster stack.
  time(spi_rasters) <- as.Date(dates, format = "%Y-%m-%d")
  
  # Set the layer names of the SPI raster stack.
  names(spi_rasters) <- dates
  
  # Plot the SPI raster stack for visual inspection.
  plot(spi_rasters)
  
  # Construct the output filename for the SPI raster.
  spi_file <- paste0("data/climate/drought_indices/SPI/SPI_", years[1], "_", years[2], "_", i, "_month.tif")
  
  # Save the SPI raster stack as a GeoTIFF file, overwriting any existing file.
  writeRaster(spi_rasters, spi_file, overwrite = TRUE)
  
  # Print a message to the console indicating where the SPI raster was saved.
  cat("SPI raster written to:", spi_file, "\n")
}

