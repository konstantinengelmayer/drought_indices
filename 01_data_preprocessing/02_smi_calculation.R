# R Script for Calculating Soil Moisture Index (SMI) from Soil Moisture Data
#
# Description:
# This script computes the Soil Moisture Index (SMI) using a kernel density-based approach 
# for monthly soil moisture values over a specified time period. The SMI values are computed 
# for each pixel across all layers in a raster stack, ensuring seasonal comparability.
#
# Workflow Overview:
# 1. **Library Loading**:
#    - Imports required libraries (`terra`, `stringr`) for raster data manipulation and SMI computation.
#
# 2. **Setup**:
#    - Sets the working directory and initializes parallel processing.
#
# 3. **Data Preparation**:
#    - Loads the soil moisture raster data.
#    - Extracts the time information and assigns corresponding months for each layer.
#
# 4. **Function Definition**:
#    - Defines a custom function to compute SMI per pixel using kernel density estimation (KDE) 
#      and cumulative distribution functions (CDFs) for each month.
#
# 5. **SMI Calculation**:
#    - Applies the SMI computation function across all pixels using parallel processing.
#
# 6. **Output**:
#    - Saves the computed SMI raster stack as a GeoTIFF file.
#    - Optionally visualizes the computed SMI values.
#
# Notes:
# - Ensure the soil moisture raster input file is correctly formatted and located in the specified directory.
# - Adjust the working directory
# - Adjust the number of cores (`cores`) as per system capability to optimize performance.

# Load required libraries
library(terra)
library(stringr)

#############################Adjust here#############################
# Set working directory
setwd("C:/Users/konst/Documents/")
cores <- 14  # Number of cores for parallel processing
####################################################################

# -------------------------------
# Load and preprocess soil moisture data
# -------------------------------

# Load the merged soil moisture raster (covering the period 1991-2024)
soil_moist <- rast("data/climate/soil_moist/soi_moist_1991_2024_merged.tif")

# Extract the years from the source information (filenames) using a regular expression that finds 4-digit numbers.
years <- str_extract_all(sources(soil_moist), "\\d{4}")[[1]]

# Convert the layer names (assumed to be date strings) into Date objects.
dates <- as.Date(names(soil_moist), format = "%Y-%m-%d")

# From these dates, extract the numeric month (1 to 12) for each layer.
months_numeric <- as.numeric(format(dates, "%m"))


# -------------------------------
# Load and align soil saturated water content (NFKW) data
# -------------------------------

# Load the NFKW raster (representing saturated water content) from the soil directory.
nfkw <- rast("data/soil/NFKWe1000_250/NFKWe1000_250.tif")

# Reproject the NFKW raster to the coordinate system "EPSG:31467" to match the other spatial data.
nfkw <- project(nfkw, "EPSG:31467")

# Resample the NFKW raster so that its resolution and extent align with the first layer of the soil moisture raster.
# The bilinear interpolation method is used to smoothly adjust the values.
nfkw <- resample(nfkw, soil_moist[[1]], method = "bilinear")


# -------------------------------
# Normalize the soil moisture data
# -------------------------------

# Normalize soil moisture by dividing each soil moisture value by the corresponding saturated water content value.
# This creates a normalized soil moisture index.
soil_moist_normalized <- soil_moist / nfkw

# Plot the normalized soil moisture raster for visual inspection.
plot(soil_moist_normalized)


# -------------------------------
# Define a function to compute the Soil Moisture Index (SMI) for a single pixel over time
# -------------------------------
smi_function <- function(cell_values, months_numeric) {
  # Determine the number of time steps (layers) for the current pixel
  nlyr <- length(cell_values)
  
  # Initialize a vector to store the computed SMI values; default all values to NA.
  smi_values <- rep(NA, nlyr)
  
  # Identify all unique months present in the time series (e.g., 1 for January, 2 for February, etc.).
  unique_months <- unique(months_numeric)
  
  # Prepare a list to store the cumulative distribution function (CDF) for each month.
  # The list has an element for each month up to the maximum month number.
  cdf_funcs <- vector("list", max(months_numeric))
  
  # Precompute the density estimates and corresponding CDF functions for each unique month.
  for (m in unique_months) {
    # Find the indices in the time series that correspond to the current month 'm'.
    month_indices <- which(months_numeric == m)
    
    # Extract all soil moisture values for month 'm' over all years.
    x_month <- cell_values[month_indices]
    
    # Remove any NA values from the data.
    x_month <- x_month[!is.na(x_month)]
    
    # If there is insufficient data (fewer than 2 values), set the CDF function to NULL.
    if (length(x_month) < 2) {
      cdf_funcs[[m]] <- NULL
    } else {
      # Attempt to estimate the kernel density using a Gaussian kernel and an automatic bandwidth selection ("ucv").
      # If an error occurs (e.g., due to problematic data), fall back to a fixed bandwidth of 0.1.
      density_est <- tryCatch(
        density(x_month, kernel = "gaussian", bw = "ucv", n = 512),
        error = function(e) density(x_month, kernel = "gaussian", bw = 0.1, n = 512)
      )
      
      # Create the cumulative distribution function (CDF) from the density estimate:
      # - 'cdf_x' are the points at which the density is estimated.
      # - 'cdf_y' is the cumulative sum of the density values, normalized so that the maximum is 1.
      cdf_x <- density_est$x
      cdf_y <- cumsum(density_est$y) / sum(density_est$y)
      
      # Construct an interpolation function (approxfun) for the CDF.
      # The function returns 0 for values below the range and 1 for values above the range.
      cdf_func <- approxfun(cdf_x, cdf_y, yleft = 0, yright = 1)
      
      # Save the CDF function for month 'm' in the list.
      cdf_funcs[[m]] <- cdf_func
    }
  }
  
  # Compute the SMI values for each time step in the time series for the pixel.
  for (t in 1:nlyr) {
    # Extract the observed soil moisture value at time 't'.
    x <- cell_values[t]
    # Determine the month corresponding to time 't'.
    m <- months_numeric[t]
    
    # If the observed value is NA or if there is no valid CDF function for that month, assign NA.
    if (is.na(x) || is.null(cdf_funcs[[m]])) {
      smi_values[t] <- NA
    } else {
      # Otherwise, compute the SMI as the cumulative probability of observing the value 'x'
      # based on the precomputed CDF for that month.
      smi_values[t] <- cdf_funcs[[m]](x)
    }
  }
  
  # Return the vector of computed SMI values for this pixel.
  return(smi_values)
}


# -------------------------------
# Apply the SMI calculation across all pixels using parallel processing
# -------------------------------
# The 'app' function applies the 'smi_function' to each pixel in the soil moisture raster.
# It passes the 'months_numeric' vector to the function and uses the specified number of CPU cores (cores).
smi_raster <- app(soil_moist, smi_function, months_numeric = months_numeric, cores = cores)

# Assign names to the SMI raster layers based on the dates.
names(smi_raster) <- dates

# Set the time dimension (metadata) of the SMI raster using the dates.
time(smi_raster) <- as.Date(dates, format = "%Y-%m-%d")

# Plot the computed SMI raster stack for visual inspection.
plot(smi_raster)


# -------------------------------
# Save the computed SMI raster stack to disk
# -------------------------------
# Create an output filename using the first and last year extracted earlier.
output_filename <- paste0("data/climate/drought_indices/SMI/test_smi_", years[1], "_", years[2], ".tif")

# Write the SMI raster stack to a GeoTIFF file. The 'overwrite = TRUE' option allows replacing any existing file.
writeRaster(smi_raster, output_filename, overwrite = TRUE)

# Plot the resulting SMI raster one final time.
plot(smi_raster)
