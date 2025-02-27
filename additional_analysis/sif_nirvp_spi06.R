# ---------------------------------------------------------
# Script for Extracting and Analyzing Remote Sensing and Climate Data
# ---------------------------------------------------------
# Description:
# This script extracts remote sensing data (SIF Anomalies, NIRVP, SPI06) 
# for a set of spatial points, resamples these variables, and produces 
# time series plots comparing the rescaled values of these variables.
# The script includes the following main steps:
# 1. Loading and projecting spatial points.
# 2. Loading raster stacks for SIF, NIRVP, and SPI06.
# 3. Extracting data for each point and identifying common time points.
# 4. Rescaling SIF and NIRVP to the SPI06 scale.
# 5. Generating time series plots for each point.
# 6. Optionally saving the plots or combining results for further analysis.

# Load required libraries
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------
# 1. Load spatial points and project them
# ---------------------------
points_sf <- read_sf("data/Untersuchungsgebiete/MW3-Inventar_2024-12-04_all.gpkg")
# Optionally filter to one point (uncomment if needed)
# points_sf <- points_sf[points_sf$`Key-Code` == "dre21", ]

points_vect <- vect(points_sf)  # Convert sf to SpatVector
points_vect <- project(points_vect, "EPSG:31467")  # Reproject

# ---------------------------
# 2. Load the raster stacks
# ---------------------------
sif_anomaly <- rast("data/sif/troposif/sif_anomaly_2018_2024.tif")
nirvp_anomaly <- rast("data/vegetation_indices/nirv_par_anom_2018-05_2024-09.tif")

spi06_file <- list.files("data/climate/drought_indices",
                         full.names = TRUE,
                         pattern = "_1991_2024.*\\.tif$",
                         recursive = TRUE)[20]
spi06 <- rast(spi06_file)

# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(scales)

# ---------------------------
# 3. Loop over each point to extract data and create plots using only common dates
# ---------------------------
for (i in 1:nrow(points_vect)) {
  
  # Extract values for each raster stack at the i-th point
  sif_vals <- terra::extract(sif_anomaly, points_vect[i, ])
  nirvp_vals <- terra::extract(nirvp_anomaly, points_vect[i, ])
  spi06_vals <- terra::extract(spi06, points_vect[i, ])
  
  # Convert extracted values to numeric vectors (remove the ID column)
  sif_values <- as.numeric(sif_vals[, -1])
  nirvp_values <- as.numeric(nirvp_vals[, -1])
  spi06_values <- as.numeric(spi06_vals[, -1])
  
  # Convert layer names to Date objects (assumes names are in "yyyy-mm-dd" format)
  sif_dates <- as.Date(names(sif_anomaly))
  nirvp_dates <- as.Date(names(nirvp_anomaly))
  spi06_dates <- as.Date(names(spi06))
  
  # Find the common dates
  common_dates <- intersect(sif_dates, intersect(nirvp_dates, spi06_dates))
  common_dates <- as.Date(common_dates, origin = "1970-01-01")
  
  # Filter values to keep only those for common dates
  sif_values <- sif_values[sif_dates %in% common_dates]
  nirvp_values <- nirvp_values[nirvp_dates %in% common_dates]
  spi06_values <- spi06_values[spi06_dates %in% common_dates]
  
  # For SIF and NIRvP, rescale to the range of SPI06
  # 1. Get SPI06 range (min and max)
  spi06_min <- min(spi06_values, na.rm = TRUE)
  spi06_max <- max(spi06_values, na.rm = TRUE)
  
  # 2. Define a rescaling function
  rescale_to_spi <- function(x, spi_min, spi_max) {
    # Normalize x to [0,1] based on its own range
    x_norm <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    # Stretch to SPI06's range
    x_rescaled <- spi_min + x_norm * (spi_max - spi_min)
    return(x_rescaled)
  }
  
  # Apply the rescaling function to SIF and NIRvP values
  sif_rescaled <- rescale_to_spi(sif_values, spi06_min, spi06_max)
  nirvp_rescaled <- rescale_to_spi(nirvp_values, spi06_min, spi06_max)
  
  # For SPI06, use the original values so its scale remains unchanged
  spi06_rescaled <- spi06_values
  
  # Optionally, create data frames for each variable (if needed elsewhere)
  df_sif <- data.frame(date = sif_dates[sif_dates %in% common_dates], value = sif_values, variable = "SIF Anomaly")
  df_nirvp <- data.frame(date = nirvp_dates[nirvp_dates %in% common_dates], value = nirvp_values, variable = "NIRVP Anomaly")
  df_spi06 <- data.frame(date = spi06_dates[spi06_dates %in% common_dates], value = spi06_values, variable = "SPI06")
  
  # Combine the data frames (if you need a long format for plotting)
  ts_data <- bind_rows(df_sif, df_nirvp, df_spi06)
  
  # Create a data frame with the rescaled data and dates
  normalized_data <- data.frame(
    Date = as.Date(common_dates),
    SIF = sif_rescaled,
    NIRvP = nirvp_rescaled,
    SPI06 = spi06_rescaled
  )
  
  ID <- points_vect[i, ]$`Key-Code`
  
  # Define year breaks based on the range of your data
  year_breaks <- seq(min(normalized_data$Date), max(normalized_data$Date), by = "year")
  
  # Plot 1: SIF vs SPI06
  plot_sif_spi <- ggplot(normalized_data, aes(x = Date)) +
    geom_line(aes(y = SIF, color = "SIF"), linewidth = 0.7) +
    geom_line(aes(y = SPI06, color = "SPI06"), linewidth = 0.7) +
    scale_x_date(breaks = year_breaks, labels = scales::date_format("%Y")) +
    labs(title = paste0("SIF Anomalies vs SPI06 (Rescaled)", " (ID = ", ID, ")"),
         y = "Value (SPI06 Scale)",
         color = "Variable") +
    theme_minimal()
  
  # Plot 2: NIRvP vs SPI06
  plot_nirvp_spi <- ggplot(normalized_data, aes(x = Date)) +
    geom_line(aes(y = NIRvP, color = "NIRvP"), linewidth = 0.7) +
    geom_line(aes(y = SPI06, color = "SPI06"), linewidth = 0.7) +
    scale_x_date(breaks = year_breaks, labels = scales::date_format("%Y")) +
    labs(title = paste0("NIRvP Anomalies vs SPI06 (Rescaled)", " (ID = ", ID, ")"),
         y = "Value (SPI06 Scale)",
         color = "Variable") +
    theme_minimal()
  
  # Arrange the plots in a grid (requires gridExtra)
  grid.arrange(plot_sif_spi, plot_nirvp_spi, ncol = 1)
}
