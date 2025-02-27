################################################################################
# Script: Correlation between SIF data and multiple drought indices
# 
# Description:
#   1) Load and preprocess SIF data, including STL decomposition to get a 
#      "remainder" layer (detrended + deseasonalized).
#   2) Load and resample drought indices to match SIF data.
#   3) For each Area of Interest (AOI), crop/mask SIF and drought rasters, 
#      then compute pixel-wise correlation.
#   4) Summarize results (fraction of significant pixels, mean correlation) 
#      and produce a final table and heatmap plot.
################################################################################
# --- Libraries ---
library(terra)      # For raster and vector operations
library(parallel)   # For potential parallel processing
library(dplyr)      # Data manipulation
library(sf)         # Handling spatial vector data
library(tidyr)      # Data reshaping
library(ggplot2)    # Plotting

# --- 1. Load AOI and Prepare Shapefile ---
# Load the shapefile of Areas of Interest (AOIs)
aoi <- vect("C:/Users/konst/Documents/Hiwi/mw3/drought_indicies/data/Untersuchungsgebiete/all_merged_with_east.shp")
# Reproject the AOIs to match the CRS of the drought indices (EPSG:31467)
aoi <- project(aoi, "EPSG:31467")
# Exclude AOIs where AreaName equals "MeckVorp"
aoi <- aoi[aoi$AreaName != "MeckVorp", ]
# Save AOI names for later use
aoi_names <- aoi$AreaName

# --- 2. Load Drought Indices and SIF Anomaly Data ---
# List drought index raster files from the 'data/climate/' directory
variable_files <- list.files(
  path = "data/climate/",
  pattern = "_1991_2024.*\\.tif$",  # Files with '_1991_2024' in their name and ending with .tif
  full.names = TRUE,
  recursive = TRUE
)[2:27]  # Example: select files 2 through 27
# Remove any files that contain "sif" in their name (we load SIF separately)
variable_files <- variable_files[!grepl("sif", variable_files)]

# Use the first drought file to obtain reference resolution for resampling
reso <- rast(variable_files[1])

# --- Load and Preprocess SIF Anomaly Data ---
# Load SIF anomaly raster (e.g., from TROPOMI)
sif_anomaly <- rast("data/sif/troposif/sif_anomaly_2018_2024.tif")
# Reproject SIF anomaly to EPSG:31467
sif_anomaly <- project(sif_anomaly, "EPSG:31467")
# Resample SIF anomaly to match the resolution of the drought file
sif_anomaly <- terra::resample(sif_anomaly, reso)
# Subset SIF anomaly layers to include only the dates from 2018-05-01 to 2024-06-01 (monthly)
sif_anomaly <- sif_anomaly[[as.character(seq(as.Date("2018-05-01"), by = "month", to = as.Date("2024-06-01")))]]

# Load landcover raster and set non-land pixels (0) to NA
landcover <- rast("data/landcover/landcover_boolean.tif")
landcover[landcover[] == 0] <- NA

# Crop the SIF anomaly data to the extent of the landcover (masking non-land areas)
sif_anomaly <- crop(sif_anomaly, landcover, mask = TRUE)
# Plot SIF anomaly to check results
plot(sif_anomaly)

# --- 4. Define Pixel-wise Correlation Function ---
# This function computes Spearman's correlation between two time series: SIF and drought values.
# It returns a vector of two values: correlation coefficient (r) and p-value.
corr_pixel_fun <- function(x) {
  n <- length(x)             # Total number of elements (combined time series)
  half <- n / 2              # Assume first half is SIF and second half is drought index
  sif_vals <- x[1:half]      # Extract SIF values
  drought_vals <- x[(half+1):n]  # Extract drought values
  
  # If all values are NA in either series, return NA for both r and p-value
  if (all(is.na(sif_vals)) || all(is.na(drought_vals))) return(c(NA, NA))
  # Identify indices where both series have non-missing data
  good_idx <- which(!is.na(sif_vals) & !is.na(drought_vals))
  # Require at least 3 data points for correlation computation
  if (length(good_idx) < 3) return(c(NA, NA))
  
  # Perform Spearman correlation test on valid data points
  test <- cor.test(sif_vals[good_idx], drought_vals[good_idx], method = "spearman")
  c(test$estimate, test$p.value)  # Return correlation coefficient and p-value
}

# --- 5. Correlation for a Single AOI ---
# This function calculates pixel-wise correlation between SIF anomalies and a drought index
# within a given AOI, then aggregates the results.
correlate_sif_drought <- function(aoi_poly, sif_rast, drought_file, aoi_id) {
  # Load the drought raster file
  drought_rast <- rast(drought_file)
  
  # Crop and mask both SIF and drought rasters to the AOI polygon
  sif_aoi <- mask(crop(sif_rast, aoi_poly), aoi_poly)
  drought_aoi <- mask(crop(drought_rast, aoi_poly), aoi_poly)
  
  # Find common layers between SIF and drought data
  common_layers <- intersect(names(sif_aoi), names(drought_aoi))
  drought_aoi <- drought_aoi[[common_layers]]
  sif_aoi <- sif_aoi[[common_layers]]
  
  # Optionally, keep only the months April to September (filter layer names)
  idx_keep <- grep("-0[4-9]-", names(sif_aoi))
  sif_aoi <- sif_aoi[[idx_keep]]
  drought_aoi <- drought_aoi[[idx_keep]]
  
  # Combine SIF and drought data for pixel-wise correlation (stack them)
  stacked_r <- c(sif_aoi, drought_aoi)
  # Apply the correlation function to each pixel (across the time series)
  cor_r <- app(stacked_r, corr_pixel_fun)
  
  # Extract correlation and p-values from the resulting raster
  vals_cor <- values(cor_r)  # Matrix with 2 columns: [r, p]
  
  pvals <- vals_cor[, 2]
  rvals <- vals_cor[, 1]
  total_pixels <- sum(!is.na(pvals))
  
  # Calculate fraction of pixels with significant correlation (p < 0.05)
  sig_pixels <- sum(pvals < 0.05, na.rm = TRUE)
  frac_sig <- sig_pixels / total_pixels
  
  # Calculate mean correlation (overall and for significant pixels only)
  mean_r_all <- mean(rvals, na.rm = TRUE)
  mean_r_sig <- mean(rvals[pvals < 0.05], na.rm = TRUE)
  
  # Return a data frame summarizing the results for this AOI and drought file
  data.frame(
    aoi_id = aoi_id,
    drought_name = basename(drought_file),
    frac_sig_pixels = frac_sig,
    mean_r_all = mean_r_all,
    mean_r_sig = mean_r_sig
  )
}

# --- 6. Master Loop: Compute Correlations Across AOIs and Drought Files ---
# Initialize a list to store results from each AOI-drought file combination
results_list <- list()

# Loop over each AOI polygon
for (i in seq_len(nrow(aoi))) {
  aoi_poly <- aoi[i]               # Current AOI polygon
  aoi_name_val <- aoi_names[i]     # Name of the current AOI (e.g., "Eifel")
  
  # Use the preprocessed SIF anomaly data
  current_sif <- sif_anomaly
  
  # Loop through each drought file in the list
  for (dfile in variable_files) {
    df_row <- correlate_sif_drought(
      aoi_poly  = aoi_poly,
      sif_rast  = current_sif,
      drought_file = dfile,
      aoi_id    = aoi_name_val
    )
    # Append the result row to the results list
    results_list[[length(results_list) + 1]] <- df_row
  }
}

# Combine all result rows into a single data frame
final_results <- do.call(rbind, results_list)

# --- 7. Post-Processing and Visualization ---
# Prepare the results for visualization and further analysis
results_anomaly <- final_results

# Reshape to wide format: one row per AOI, columns for each drought index's mean correlation
results_anomaly_wide <- results_anomaly %>%
  select(aoi_id, drought_name, mean_r_all) %>%
  pivot_wider(
    names_from = drought_name,
    values_from = mean_r_all
  )

# Clean up drought index names (remove extra parts like underscores, year ranges, and file extensions)
results_anomaly$drought_name <- sub("_[0-9]{4}_[0-9]{4}|\\.tif", "", results_anomaly$drought_name)

# Create a tile plot showing the fraction of significant pixels, with mean correlation values as labels
p <- ggplot(results_anomaly,
            aes(x = drought_name,
                y = factor(aoi_id),
                fill = ifelse(frac_sig_pixels > -10, frac_sig_pixels, NA))) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(frac_sig_pixels > -10, round(mean_r_all, 2), "")), 
            color = "black", size = 3) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red",
                       midpoint = 0.5, limits = c(0, 1), na.value = "white", name = "Fraction Sig.") +
  labs(
    title = "Mean Pixel-Wise Correlation of NDVI Anomaly vs. Drought Indices\n(April to September, 2018 to 2024)",
    x = "Drought Index",
    y = "AOI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# Save the plot as a PNG file
ggsave("figures/correlations/correlation_ndvi_anomaly_drought_indices.png",
       plot = p, width = 12, height = 6, dpi = 300, bg = "white")

# Summarize the mean correlation over all areas for each drought index
results_anomaly_summary <- results_anomaly %>%
  group_by(drought_name) %>%
  summarize(
    mean_r_over_all_areas = mean(mean_r_all, na.rm = TRUE),
    .groups = "drop"
  )

# Print the summary statistics to the console
print(results_anomaly_summary)

# Order the summary results by mean correlation and export to an Excel file
results_anomaly_summary <- results_anomaly_summary[order(results_anomaly_summary$mean_r_over_all_areas), ]
library(writexl)
write_xlsx(results_anomaly_summary, "figures/correlations/correlation_sif_anomaly_drought_indices.xlsx")