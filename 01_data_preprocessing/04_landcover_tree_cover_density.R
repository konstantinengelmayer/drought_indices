# R Script for Processing Tree Cover Density and Land Cover Data

# Description:
# This script processes the tree cover density and land cover raster datasets, 
# performing a series of operations including projection, resampling, cropping, 
# and thresholding to prepare data for further analysis or modeling.
#
# Workflow:
# 1. Load tree cover density and climate temperature data.
# 2. Reproject the tree cover density raster to match the target raster.
# 3. Resample the tree cover density raster to match the spatial resolution of the target raster.
# 4. Crop the tree cover density raster to the extent of the target raster, applying a mask.
# 5. Modify the tree cover density values by setting the value of 255 to 0 and save the result.
# 6. Load the land cover raster and reproject it to match the target raster.
# 7. Apply a Boolean mask to extract land cover values for specific land types.
# 8. Save the processed land cover raster to disk.
#
# Data Requirements:
# - Tree Cover Density raster: A raster representing the density of tree cover.
# - Target climate raster: A raster containing climate data (e.g., air temperature).
# - Land cover raster: A raster representing land cover types (e.g., from CORINE Land Cover dataset).
#
# Outputs:
# - Resampled, reprojected, and cropped tree cover density data.
# - Processed Boolean land cover mask representing specific land types.

library(terra)

# Set Your Project Route Directory here
setwd("C:/Users/konst/Documents/")

# Load the tree cover density raster for the year 2018
tree_cover_density <- rast("/data/tree_cover_density/142977/Results/TCD_2018_100m_eu_03035_v020/TCD_2018_100m_eu_03035_v020/DATA/TCD_2018_100m_eu_03035_V2_0.tif")

# Load the target raster (e.g., air temperature data) for resampling and projection
target <- rast("data/climate/air_temp_max/air_temp_max_1991_2024_merged.tif")

# Reproject tree cover density to match the target raster's coordinate reference system (CRS)
tree_cover_density <- project(tree_cover_density, target)

# Resample tree cover density raster to match the spatial resolution of the target raster
tree_cover_density <- resample(tree_cover_density, target)

# Crop the tree cover density raster to the extent of the target raster and mask areas outside the extent
tree_cover_density <- crop(tree_cover_density, target, mask = TRUE)

# Extract the second band of the tree cover density raster (assuming this contains the relevant data)
tcd <- tree_cover_density[[2]]

# Set the value of 255 to 0 in the tree cover density raster (assuming 255 represents no data or an invalid value)
tcd[tcd[] == 255] <- 0

# Save the processed tree cover density raster to disk
writeRaster(tcd, "data/tree_cover_density/tcd_resampled_reprojected.tif")

# Load the land cover raster
landcover <- rast("/data/landcover/142989/Results/U2018_CLC2018_V2020_20u1/U2018_CLC2018_V2020_20u1/U2018_CLC2018_V2020_20u1.tif")

# Reproject the land cover raster to match the target raster
landcover <- project(landcover, target)

# Create a Boolean mask for land cover values between 20 and 29 (representing specific land cover types)
landcover_boolean <- (landcover >= 20 & landcover <= 29)

# Save the processed Boolean land cover mask to disk
writeRaster(landcover_boolean, "data/landcover/landcover_boolean.tif")
