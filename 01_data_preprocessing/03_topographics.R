# R Script for Topographic and Hydrological Analysis

# Description:
# This script performs a series of topographic and hydrological analyses, including:
# - Loading and preprocessing of Digital Elevation Model (DEM) and climate data.
# - Resampling and reprojecting DEM data to match the resolution and projection of other datasets.
# - Calculating topographic attributes such as slope, aspect, TPI (Topographic Position Index), and TRI (Terrain Ruggedness Index).
# - Performing hydrological analyses, including sink filling, flow direction, and flow accumulation.
# - Calculating the Topographic Wetness Index (TWI) based on flow accumulation and slope.
#
# Inputs:
# - DEM raster file: Digital Elevation Model data in ASCII format.
# - Climate data raster: Maximum air temperature data (1991–2024) in TIFF format.
# - Area of Interest (AOI) shapefile: Polygon shapefile defining the study area.
#
# Outputs:
# - Reprojected and resampled DEM file: Adjusted to match the target climate data.
# - Topographic attributes raster files: Slope, aspect, TPI, and TRI.
# - Hydrological analysis files: Filled DEM, flow direction, flow accumulation, and TWI rasters.
#
# Notes:
# - Ensure that the input datasets (DEM, air temperature, and AOI) are correctly aligned in terms of spatial reference systems and extents.
# - The script uses the WhiteboxTools library for hydrological analyses (sink filling, flow direction, and flow accumulation).
# - Outputs will be saved in specified directories for further use in analysis or visualization.

# Libraries used in the script
library(terra)      # For raster data manipulation and analysis
library(mapview)    # For interactive mapping (optional)
library(whitebox)   # For advanced geospatial and hydrological analysis
library(sf)         # For handling vector data (shapefiles)

# Set Your Project Route Directory here
setwd("C:/Users/konst/Documents/")

# 1. Load the Digital Elevation Model (DEM) data
dem <- rast("data/topographics/dgm200.utm32s.gridascii/dgm200.utm32s.gridascii/dgm200/dgm200_utm32s.asc")

# 2. Reproject DEM to match the target CRS (EPSG:31467)
dem <- project(dem, "EPSG:31467")

# List all files in the "data/climate/air_temp_max/" directory that end with "merged.tif"
target_files <- list.files("data/climate/air_temp_max/", pattern = "merged\\.tif$", full.names = TRUE)

# Read all the matched files as a raster stack
target <- rast(target_files)

# Check the files being read
print(target_files)

# 4. Resample DEM to match the resolution and extent of the air temperature data
dem <- resample(dem, target)

# 5. Save the reprojected and resampled DEM as a new raster file
writeRaster(dem, "data/topographics/dem_resampled_reprojected.tif")

# 6. Load Area of Interest (AOI) shapefile (study area boundary)
aoi <- vect("data/Untersuchungsgebiete/all_merged_with_east.shp")

# 7. Reproject AOI to match the CRS of the DEM
aoi <- project(aoi, "EPSG:31467")

# 8. Crop DEM to the AOI, mask areas outside the AOI
dem <- crop(dem, aoi, mask = TRUE)

# 9. Calculate topographic attributes using terrain analysis (slope, aspect, TPI, TRI)
topographics <- terrain(dem, c("slope", "aspect", "TPI", "TRI"), unit = "radians")

# 10. Fill sinks (depressions) in the DEM using WhiteboxTools
wbt_fill_depressions("data/topographics/dem_resampled_reprojected.tif", "data/temp/filled_dem.tif")

# 11. Load the filled DEM
filled_dem <- rast("data/temp/filled_dem.tif")

# 12. Calculate flow direction using the D8 algorithm from WhiteboxTools
wbt_d8_pointer("data/temp/filled_dem.tif", "data/temp/flowdir.tif")

# 13. Load the flow direction raster
flowdir <- rast("data/temp/flowdir.tif")

# 14. Calculate flow accumulation using WhiteboxTools
wbt_d8_flow_accumulation("data/temp/filled_dem.tif", "data/temp/flowacc.tif")

# 15. Load the flow accumulation raster
flowacc <- rast("data/temp/flowacc.tif")

# 16. Calculate the Topographic Wetness Index (TWI) using the formula: TWI = ln(a / tan(beta))
# where 'a' is the flow accumulation and 'beta' is the slope
slope_adjusted <- tan(topographics[[1]]) + 1e-6  # Avoid division by zero by adding a small value
twi <- log(flowacc / slope_adjusted)  # Compute the TWI


# Merge everything
topographics <- c(dem, topographics, twi)

names(topographics) <- c("dem", "slope", "aspect", "TPI", "TRI", "TWI")
writeRaster(topographics, "data/topographics/topographics.tif", datatype = "FLT4S")

