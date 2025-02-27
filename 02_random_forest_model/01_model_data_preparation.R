# ---------------------------------------------------------
# Script for Extracting and Merging Environmental Data for Analysis
# ---------------------------------------------------------
# Description:
# This script extracts various environmental data layers (correlation,
# topography, tree cover density, aridity index, and research area) at
# specific points from a correlation raster. It then merges the extracted
# data into a single dataset, performs spatial extraction, handles missing 
# research area data by assigning values from the nearest polygon, and 
# finally outputs the dataset to a CSV file for further analysis.
#
# The script includes the following key steps:
# 1. Load environmental data layers including correlation raster, 
#    topographics, tree cover density, aridity index, and research area.
# 2. Extract topographic, tree cover, aridity index, and research area 
#    values for the points in the correlation raster.
# 3. Handle missing research area values by assigning the nearest polygon's 
#    value to the points with NA values.
# 4. Merge all extracted data into a final dataframe.
# 5. Output the final data to a CSV file.

# Load libraries
library(terra)

# -------------------------------------------------------------------
# 1. Load Data
# -------------------------------------------------------------------
# Load the correlation raster
correlation_raster <- rast("data/correlation/sif_anomaly/correlation_sif_SPI_1991_2024_06_month.tif")
topographics <- rast("data/topographics/topographics.tif")
tree_cover_density <- rast("data/tree_cover_density/tcd_resampled_reprojected.tif")
ai <- rast("data/climate/drought_indices/AI/aridity_index.tif")
aoi <- vect("data/Untersuchungsgebiete/all_merged_with_east.shp")  # Research area shapefile
aoi <- project(aoi, ai)


# Convert the correlation raster to a data frame with xy coordinates
correlation_df <- as.data.frame(correlation_raster, xy = TRUE)

# -------------------------------------------------------------------
# 2. Extract Topographic Values
# -------------------------------------------------------------------
# Convert correlation data frame to a SpatVector
pts <- vect(correlation_df, geom = c("x", "y"), crs = crs(correlation_raster))

# Extract topographic values at the points
vals_topo <- extract(topographics, pts)

# -------------------------------------------------------------------
# 3. Extract Tree Cover Density, AI values, and Research Area
# -------------------------------------------------------------------
# Extract tree cover density values at the points
vals_treecover <- extract(tree_cover_density, pts)
vals_ai <- extract(ai, pts)

# Extract research area values at the points
vals_area <- extract(aoi, pts)

# For any points where the research area is still NA, assign the value from the nearest polygon
# Assuming the first column in vals_area is the ID and the second column holds the research area attribute.
na_idx <- which(is.na(vals_area[[3]]))
if(length(na_idx) > 0){
  # Compute the distances between the NA points and all polygons
  dists <- distance(pts[na_idx], aoi)
  # For each point, find the index of the nearest polygon
  nearest_poly <- apply(dists, 1, which.min)
  # Assign the research area from the nearest polygon to the NA entries
  vals_area[na_idx, 3] <- aoi$AreaName[nearest_poly]
}

# -------------------------------------------------------------------
# 4. Merge Extracted Data into the Correlation Data Frame
# -------------------------------------------------------------------
# Combine correlation data frame with extracted values
correlation_df_extended <- cbind(
  correlation_df,
  vals_topo[ , -1],        # Topographic layers
  vals_treecover[ , -1],   # Tree cover density layer
  vals_ai[ , -1],          # Aridity index layer
  vals_area[ , 3]         # Research area layer
)

# Rename columns for clarity (adjust indexes if needed)
names(correlation_df_extended)[c(11, 12, 13)] <- c("tree_cover_density", "aridity_index", "aoi")

# Final dataset selection, including research area
data <- correlation_df_extended[, c(3, 5:12, ncol(correlation_df_extended))]

# Write the final CSV file
write.csv(data, "data/model_data/model_data.csv", row.names = FALSE)

