# R Script for Comparing Long-Term Monthly Climate Data Across Research Areas

# Description:
# This script analyzes and compares long-term monthly climate data across multiple research areas. It:
# - Loads spatial data for different research areas.
# - Extracts climate variables (e.g., precipitation, soil moisture, evaporation, maximum air temperature) for these areas over time.
# - Calculates monthly mean values for each variable and area.
# - Visualizes the data in a faceted line plot to compare climate trends among the areas.

# Workflow Overview:
# 1. Library Loading:
#    - Imports necessary libraries for spatial data manipulation and visualization (`terra`, `dplyr`, `ggplot2`, `tidyr`).

# 2. Data Loading:
#    - Loads spatial vector data for research areas from shapefiles.
#    - Loads raster data for climate variables covering the period 1991-2024.

# 3. Data Preparation:
#    - Reprojects research areas to match the coordinate reference system (CRS) of the climate data.
#    - Defines a function to calculate monthly mean values for each area and climate variable.

# 4. Data Analysis:
#    - Applies the function to calculate monthly means for all combinations of areas and variables.
#    - Aggregates the results into a single dataframe for plotting.

# 5. Visualization:
#    - Creates a faceted line plot using `ggplot2` to display monthly mean climate variables for each area.
#    - Saves the plot to a specified directory.

# Data Requirements:
# - Research Areas: Shapefiles (`.shp`) of the study areas.
# - Climate Data: Raster datasets (`.tif`) for precipitation, soil moisture, evaporation, and maximum air temperature from 1991 to 2024.


# Load necessary libraries
library(terra)    # For spatial data manipulation
library(dplyr)    # For data manipulation and summarization
library(ggplot2)  # For plotting
library(tidyr)    # For reshaping data

# Set Your Project Route Directory here
setwd("C:/Users/konst/Documents/")

# Load Research Areas (polygon shapefiles)
# The areas are defined using vector data of specific regions
areas <- list(
  Eifel = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_Eifel.shp"),
  Kellerwald = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_Kellerwald.shp"),
  Koenigsforst = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_Koenigsforst.shp"),
  Lahntal = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_Lahntal.shp"),
  Lindenbergerwald = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_LindenbergerWald.shp"),
  HohesHolz = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_HohesHolz.shp"),
  Mueritz = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_Mueritz.shp"),
  Wendeforst = vect("data/Untersuchungsgebiete/002a_Sen1-Subset_Wendeforst.shp")
)

# Load Climate Data
# The climate data is stored as raster objects representing various climate variables
climate_data <- list(
  Precipitation = rast("data/climate/precipitation/precipitation_1991_2024_merged.tif"),
  Soil_Moisture = rast("data/climate/soil_moist/soi_moist_1991_2024_merged.tif"),
  Evaporation = rast("data/climate/evapo_p/evapo_p_1991_2024_merged.tif"),
  Air_temp_max = rast("data/climate/air_temp_max/air_temp_max_1991_2024_merged.tif")
)

# Reproject research areas to match the CRS (Coordinate Reference System) of the climate data
areas <- lapply(areas, function(area) {
  project(area, crs(climate_data$Precipitation))  # Reproject each research area to the CRS of Precipitation data
})

# Function to calculate monthly means for a specific area
# This function will extract climate data for each area and compute the monthly means
calc_monthly_means <- function(area, raster_data) {
  extracted <- terra::extract(raster_data, area, fun = mean, na.rm = TRUE)  # Extract mean for each area
  time_info <- time(raster_data)  # Get time information for the raster layers
  
  # Reshape the extracted data into a long format
  long_df <- extracted %>%
    pivot_longer(-ID, names_to = "Date", values_to = "Value") %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(ID, Date) %>%
    summarize(Mean = mean(Value, na.rm = TRUE), .groups = "drop")
  
  # Group by month and calculate the mean for each month
  extracted_data <- long_df %>%
    mutate(Year = format(Date, "%Y"),  # Extract Year from Date
           Month = format(Date, "%m")) %>%  # Extract Month from Date
    group_by(Month) %>%
    summarise(Mean = mean(Mean, na.rm = TRUE))  # Compute monthly mean
}

# Calculate and combine data for all variables and areas
# This will calculate the monthly mean for each climate variable across all research areas
long_term_means <- lapply(names(climate_data), function(var_name) {
  lapply(names(areas), function(area_name) {
    data <- calc_monthly_means(areas[[area_name]], climate_data[[var_name]])  # Calculate monthly mean
    data <- mutate(data, Variable = var_name, Area = area_name)  # Add variable and area names
    data
  }) %>% bind_rows()  # Combine results for all areas into one data frame
}) %>% bind_rows()  # Combine results for all variables into one data frame

# Modify the variable names for plotting with units and full names
long_term_means <- long_term_means %>%
  mutate(Variable = recode(Variable,
                           "Precipitation" = "Precipitation (mm)",
                           "Soil_Moisture" = "Plant Usable Water (%)",
                           "Evaporation" = "Potential Evapotranspiration (mm)",
                           "Air_temp_max" = "Maximum Air Temperature (K)"))

# Create a plot for the long-term monthly climate comparison
climate_plot <- ggplot(long_term_means, aes(x = Month, y = Mean, color = Area, group = Area)) +
  geom_line() +  # Plot lines for each area
  facet_wrap(~ Variable, scales = "free_y") +  # Facet by variable, with separate y-axis scales
  theme_minimal() +  # Apply minimal theme
  labs(title = "Long-term Monthly Climate Comparison (1991-2024)",
       x = "Month", y = "Mean Value") +  # Add labels
  scale_x_discrete(labels = month.abb)  # Use abbreviated month names

# Print the plot to the console
print(climate_plot)

# Save the plot as a PNG image
ggsave("figures/area_compare/long_term_climate_comparison.png", plot = climate_plot, 
       width = 11, height = 6, dpi = 300, bg = "white")

###################################################
# Function to extract mean values per AOI (Area of Interest) per time point
# This function extracts climate data from rasters for each AOI and reshapes the data into a long format
extract_climate_data <- function(climate_raster, aoi_list) {
  results <- list()  # Initialize an empty list to store results
  for (aoi_name in names(aoi_list)) {
    aoi <- aoi_list[[aoi_name]]  # Get the current AOI
    # Extract mean values over the AOI for each time point
    extracted <- terra::extract(climate_raster, aoi, fun = mean, na.rm = TRUE)
    extracted$AOI <- aoi_name  # Add AOI name to the extracted data
    # Reshape the extracted data to long format
    extracted_long <- extracted %>%
      pivot_longer(cols = -c(ID, AOI), names_to = "time", values_to = "value")  # Reshape data
    
    # Remove the 'ID' column if it exists
    extracted_long <- extracted_long %>%
      select(-ID)
    
    # Store the results for the current AOI
    results[[aoi_name]] <- extracted_long
  }
  # Combine the results for all AOIs
  combined_results <- bind_rows(results)
  return(combined_results)
}

# Extract data for all climate variables
climate_extracted <- list()
for (clim_name in names(climate_data)) {
  raster <- climate_data[[clim_name]]
  extracted_data <- extract_climate_data(raster, areas)  # Extract climate data
  extracted_data$Climate_Variable <- clim_name  # Add climate variable name
  climate_extracted[[clim_name]] <- extracted_data  # Store extracted data
}

# Combine all extracted data for climate variables
climate_all <- bind_rows(climate_extracted)

# Reshape data to wide format (for each AOI and climate variable)
climate_wide <- climate_all %>%
  pivot_wider(names_from = Climate_Variable, values_from = value)

# Ensure the 'time' column is of Date type for proper analysis
climate_wide$time <- as.Date(climate_wide$time, format = "%Y-%m-%d")

# Compute summary statistics (mean values over time)
summary_stats <- climate_wide %>%
  group_by(AOI) %>%
  summarise(
    Precipitation_mean = mean(Precipitation, na.rm = TRUE),
    Soil_Moisture_mean = mean(Soil_Moisture, na.rm = TRUE),
    Evaporation_mean = mean(Evaporation, na.rm = TRUE),
    Air_temp_max_mean = mean(Air_temp_max, na.rm = TRUE)
  )

# Set AOI as row names for easier interpretation
climate_means <- summary_stats %>%
  select(AOI, ends_with("_mean")) %>%
  tibble::column_to_rownames("AOI")

# **Normalization Step: Standardize the Climate Variables**
# Standardize the climate variables (columns) for comparison
climate_means_scaled <- scale(climate_means)

# Compute a distance matrix between AOIs using the standardized data
distance_matrix_scaled <- dist(climate_means_scaled)
print(as.matrix(distance_matrix_scaled))

# Optional: Update Clustering and PCA with Standardized Data

# Hierarchical Clustering of AOIs based on standardized climate variables
hc_scaled <- hclust(distance_matrix_scaled)
plot(hc_scaled, main = "Hierarchical Clustering of AOIs Based on Standardized Climate Variables")

# Principal Component Analysis (PCA)
# Ensure you have the 'ggfortify' package installed for autoplot
if (!require(ggfortify)) {
  install.packages("ggfortify")
  library(ggfortify)
}

# Perform PCA on the scaled climate data
pca_result_scaled <- prcomp(climate_means_scaled)
pca_plot <- autoplot(pca_result_scaled, data = summary_stats, colour = 'AOI',
                     main = "PCA of AOIs Based on Standardized Climate Variables (1991-2024)")

# Save the PCA plot as a PNG image
ggsave("figures/area_compare/long_term_climate_comparison_pca.png", plot = pca_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")
