# R Script for Downloading and Processing Climate Data from DWD

# Description:
# This script automates the download, extraction, and processing of monthly climate data from the German Weather Service (DWD) Open Data portal. It:
# - Downloads monthly data for specified climate variables (e.g., precipitation, evaporation, air temperature max) over a defined period.
# - Extracts compressed files and removes the original archives to save space.
# - Processes the extracted data into raster stacks with appropriate time stamps and coordinate reference systems.
# - Saves the merged raster data for further analysis.

# Workflow Overview:
# 1. **Library Loading**:
#    - Imports necessary libraries (`dplyr`, `R.utils`, `terra`) for data manipulation and processing.

# 2. **Parameter Definition**:
#    - Defines the years and months for which data will be downloaded.
#    - Specifies the climate variables to be processed along with their corresponding URLs and file naming patterns.

# 3. **Data Download and Extraction**:
#    - Creates a generalized function to download data for each variable.
#    - Loops through the variables, years, and months to download all relevant files.
#    - Extracts the downloaded `.gz` files and removes the archives.

# 4. **Data Processing**:
#    - For each climate variable:
#      - Lists the extracted `.asc` files.
#      - Loads them into a raster stack.
#      - Assigns appropriate dates to each raster layer based on the filenames.
#      - Sets the coordinate reference system to EPSG:31467.
#      - Saves the merged raster stack to a `.tif` file.

# Notes:
# - Adjust the `variables` list to include or exclude climate variables as needed.
# - Make sure the `data` directory structure exists or adjust the paths accordingly.
# - use_month_names checks if the file of the climate variables are within months folder or not

# Load necessary libraries
library(dplyr)
library(R.utils) # For gunzip function
library(terra)   # For raster processing

#############################Adjust here#############################
setwd("C:/Users/konst/Documents/") # Set Your Project Route Directory here

# Define the range of years for which climate data will be downloaded.
years <- 1991:2024

# Create a vector of month numbers formatted as two-digit strings ("01", "02", …, "12").
months <- sprintf("%02d", 1:12)

# Create a vector of month names with a number prefix, used to form part of the URL path for some datasets.
month_names <- c("01_Jan", "02_Feb", "03_Mar", "04_Apr", "05_May", "06_Jun",
                 "07_Jul", "08_Aug", "09_Sep", "10_Oct", "11_Nov", "12_Dec")

# Define a list of variables that will be downloaded and processed.
# Each element in the list is itself a list that contains:
#   - name: the type of climate data (e.g., precipitation, evapo_p)
#   - base_url: the base URL where the data is hosted
#   - dest_folder: the local folder where the data will be saved
#   - filename_pattern: a pattern to create the correct file name for each month and year
#   - use_month_names: a flag indicating whether the month name folder should be included in the URL
variables <- list(
  list(
    name = "precipitation",
    base_url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/precipitation",
    dest_folder = "data/climate/precipitation",
    filename_pattern = "grids_germany_monthly_precipitation_%04d%02d.asc.gz",
    use_month_names = TRUE
  ),
  list(
    name = "evapo_p",
    base_url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/evapo_p",
    dest_folder = "data/climate/evapo_p",
    filename_pattern = "grids_germany_monthly_evapo_p_%04d%02d.asc.gz",
    use_month_names = FALSE
  ),
  list(
    name = "air_temp_max",
    base_url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/air_temperature_max",
    dest_folder = "data/climate/air_temp_max",
    filename_pattern = "grids_germany_monthly_air_temp_max_%04d%02d.asc.gz",
    use_month_names = TRUE
  ),
  list(
    name = "soil_moist",
    base_url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/soil_moist",
    dest_folder = "data/climate/soil_moist",
    filename_pattern = "grids_germany_monthly_soil_moist_%04d%02d.asc.gz",
    use_month_names = FALSE
  )
)

##################################################################
# Check if the main data directory exists; if not, create it.
# This ensures that the "data/climate" directory is available for storing all files.
if (!dir.exists(file.path(getwd(), "data/climate"))) {
  dir.create(file.path(getwd(), "data/climate"), recursive = TRUE)
  message(sprintf("Created data directory: %s", file.path(getwd(), "data/climate")))
}

# Function to download climate data files.
# It accepts a variable configuration along with years, months, and month names.
download_climate_data <- function(variable, years, months, month_names) {
  # Extract details about the current climate variable.
  name <- variable$name
  base_url <- variable$base_url
  dest_folder <- variable$dest_folder
  filename_pattern <- variable$filename_pattern
  use_month_names <- variable$use_month_names
  
  # Make sure that the destination folder exists; if not, create it.
  if (!dir.exists(dest_folder)) {
    dir.create(dest_folder, recursive = TRUE)
  }
  
  # Loop over each year.
  for (year in years) {
    # Loop over each month using the index to access both month number and month name.
    for (i in seq_along(months)) {
      month <- months[i]       # Two-digit month number (e.g., "01")
      month_name <- month_names[i]  # Corresponding month name (e.g., "01_Jan")
      
      # Construct the file name using the provided pattern.
      # The pattern inserts the year and the month (as a number) into the file name.
      file_name <- sprintf(filename_pattern, year, as.numeric(month))
      
      # Depending on the variable configuration, include the month name in the URL or not.
      if (use_month_names) {
        # URL structure when month names are used.
        url <- sprintf("%s/%s/%s", base_url, month_name, file_name)
      } else {
        # URL structure when month names are not used.
        url <- sprintf("%s/%s", base_url, file_name)
      }
      
      # Define the destination file path where the downloaded file will be saved.
      destfile <- file.path(dest_folder, file_name)
      
      # Check if the file already exists locally to avoid re-downloading.
      if (!file.exists(destfile)) {
        tryCatch({
          # Download the file in binary mode.
          download.file(url, destfile, mode = "wb")
          message(sprintf("Downloaded: %s", destfile))
        }, error = function(e) {
          # If there's an error (e.g., file not found), display a warning message.
          warning(sprintf("Failed to download %s: %s", url, e$message))
        })
      } else {
        message(sprintf("File already exists: %s", destfile))
      }
    }
  }
}

# Function to extract downloaded .gz (gzip compressed) files.
# After downloading, the data files are compressed and need to be extracted.
extract_climate_data <- function(variable) {
  dest_folder <- variable$dest_folder
  # Find all files in the destination folder with a .gz extension.
  gz_files <- list.files(dest_folder, pattern = "\\.gz$", full.names = TRUE)
  
  # Loop through each compressed file.
  for (file in gz_files) {
    # Use the gunzip function from the R.utils package to extract the file.
    # The original .gz file is removed after extraction.
    R.utils::gunzip(file, overwrite = TRUE, remove = TRUE)
    message(sprintf("Extracted and removed: %s", file))
  }
}

# Function to process the extracted climate data files.
# This involves reading the data files into a raster stack, assigning dates, setting the coordinate system, and saving the merged output.
process_climate_data <- function(variable) {
  dest_folder <- variable$dest_folder
  # List all .asc files (ASCII grid files) in the destination folder.
  asc_files <- list.files(dest_folder, pattern = "\\.asc$", full.names = TRUE)
  
  # If no .asc files are found, output a message and exit the function.
  if (length(asc_files) == 0) {
    message(sprintf("No .asc files found in %s", dest_folder))
    return(NULL)
  }
  
  # Load the .asc files as a raster stack (a collection of raster layers).
  raster_stack <- rast(asc_files)
  
  # Extract date information from the filenames.
  # Remove the common prefix and the file extension to isolate the date part.
  file_dates <- basename(asc_files)
  file_dates <- sub(paste0("grids_germany_monthly_", variable$name, "_"), "", file_dates)
  file_dates <- sub("\\.asc$", "", file_dates)
  
  # Convert the extracted date strings into proper Date objects.
  # The date is constructed by taking the year and month from the file name and setting the day to "01".
  dates <- as.Date(paste0(substr(file_dates, 1, 4), "-", substr(file_dates, 5, 6), "-01"))
  
  # Assign the extracted dates to the raster stack as both a time attribute and layer names.
  time(raster_stack) <- dates
  names(raster_stack) <- dates
  
  # Set the Coordinate Reference System (CRS) for the raster stack.
  crs(raster_stack) <- "EPSG:31467"
  
  # Define the output file name for the merged raster stack.
  # The output file includes the variable name and the range of years.
  output_file <- file.path(dest_folder, paste0(variable$name, "_", min(years), "_", max(years), "_merged.tif"))
  
  # Save the raster stack to a GeoTIFF file, overwriting any existing file with the same name.
  writeRaster(raster_stack, output_file, overwrite = TRUE)
  
  message(sprintf("Processed and saved raster stack to: %s", output_file))
}

# Loop over each climate variable defined in the 'variables' list.
# For each variable, the code downloads the data, extracts the files, and processes them.
for (variable in variables) {
  message(sprintf("Processing variable: %s", variable$name))
  download_climate_data(variable, years, months, month_names)  # Download data files
  extract_climate_data(variable)  # Extract the downloaded compressed files
  process_climate_data(variable)  # Process and merge the extracted data into a raster stack
}


