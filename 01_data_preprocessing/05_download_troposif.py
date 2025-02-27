# Python Script for Downloading, Cropping, and Merging SIF Data from API

# Description:
# This script downloads monthly SIF (Solar-Induced Fluorescence) data from an API, 
# processes the data by cropping it to a specified geographic region (Germany in this case), 
# and then merges the cropped data into a single NetCDF file for further analysis.
#
# Workflow:
# 1. Define the output directory where the NetCDF files will be saved.
# 2. Specify the start and end years for the data retrieval (2018–2024 in this case).
# 3. Loop through each year and fetch the SIF data for each month.
# 4. For each month's data:
#    - Download the raw .nc file.
#    - Open the file using `xarray`.
#    - Extract and compute the time in days since a reference date (2000-01-01).
#    - Crop the data to a specified geographic region (Germany's bounding box).
#    - Save the cropped data to a new NetCDF file.
# 5. After all files are downloaded and cropped, combine them into a single dataset.
# 6. Save the combined dataset to a new NetCDF file.
#
# Data Requirements:
# - Access to the API providing the Solar-Induced Fluorescence (SIF) data in NetCDF format.
# - Geographic boundaries for cropping: Germany (latitude 47–55, longitude 5.5–15).
#
# Outputs:
# - Cropped NetCDF files for each month within the given year range.
# - A single merged NetCDF file containing all the cropped data.

import os
import requests
import xarray as xr
from datetime import datetime

# Directory to save and access the NetCDF files
output_directory = "C:/Users/konst/Documents/data/sif/troposif"
os.makedirs(output_directory, exist_ok=True)

# Static base API URL template, with only the year changing in the loop
base_api_url_template = "https://data-portal.s5p-pal.com/api/s5p-l3/sif/month/{year}"

# User input for the start and end years
start_year = 2018
end_year = 2024

# Download loop: Each year within the range
for year in range(start_year, end_year + 1):
    # Construct the URL for each year, keeping "month" static
    base_api_url = base_api_url_template.format(year=year)

    # Fetch the main JSON listing for the specific year/month endpoint
    main_data = requests.get(base_api_url).json()

    # Loop over each item link in the main JSON data and download the .nc files
    for item in main_data["links"]:
        if item["rel"] == "item" and item["type"] == "application/geo+json":
            item_data = requests.get(item["href"]).json()
            asset = item_data["assets"]["product"]
            nc_url = asset["href"]
            nc_filename = os.path.join(output_directory, asset["file:local_path"])
            
            # Download the NetCDF file and save it to disk
            with open(nc_filename, "wb") as nc_file:
                nc_file.write(requests.get(nc_url).content)
            print(f"Downloaded: {nc_filename}")

            # Open the downloaded NetCDF file
            ds = xr.open_dataset(nc_filename)

            # Extract the date from the filename (expected format: 'month-YYYYMM')
            filename = os.path.basename(nc_filename)
            date_str = filename.split("month-")[1][:6]  # Extracts "202401" from the filename
            date = datetime.strptime(date_str, "%Y%m")

            # Compute time in days since the reference date (2000-01-01)
            ref_date = datetime(2000, 1, 1)
            time_in_days = (date - ref_date).days

            # Ensure the 'time' dimension exists and assign the computed time coordinate
            if 'time' not in ds.dims:
                ds = ds.expand_dims('time')
            ds = ds.assign_coords(time=[time_in_days])

            # Coordinate names are assumed to be 'latitude' and 'longitude'
            lat_name = 'latitude'
            lon_name = 'longitude'

            # Define the bounding box for Germany's latitude and longitude
            min_lat, max_lat = 47, 55
            min_lon, max_lon = 5.5, 15

            # Determine if latitude and longitude coordinates are increasing or decreasing
            lat_ascending = ds[lat_name][0] < ds[lat_name][-1]
            lon_ascending = ds[lon_name][0] < ds[lon_name][-1]

            # Create slices to extract the relevant region for Germany
            lat_slice = slice(min_lat, max_lat) if lat_ascending else slice(max_lat, min_lat)
            lon_slice = slice(min_lon, max_lon) if lon_ascending else slice(max_lon, min_lon)

            # Crop the dataset to the bounding box for Germany
            ds_cropped = ds.sel({lat_name: lat_slice, lon_name: lon_slice})

            # Create a new filename with '_crop' inserted before '.nc'
            base_filename, ext = os.path.splitext(nc_filename)
            cropped_nc_filename = f"{base_filename}_crop{ext}"

            # Save the cropped dataset to the new filename
            ds_cropped.to_netcdf(cropped_nc_filename)
            print(f"Cropped and saved: {cropped_nc_filename}")

# After downloading and cropping, merge all '_crop.nc' files in the output directory
nc_files = [os.path.join(output_directory, f) for f in os.listdir(output_directory) if f.endswith("_crop.nc")]
datasets = [xr.open_dataset(nc) for nc in nc_files]

# Extract the correct year and month from filenames for the time coordinates
time_coords = []
for nc_file in nc_files:
    filename = os.path.basename(nc_file)
    date_str = filename.split("month-")[1][:6]  # Extracts "201908" from the filename
    date = datetime.strptime(date_str, "%Y%m")
    time_coords.append(date)

# Combine all datasets along a new time dimension with extracted time coordinates
combined = xr.concat(datasets, dim="time")

# Save the combined dataset to a new NetCDF file
combined_output_path = os.path.join(output_directory, "combined_data.nc")
combined.to_netcdf(combined_output_path)

print(f"Merged file saved to: {combined_output_path}")
