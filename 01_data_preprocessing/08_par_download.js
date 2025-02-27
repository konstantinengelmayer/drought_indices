// Load the MODIS PAR dataset
var dataset = ee.ImageCollection('MODIS/061/MCD18C2')
                  .filter(ee.Filter.date('2018-01-01', '2024-12-31')); // Define the temporal range

// Select the 'GMT_1200_PAR' band
var gmt_1200_par = dataset.select('GMT_1200_PAR');

// Load the administrative boundary of Germany
var germany = ee.FeatureCollection('FAO/GAUL/2015/level0')
                .filter(ee.Filter.eq('ADM0_NAME', 'Germany')); // Filter for Germany

// Define a function to calculate monthly aggregates
var monthlyAggregation = ee.ImageCollection(
  ee.List.sequence(0, 83).map(function (monthIndex) {
    var monthStart = ee.Date('2018-01-01').advance(monthIndex, 'month');
    var monthEnd = monthStart.advance(1, 'month');
    var monthlyImages = gmt_1200_par.filterDate(monthStart, monthEnd);
    
    return monthlyImages.mean() // Calculate the monthly mean
                           .set('system:time_start', monthStart.millis()) // Set time property
                           .clip(germany); // Clip to Germany
  })
);

// Visualization parameters (optional)
var gmt_1200_par_vis = {
  min: -236,
  max: 316,
  palette: ['0f17ff', 'b11406', 'f1ff23']
};

// Add the first monthly image as a layer to verify
Map.centerObject(germany, 6); // Center the map over Germany
Map.addLayer(monthlyAggregation.first(), gmt_1200_par_vis, 'First Monthly Aggregated PAR');

// Export the monthly aggregated data as an image collection
Export.image.toDrive({
  image: monthlyAggregation.toBands(), // Convert collection to a multiband image
  description: 'MODIS_PAR_Monthly_Aggregated_2018_2024',
  folder: 'EarthEngineData',
  scale: 1000, // Specify the spatial resolution in meters
  region: germany.geometry(), // Limit export to Germany's boundary
  maxPixels: 1e13 // Set the maximum pixel allowance for large datasets
});