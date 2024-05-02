## Running projections

#set path for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

###### Source scripts ######
# load in required libraries
library(raster)
library(dplyr)
library(sf)
library(roads)
library(terra)
library(tmap)
library(purrr)
library(caribouMetrics)
library(pfocal)

# load functions used in script
devtools::load_all()

# set rasterOptions to allow higher memory usage
#prevOpts <- terraOptions(memfrac = 0.9)

#densities to test
low <- 0.000001
high <- 0.00001

# For Revelstoke at fine resolution

# First run prepInputs to create filtered inputs for QGIS plugin using the
# original cost surface
prepInputs(
  cutblocksPth = paste0(data_path_drvd, "cutblocks_revelstoke_real.gpkg"),
  roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
  tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
  costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif"),
  outPth = paste0(data_path_drvd, "TSA27_real_cuts/"),
  aggFact = 1, #factor of aggregation of cost surface. 1 = no aggregation.
  saveInputs = TRUE,
  replaceNA = -65000
)

# Logs copied from QIS for running the plugin and unioning the results to the input roads
# QGIS version: 3.12.0-București
# QGIS code revision: cd141490ec
# Qt version: 5.11.2
# GDAL version: 3.0.4
# GEOS version: 3.8.0-CAPI-1.13.1
# PROJ version: Rel. 6.3.1, February 10th, 2020
# Processing algorithm…
# Algorithm 'Forest Road Network Creation' starting…
# Input parameters:
#   { 'ANGLES_CONSIDERED' : 1, 'HEURISTIC_IN_POLYGONS' : '',
#     'INPUT_COST_RASTER' : 'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27_real_cuts/input_cost.tif',
#     'INPUT_POLYGONS_TO_ACCESS' : 'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27_real_cuts/input_cutblocks.gpkg|layername=input_cutblocks',
#     'INPUT_RASTER_BAND' : 1,
#     'INPUT_ROADS_TO_CONNECT_TO' : 'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27_real_cuts/input_roads.gpkg|layername=input_roads',
#     'METHOD_OF_GENERATION' : 1, 'OUTPUT' : 'TEMPORARY_OUTPUT', 'PUNISHER_135DEGREES' : 5,
#     'PUNISHER_45DEGREES' : 1.25, 'PUNISHER_90DEGREES' : 2, 'SKIDDING_DISTANCE' : 200 }
#
#
# Algorithm 'Union' starting…
# Input parameters:
#   { 'INPUT' : 'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27_real_cuts/klementProjection.shp',
#     'OUTPUT' : 'TEMPORARY_OUTPUT',
#     'OVERLAY' : 'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27_real_cuts/input_roads.gpkg|layername=input_roads',
#     'OVERLAY_FIELDS_PREFIX' : '' }





