## Running projections on full TSA using subsets (to do high resolution)

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

# # For Revelstoke at fine resolution
# run_projections(
#   cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg"),
#   roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
#   tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
#   costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif"),
#   outPth = paste0(data_path_drvd, "TSA27/"),
#
#   #Klement QGIS projection results shapefile
#   klementProj = paste0(data_path_drvd, "TSA27/", "klementProjection.shp"),
#
#   #set high and low sampling densities for projections
#   low  = low,
#   high = high,
#
#   aggFact = 1 #factor of aggregation of cost surface. 1 = no aggregation.
# )

# For Revelstoke at coarse resolution
run_projections(
  cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg"),
  roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
  tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
  costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif"),
  outPth = paste0(data_path_drvd, "TSA27_1000/"),

  #Klement QGIS projection results shapefile
  klementProj = NULL,

  #set high and low sampling densities for projections
  low  = low,
  high = high,

  aggFact = 10 #factor of aggregation of cost surface. 1 = no aggregation.
)

# For Fort Nelson at coarse resolution
# Didn't work bc award date not in DRA. Need forest roads
run_projections(
  cutblocksPth = paste0(data_path_raw, "cutblocks_ft_nelson.gpkg"),
  roadsPth = paste0(data_path_drvd, "combined_ft_nelson_roads.gpkg"),
  tsaBoundaryPth = paste0(data_path_raw, "tsa8_boundaries.gpkg"),
  costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif"),
  outPth = paste0(data_path_drvd, "TSA8_1000/"),

  #Klement QGIS projection results shapefile
  klementProj = NULL,

  #set high and low sampling densities for projections
  low  = high,
  high = high,

  aggFact = 10 #factor of aggregation of cost surface. 1 = no aggregation.
)
beepr::beep()

# set rasterOptions back to previous value
# terraOptions(memfrac = prevOpts$memfrac)
print(Sys.time())
