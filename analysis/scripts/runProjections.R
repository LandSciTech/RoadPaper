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

# First run prepInputs to create filtered inputs for QGIS plugin
prepInputs(
  cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg"),
  roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
  tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
  costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif"),
  outPth = paste0(data_path_drvd, "TSA27/"),
  aggFact = 1, #factor of aggregation of cost surface. 1 = no aggregation.
  saveInputs = TRUE
)

# Code copied from QIS for running the plugin
# processing.run("FRNC:Forest Road Network Creation",
# {'INPUT_COST_RASTER':'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27/input_cost.tif',
# 'INPUT_RASTER_BAND':1,
# 'INPUT_POLYGONS_TO_ACCESS':'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27/input_cutblocks.gpkg',
# 'INPUT_ROADS_TO_CONNECT_TO':'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27/input_roads.gpkg',
# 'SKIDDING_DISTANCE':100,'METHOD_OF_GENERATION':1,'HEURISTIC_IN_POLYGONS':'',
# 'ANGLES_CONSIDERED':1,'PUNISHER_45DEGREES':1.25,'PUNISHER_90DEGREES':2,
# 'PUNISHER_135DEGREES':5,
# 'OUTPUT':'C:/Users/endicotts/Documents/gitprojects/RoadPaper/analysis/data/derived_data/TSA27/klementProjection.shp'})



#
run_projections(
  cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg"),
  roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
  tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
  costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif"),
  outPth = paste0(data_path_drvd, "TSA27/"),

  #Klement QGIS projection results shapefile
  klementProj = paste0(data_path_drvd, "TSA27/", "klementProjection.shp"),

  #set high and low sampling densities for projections
  low  = low,
  high = high,

  aggFact = 1, #factor of aggregation of cost surface. 1 = no aggregation.
  saveInputs = TRUE
)

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
