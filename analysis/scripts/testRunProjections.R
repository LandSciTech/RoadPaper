## Running projections on test area

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

#densities to test
low <-  0.000001
high <- 0.00001


run_projections(
  cutblocksPth = paste0(data_path_drvd, "testing_cutblocks.gpkg"),
  roadsPth = paste0(data_path_drvd, "testing_ex_roads.gpkg"),
  tsaBoundaryPth = paste0(data_path_drvd, "testing_tsb.gpkg"),
  costPth = paste0(data_path_drvd, "testing_cost.tif"),
  outPth = paste0(data_path_drvd, "test_tsb/"),

  #Klement QGIS projection results shapefile
  klementProj = NULL,

  #set high and low sampling densities for projections
  low  = low,
  high = high,

  aggFact = 1, #factor of aggregation of cost surface. 1 = no aggregation.
  saveInputs = FALSE
)
