# TSA 8 - Fort Nelson   ##### FOR FLOW CHART

# set data source to proper location

data_path <- "data/"

# library load 

source("davidfolder/roadsLibrarySource.R")

# Enter the 3 required TSA variables 


cutblocks <- st_read(paste0(data_path, "fort_nelson/tsa8_cutblocks.shp"))

roads <- st_read(paste0(data_path, "fort_nelson/tsa8_roads.shp"))

tsaBoundary <- st_read(paste0(data_path, "fort_nelson/tsa8_boundaries.shp"))


# Prepare data - Choose years for start of projections

source("davidfolder/dataPreparation.R")

############## FLOW CHART
tsaCost_st <-  raster::raster(paste0(data_path, "fort_nelson/projections_gis/tsaCost_st_FortNelson.tif")) 
cutblocks <- st_read(paste0(data_path, "fort_nelson/post1990_cutblocksTSA8.shp"))
############################### FLOW CHART ^

# Load in functions used

source("davidfolder/functionsSource.R")


# Run projections and metrics - Output tables and graph

source("davidfolder/runProjections.R")


# Create difference maps and find percent matching

source("davidfolder/differenceMaps.R")

source("davidfolder/quantifying_change_projVSobs.R")



###################################################################################

mst_roadsTSA16 <- st_as_sf(lcp_roads$roads)
st_write(mst_roadsTSA16, "data/fort_nelson/projections_gis/lcp_results.shp") 

writeRaster(centroid_setMean_merge, "data/fort_nelson/projections_gis/setDensity_centroid.tif")


lcp_roads <- roads::projectRoads(landings = cutblocks,
                                 cost = tsaCost_st,
                                 roads = roadsExist,
                                 roadMethod = "lcp")

setDense_mask <- st_read(paste0(data_path, "fort_nelson/flow_chart_layers/setDens_mask.shp"))

setDensity_flow <- raster::raster(paste0(data_path, "fort_nelson/projections_gis/masked_setDens.tif"))



#### trying compare classification function in greenbrown package

install.packages("greenbrown", repos="http://R-Forge.R-project.org")

install.packages("strucchange")

install.packages("Kendall")

install.packages("bfast")

install.packages("phenopix")

install.packages("quantreg")

library("greenbrown")

agreementMap <- greenbrown::CompareClassification(disturbance_centroid@processedData$anthroBuff
                                                  , disturbance_observed@processedData$anthroBuff)


