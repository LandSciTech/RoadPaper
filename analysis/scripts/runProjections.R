## Running projections on full TSA using subsets (to do high resolution)

#set path for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

###### Source scripts ######
# load in required libraries
library(dplyr)
library(sf)
library(roads)
library(stars)
library(raster)
library(tmap)
library(rgdal)
library(spex)
library(purrr)
library(starsExtra)
library(ggplot2)
library(gridExtra)
library(caribouMetrics)
library(raster)
library(forcats)
library(rgeos)
library(data.table)
library(pfocal)

# load functions used in script
devtools::load_all()

# set rasterOptions to allow higher memory usage
prevOpts <- rasterOptions(chunksize = 1e+09, maxmemory = 8e+09, memfrac = 0.9)

######### load in data for projections ########################################

#forest harvest cutblocks
cutblocks <- st_make_valid(st_read(paste0(data_path_raw, "cutblocks_revelstoke.shp")))
#modern observed roads
roads <- st_read(paste0(data_path_raw, "roads_revelstoke.shp"))
#boundary for running projection
tsaBoundary <- st_read(paste0(data_path_raw, "new_tsa27_boundaries.shp"))
#cost surface raster layer
bc_cost_surface <- raster::raster(paste0(data_path_raw, "cost_surface_bc_ha.tif"))
#subsets of TSA (in order to run high resolution across large TSA)
tsbs <- map(list.files(paste0(data_path_raw, "subs/"), pattern = ".shp",
                       full.names = TRUE), st_read)
tsbs <- (tsbs[1:3]) #for testing on smaller area
#Klement QGIS projection results shapefile
klementProj <- st_read(paste0(data_path_drvd,
                              "klementProjection.shp"))

###### Parameters #############################################################

# set years for projection start (the projection is set to go from 1990 onwards)
roadsYear <- 19900000
# set cutblock year
cutblocksYear <- 1990
#set high and low sampling densities for projections
low <- 0.000001
high <- 0.0001
#if lakes block path then set high value for lakes (NA) on cost surface.
lakeValue <- 65000 #often this isn't needed but Revelstoke TSA requires this.
aggFact <- 1 #factor of aggregation of cost surface. 1 = no aggregation.

###############################################################################

# parameter table creation for running projections
sampleDens <- c(low,high,low,high,low)
sampleType <- c("regular","regular","random","random","centroid")
paramTable <- tibble(sampleType, sampleDens, output = vector("list", length(sampleDens)),
                     roadDisturbance = vector("list", length(sampleDens)),
                     roadDensity = vector("list", length(sampleDens)),
                     roadPresence = vector("list", length(sampleDens)),
                     distanceToRoad = vector("list", length(sampleDens)),
                     forestryDisturbance = vector("list", length(sampleDens)))

#filter roads by year to make existing forestry road network
roads[is.na(roads)] <- roadsYear
roadsExist <- filter(roads, AWARD_DATE <= roadsYear)

cutblocksPrior <- filter(cutblocks, HARVESTYR <= cutblocksYear)
cutblocks <- filter(cutblocks, HARVESTYR > cutblocksYear)

### prepare cost surface layer
tsaCost <- crop(bc_cost_surface, tsaBoundary)

tsaCost <- raster::aggregate(tsaCost, fact = aggFact, fun = raster::mean)

# burn roads into cost raster
roadsExist_rast <- terra::rasterize(terra::vect(roadsExist), terra::rast(tsaCost),
                                    background = 0) == 0

tsaCost_st <- terra::rast(tsaCost) * roadsExist_rast
roadsExist <- roadsExist %>%  st_transform(st_crs(tsaCost_st))

# setting lake values high because they are blocking paths if NA - not necessary for all landscapes
tsaCost_st[is.na(tsaCost_st)] <- lakeValue

#Running projections and creating raster layers of the various metrics
allResults <- projectAll(tsbs = tsbs, paramTable = paramTable,
                                  costSurface = tsaCost_st,
                                  boundary = tsaBoundary,
                                  cutblocks = cutblocks,
                                  nonAggregatedCostSurface = bc_cost_surface,
                                  existingRoads = roadsExist,
                                  observedRoads = roads)
allResults #results of projected road shapefiles and metric rasters (including observed)

#Retrieving mean values for cutover and overall
meanTable <- getMetricMeans(allResults, cutblocks)

meanTable$sampleDens <-c("low sample density","high sample density",
                         "low sample density","high sample density","centroid",
                         "observed", "klementQGIS", "low sample density",
                         "high sample density","low sample density",
                         "high sample density","centroid", "observed", "klementQGIS")


meanTable #resulting table with all mean values from the metrics (overall & cutover)

# set rasterOptions back to previous value
rasterOptions(chunksize = prevOpts$chunksize, maxmemory = prevOpts$maxmemory,
              memfrac = prevOpts$memfrac)
