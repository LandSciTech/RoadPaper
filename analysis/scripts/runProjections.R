## Running projections on full TSA using subsets (to do high resolution)

#set working directory
#setwd("C:/Users/LapinsD/Desktop/Dlapins_rstudio/RoadAnalysis")

#set path for data
data_path <- "data/"

###### Source scripts ######
# load in required libraries
## Library source for TSA road projection metric script

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

# load functions used in script - source script
source("davidfolder/functionsSource.R")

######### load in data for projections ########################################

#forest harvest cutblocks
cutblocks <- st_make_valid(st_read(paste0(data_path, "revelstoke/cutblocks_revelstoke.shp")))
#modern observed roads
roads <- st_read(paste0(data_path, "revelstoke/roads_revelstoke.shp"))
#boundary for running projection
tsaBoundary <- st_read(paste0(data_path, "revelstoke/new_tsa27_boundaries.shp"))
#cost surface raster layer
bc_cost_surface <- raster::raster(paste0(data_path, "cost_surface_bc_ha.tif"))
#subsets of TSA (in order to run high resolution across large TSA)
tsbs <- map(list.files("data/revelstoke/subs/", pattern = ".shp",
                       full.names = TRUE), st_read)
#tsbs <- (tsbs[1:3]) #for testing on smaller area
#Klement QGIS projection results shapefile
klementProj <- st_read(paste0(data_path, "revelstoke/klementProjection.shp"))

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

tsaCost_st <- stars::st_as_stars(tsaCost)
tmplt <- stars::st_as_stars(st_bbox(tsaCost_st), nx = raster::ncol(tsaCost),
                            ny = raster::nrow(tsaCost), values = 1)
roadsExist_st <- stars::st_rasterize(roadsExist %>% dplyr::select(-AWARD_DATE), template = tmplt,
                                     options = "ALL_TOUCHED=TRUE") == 1
tsaCost_st <- tsaCost_st * roadsExist_st
tsaCost_st <- as(tsaCost_st, "Raster")
roadsExist <- roadsExist %>%  st_transform(st_crs(tsaCost_st))

# setting lake values high because they are blocking paths if NA - not necessary for all landscapes
tsaCost_st[is.na(tsaCost_st[])] <- lakeValue

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
