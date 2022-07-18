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
prevOpts <- terraOptions(memfrac = 0.9)

######### load in data for projections ########################################

#forest harvest cutblocks
cutblocks <- st_make_valid(st_read(paste0(data_path_raw, "cutblocks_revelstoke.shp")))
#modern observed roads
roads <- st_read(paste0(data_path_raw, "roads_revelstoke.shp"))
#boundary for running projection
tsaBoundary <- st_read(paste0(data_path_raw, "new_tsa27_boundaries.shp"))
#cost surface raster layer
bc_cost_surface <- terra::rast(paste0(data_path_raw, "cost_surface_bc_ha.tif"))


#Use subsets of TSA (in order to avoid memory issues)?
use_sub <- FALSE

if(use_sub){
  tsbs <- map(list.files(paste0(data_path_raw, "subs/"), pattern = ".shp",
                         full.names = TRUE), st_read)
  # tsbs <- tsbs[1:3] #for testing on smaller area
} else {
  tsbs <- list(tsaBoundary)
}

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

if(aggFact > 1){
  tsaCost <- raster::aggregate(tsaCost, fact = aggFact, fun = raster::mean)
}

# burn roads into cost raster
roadsExist_rast <- terra::rasterize(terra::vect(roadsExist), terra::rast(tsaCost),
                                    background = 0) == 0

tsaCost_st <- tsaCost * roadsExist_rast
roadsExist <- roadsExist %>%  st_transform(st_crs(tsaCost_st))

# setting lake values high because they are blocking paths if NA - not necessary for all landscapes
tsaCost_st <- terra::subst(tsaCost_st, from = NA, to = lakeValue)

# This doesn't work because some areas dont have any existing roads
# # Break the area into smaller parts to process separately to avoid memory problems
# grid <- st_make_grid(tsaBoundary, n = 5, offset = c(1472229, 655853.0))
#
# tsa_parts <- st_union(tsaBoundary) %>% st_intersection(grid) %>% st_as_sf() %>%
#   st_make_valid() %>%
#   mutate(ID = 1:n()) %>%
#   split(factor(1:nrow(.)))

#Running projections
allResults <- projectAll(tsbs = tsbs, paramTable = paramTable,
                         costSurface = tsaCost_st,
                         cutblocks = cutblocks,
                         existingRoads = roadsExist,
                         fileLocation = paste0(data_path_drvd, "TSA27"))

# recreate allResults after a restart using saved files
# allResults <- paramTable[c(1,3,5),] %>%
#   mutate(output = paste0(data_path_drvd, "TSA27", "_", sampleType, "_",
#                          sampleDens, ".shp"))

# Using David's saved results
allResults <- paramTable %>%
  mutate(output = paste0(data_path_drvd, "combinedTSBRoads", "_",
                         dplyr::case_when(sampleType == "centroid" ~ "C",
                                   sampleType == "random" & sampleDens == 1e-04 ~ "RA2",
                                   sampleType == "random" & sampleDens == 1e-06 ~ "RA1",
                                   sampleType == "regular" & sampleDens == 1e-04 ~ "RE2",
                                   sampleType == "regular" & sampleDens == 1e-06 ~ "RE1"),
                         ".shp"))

# creating raster layers of the various metrics
allMetrics <- calcMetrics(paramTable = allResults,
                          boundary = tsaBoundary,
                          nonAggregatedCostSurface = bc_cost_surface,
                          observedRoads = paste0(data_path_raw, "roads_revelstoke.shp"),
                          klementProj = paste0(data_path_drvd,
                                               "klementProjection.shp"),
                          cutblocks = cutblocks,
                          costSurface = tsaCost_st)

#Retrieving mean values for cutover and overall
meanTable <- getMetricMeans(allMetrics, cutblocks)

meanTable <-  mutate(meanTable,
                     sampleDens = case_when(
                       sampleType == "centroid" ~ "centroid",
                       sampleType == "observed" ~ "observed",
                       sampleType == "klementQGIS" ~ "klementQGIS",
                       sampleDens == low ~ "low sample density",
                       sampleDens == high ~"high sample density"))


meanTable #resulting table with all mean values from the metrics (overall & cutover)

meanTable <- mutate(meanTable, across(where(is.list), unlist))

write.csv(meanTable, paste0(data_path_drvd, "mean_table.csv"), row.names = FALSE)

# compare spatially explicit agreement
agreeTable <- agreeMetricsAll(allMetrics, prex_rast = roadsExist_rast == 0,
                              prex_vect = roadsExist, boundary = tsaBoundary,
                              cutblocks = cutblocksPrior)

write.csv(agreeTable, paste0(data_path_drvd, "agree_table.csv"), row.names = FALSE)

beepr::beep()
# set rasterOptions back to previous value
terraOptions(memfrac = prevOpts$memfrac)
print(Sys.time())
