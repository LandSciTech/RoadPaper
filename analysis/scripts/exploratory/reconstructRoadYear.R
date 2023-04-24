# Reconstruct road building year from cublock harvest date by iterating over
# cutblocks for each year

#set path for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

###### Source scripts ######
# load in required libraries
library(dplyr)
library(sf)
library(roads)
library(terra)
library(tmap)
library(here)

# Use sequence of landings to assign sequence of roads built

realRoads <- read_sf(here(data_path_drvd, "combined_revelstoke_roads.gpkg"))
cost <- rast(here(data_path_raw, "cost_surface_bc_ha_proj.tif"))
cutblocks <- read_sf(here(data_path_raw, "cutblocks_revelstoke.gpkg"))

cost <- crop(cost, vect(realRoads))

cost <- subst(cost, from = NA, to = 65000)

# burn in realRoads to have cost of 0 so that projected roads will follow them.
roadsRast <- rasterize(vect(realRoads), cost,
                              background = 0) == 0

# doesn't work if cost is 0 because areas with 0 cost are assumed to already be
# roads so no new ones will be built
cost <- cost * (roadsRast + 0.00001)

# Use highways, arterial, collector, local etc as non-forestry existing roads
roadsExist <- realRoads %>%
  filter(ROAD_CLASS %in% c("local", "arterial", "collector", "highway", "ramp",
                           "pedestrian", "runway", "yield", "driveway", "strata",
                           "lane", "service", "ferry"))

# set pre-existing roads to year 1989
roadsExist$Year <- 1950

landings <- getLandingsFromTarget(cutblocks, sampleType = "centroid")

# initialize sim list with first landings set
oldSim <- projectRoads(landings[landings$HARVEST_YEAR == min(landings$HARVEST_YEAR),],
                                   cost,
                                   roadsExist)

oldSim$roads <- oldSim$roads %>%
  mutate(Year = ifelse(is.na(Year), min(landings$HARVEST_YEAR), Year))

harv_yrs <- sort(unique(landings$HARVEST_YEAR))

# iterate over landings sets using the sim list from the previous run as input
for (i in 2:length(harv_yrs)) {
  print(i)
  newSim <- projectRoads(sim =  oldSim,
                         landings = landings[landings$HARVEST_YEAR == harv_yrs[i],])

  newSim$roads <- newSim$roads %>%
    mutate(Year = ifelse(is.na(Year), harv_yrs[i], Year))

  oldSim <- newSim
}

write_sf(newSim$roads, here(data_path_drvd, "reconstructedRoads.gpkg"))


plot(newSim$roads["Year"])

# this didn't work that well becasue the reconstructed roads do not follow the
# observed roads as well as expected. Maybe need cost of existing roads to be
# closer to 0? or might just be related to rasterization and resolution
