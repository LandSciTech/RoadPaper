# Example area for demonstrating different methods

library(raster)
library(dplyr)
library(sf)
library(roads)
library(terra)
library(tmap)
library(purrr)
library(caribouMetrics)
library(pfocal)

devtools::load_all(".")

#set path for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg")
roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg")
tsaBoundaryPth = paste0(data_path_raw, "tsa27_58_testing.gpkg")
costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif")

# densities
high <- 0.00001
low <-  0.000005


cutblocks <- read_sf(cutblocksPth)
roads <- read_sf(roadsPth)
tsaCost <- rast(costPth)

projRoads <- read_sf(paste0(data_path_drvd, "TSA27/", "TSA27_random_1e-05.shp"))

exRoads <- roads %>% filter(AWARD_DATE <= as.Date("1990-01-01"))

# find a good location
qtm(st_as_sf(cutblocks), fill = NULL, borders.lwd = 2)+
  qtm(st_as_sf(roads), lines.col = "red", lines.lwd = 2)+
  qtm(st_as_sf(projRoads), lines.col = "green", lines.lwd = 2)+
  qtm(st_as_sf(exRoads), lines.lwd = 2)+
    tm_layout(legend.show = FALSE)

# # get extent
# plot(roads)
#
# bound <- draw()
#SpatExtent : 1537685.65481532, 1546695.23062125, 741830.821988938, 750131.44756752 (xmin, xmax, ymin, ymax)
bound <- ext(c(1537685.65481532, 1546695.23062125, 741830.821988938, 750131.44756752))

roads <- vect(roads) %>% crop(bound) %>% st_as_sf()
cutblocks <- vect(cutblocks) %>% crop(bound) %>% st_as_sf()
projRoads <- vect(projRoads) %>% crop(bound) %>% st_as_sf()
exRoads <- vect(exRoads) %>% crop(bound) %>% st_as_sf()
tsaCost <- crop(tsaCost, bound)

# add a large cutblock
plot(roads %>% st_geometry())
cut_add <- draw("polygon")
cut_add <- set.crs(cut_add, crs(roads))

cutblocks <- bind_rows(cutblocks, st_as_sf(cut_add))

#
tsb <- st_bbox(roads) %>% st_as_sfc() %>% list()


# save example area for other testing
write_sf(exRoads, paste0(data_path_drvd, "testing_ex_roads.gpkg"))
write_sf(tsb[[1]], paste0(data_path_drvd, "testing_tsb.gpkg"))
write_sf(cutblocks, paste0(data_path_drvd, "testing_cutblocks.gpkg"))
writeRaster(tsaCost, paste0(data_path_drvd, "testing_cost.tif"))


sampleDens <- c(low,high,low,high,low)
sampleType <- c("regular","regular","random","random","centroid")
paramTable <- tibble(sampleType, sampleDens,
                     runTime = vector("list", length(sampleDens)),
                     output = vector("list", length(sampleDens)),
                     roadDisturbance = vector("list", length(sampleDens)),
                     roadDensity = vector("list", length(sampleDens)),
                     roadPresence = vector("list", length(sampleDens)),
                     distanceToRoad = vector("list", length(sampleDens)),
                     forestryDisturbance = vector("list", length(sampleDens))) %>%
  distinct()

# re-run projections for just the small area with different parameters
mstProj <- projectAll(tsbs = tsb, paramTable = paramTable,
                      costSurface = tsaCost,
                      cutblocks = cutblocks,
                      existingRoads = exRoads,
                      fileLocation = paste0(data_path_drvd, "for_fig/mst"),
                      method = "mst")

lcpProj <- projectAll(tsbs = tsb,
                      paramTable = paramTable %>% filter(sampleType == "centroid"),
                      costSurface = tsaCost,
                      cutblocks = cutblocks,
                      existingRoads = exRoads,
                      fileLocation = paste0(data_path_drvd, "for_fig/lcp"),
                      method = "lcp")

allProj <- bind_rows(mstProj, lcpProj, .id = "method")

allMaps <- purrr::map(1:nrow(allProj),
                      ~ qtm(tsaCost, raster.style = "cont", raster.palette = "Greys",
                            raster.alpha = 0.5)+
  qtm(cutblocks, fill = NULL, borders.lwd = 2)+
  qtm(roads, lines.col = "red", lines.lwd = 2)+
  qtm(read_sf(allProj$output[[.x]]), lines.col = "green", lines.lwd = 2)+
  qtm(exRoads, lines.lwd = 2)+
  tm_layout(legend.show = FALSE, main.title = paste(ifelse(allProj$method[.x]== 1, "MST", "LCP"),
                                                allProj$sampleType[.x],
                                                ifelse(allProj$sampleType[.x] == "centroid", "",
                                                       allProj$sampleDens[.x]))))

tmap_save(tmap_arrange(allMaps, ncol = 2), "analysis/figures/projection_methods_figure.jpg",
          dpi = 300, height = 8, width = 7.48)
