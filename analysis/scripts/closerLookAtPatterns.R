data_path_drvd <- "analysis/data/derived_data/"

##############
#get other little bits
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggpubr)
library(MetBrewer)
library(here)
library(tmap)
library(sf)
library(roads)
library(terra)
library(caribouMetrics)
library(pfocal)

devtools::load_all(here())
klementProj = paste0(data_path_drvd, "TSA27/", "klementProjection.shp")
out=sf::read_sf(klementProj)

# set example area
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg")
roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg")
tsaBoundaryPth = paste0(data_path_raw, "tsa27_58_testing.gpkg")
costPth = paste0(data_path_raw, "cost_surface_bc_ha.tif")

# densities
high <- 0.00001
low <-  0.000001
cutblocksI <- read_sf(cutblocksPth)
roadsI <- read_sf(roadsPth)
tsaCostI <- rast(costPth)

projRoadsI <- read_sf(paste0(data_path_drvd, "TSA27/", "random_1e-05_mst.gpkg"))
exRoadsI <- roads %>% filter(AWARD_DATE <= as.Date("1990-01-01"))

# find a good location
 qtm(st_as_sf(cutblocksI), fill = NULL, borders.lwd = 2)+
   qtm(st_as_sf(roadsI), lines.col = "red", lines.lwd = 2)+
   qtm(st_as_sf(projRoadsI), lines.col = "green", lines.lwd = 2)+
   qtm(st_as_sf(exRoadsI), lines.lwd = 2)+
     tm_layout(legend.show = FALSE)

 ext(tsaCost)
# # get extent
 expX = 0.02
 expY = 0.02
bound <- ext(c(1537685.65481532*(1-expX), 1546695.23062125*(1+expX), 741830.821988938*(1-expY), 750131.44756752*(1+expY)))

roads <- vect(roadsI) %>% crop(bound) %>% st_as_sf()
cutblocks <- vect(cutblocksI) %>% crop(bound) %>% st_as_sf()
exRoads <- vect(exRoadsI) %>% crop(bound) %>% st_as_sf()
tsaCost <- crop(tsaCostI, bound)
tsb <- st_bbox(cutblocksI) %>% st_as_sfc() %>% list()

qtm(st_as_sf(cutblocks), fill = NULL, borders.lwd = 2)+
  qtm(st_as_sf(roads), lines.col = "red", lines.lwd = 2)+
  qtm(st_as_sf(exRoads), lines.lwd = 2)+
  tm_layout(legend.show = FALSE)

outSmall = vect(out) %>% crop(bound) %>% st_as_sf()
outSmall$fid = NULL

write_sf(exRoads, paste0(data_path_drvd, "testing2_ex_roads.gpkg"))
write_sf(tsb[[1]], paste0(data_path_drvd, "testing2_tsb.gpkg"))
write_sf(cutblocks, paste0(data_path_drvd, "testing2_cutblocks.gpkg"))
writeRaster(tsaCost, paste0(data_path_drvd, "testing2_cost.tif"))
write_sf(roads, paste0(data_path_drvd, "testing2_obs_roads.gpkg"))
write_sf(outSmall, paste0(data_path_drvd, "testing2_hardyB.gpkg"))

# densities
high <- 0.00001
low <-  0.000001

sampleDens <- c(low,high,low,high,low)
sampleType <- c("regular","regular","random","random","centroid")
paramTable <- tibble(sampleType, sampleDens,
                     method = "mst",
                     runTime = vector("list", length(sampleDens)),
                     output = vector("list", length(sampleDens)),
                     roadDisturbance = vector("list", length(sampleDens)),
                     roadDensity = vector("list", length(sampleDens)),
                     roadPresence = vector("list", length(sampleDens)),
                     distanceToRoad = vector("list", length(sampleDens)),
                     forestryDisturbance = vector("list", length(sampleDens))) %>%
  distinct()

# re-run projections for just the small area with different parameters
devtools::load_all(here())

#TO DO: troubleshooting - why isn't projectAll working here?
mstProj <- projectAll(tsbs = tsb, paramTable = paramTable,
                      costSurface = tsaCost,
                      cutblocks = cutblocks,
                      existingRoads = exRoads,
                      fileLocation = here(data_path_drvd, "for_fig"))

dlcpProj <- projectAll(tsbs = tsb,
                       paramTable = paramTable %>%
                         filter(sampleType == "regular") %>%
                         mutate(method = "dlcp"),
                       costSurface = tsaCost,
                       cutblocks = cutblocks,
                       existingRoads = exRoads,
                       fileLocation = here(data_path_drvd,"for_fig"))

lcpProj <- projectAll(tsbs = tsb,
                      paramTable = paramTable %>%
                        filter(sampleType == "centroid") %>%
                        mutate(method = "lcp"),
                      costSurface = tsaCost,
                      cutblocks = cutblocks,
                      existingRoads = exRoads,
                      fileLocation = here(data_path_drvd,  "for_fig"))

hardyProj <-  lcpProj[1,]
hardyProj$sampleType="";hardyProj$sampleDens=NA
hardyProj$method="Hardy QGIS"
hardyProj$output =list(paste0(data_path_drvd, "testing2_hardyB.gpkg"))

allProj <- bind_rows(mstProj, dlcpProj, lcpProj, hardyProj, .id = "method") %>%
  arrange(desc(sampleType), sampleDens)

allMaps <- purrr::map(
  1:nrow(allProj),
  ~ qtm(tsaCost, raster.style = "cont", raster.palette = "Greys",
        raster.alpha = 0.5)+
    qtm(cutblocks, fill = NULL, borders = "grey40", borders.lwd = 1.5)+
    qtm(roads, lines.col = "red", lines.lwd = 2)+
    qtm(read_sf(here(allProj$output[[.x]])), lines.col = "yellow", lines.lwd = 2)+
    qtm(exRoads, lines.col = "black", lines.lwd = 2)+
    tm_layout(legend.show = FALSE,
              main.title = paste(ifelse(allProj$method[.x]== 1, "MST",
                                        ifelse(allProj$method[.x]== 2, "DLCP", "LCP")),
                                 allProj$sampleType[.x],
                                 ifelse(allProj$sampleType[.x] == "centroid", "",
                                        ifelse(allProj$sampleDens[.x] == 1e-06, "low density", "high density"))),
              main.title.size = 0.75))

tmap_save(tmap_arrange(allMaps, ncol = 2),
          here("analysis/figures/projection_methods_figure2.png"),
          dpi = 300, height = 10, width = 3.543)
