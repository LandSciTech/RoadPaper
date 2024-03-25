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
library(ggplot2)
library(stringr)
library(ggpubr)
library(MetBrewer)
library(here)
library(tmap)

# load functions used in script
devtools::load_all()

# set rasterOptions to allow higher memory usage
#prevOpts <- terraOptions(memfrac = 0.9)

#densities to test
low <- 0.000001
high <- 0.00001

# For Revelstoke at fine resolution

#build new cost raster

dem = rast(paste0(data_path_drvd, "TSA27/dem_revelstoke.tif"))
costOld = rast(paste0(data_path_raw, "cost_surface_bc_ha.tif"))
cCost = crop(costOld,dem)
#get average elevation in each cost cell
cDem = resample(dem,cCost,method="average")
cDem[is.na(cCost)]=NA
cDem = cDem
range(cDem)
terra::writeRaster(cDem,
                   filename = file.path(data_path_drvd, "TSA27/dem_revelstokeCoarse.tif"), overwrite=T)

pal_nm <- "Redon"

fig_widths <- c(min = 1.18, single = 3.543, mid = 5.51, two = 7.48)

#library(pfocal)

# read in example area data
exRoads <- read_sf(here(data_path_drvd, "testing_ex_roads.gpkg"))
tsb <- read_sf(here(data_path_drvd,  "testing_tsb.gpkg"))
cutblocks <- read_sf(here(data_path_drvd,  "testing_cutblocks.gpkg"))
tsaCost <- rast(here(data_path_drvd,  "testing_cost.tif"))
roads <- read_sf(here(data_path_drvd,  "testing_obs_roads.gpkg"))
demCost = crop(cDem,tsaCost)
demCost[tsaCost==0]=0
plot(demCost)
range(demCost)

see = data.frame(demDif = seq(-0.3,0.3,length.out=1000))
see$cost = slopePenaltyFn(1,1+see$demDif,resolution=100)
plot(see$cost~see$demDif)

# densities
high <- 0.00001
low <-  0.000001

sampleDens <- c(high, high, low)
sampleType <- c("regular","random","centroid")
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

paramTable$weightFunction = deparse1(slopePenaltyFn,collapse="\n")
paramTable$weightFunction = gsub("limitCost = NA","limitCost = 65000",paramTable$weightFunction,fixed=T)


f2 <- eval(str2lang(paramTable$weightFunction[1]))

# re-run projections for just the small area with different parameters
mstProj <- projectAll(tsbs = tsb, paramTable = paramTable,
                      costSurface = demCost,
                      cutblocks = cutblocks,
                      existingRoads = exRoads,
                      fileLocation = here(data_path_drvd, "for_fig"),roadsInCost=F)

ilcpProj <- projectAll(tsbs = tsb,
                       paramTable = paramTable %>%
                         filter(sampleType == "regular") %>%
                         mutate(method = "ilcp"),
                       costSurface = demCost,
                       cutblocks = cutblocks,
                       existingRoads = exRoads,
                       fileLocation = here(data_path_drvd, "for_fig"),roadsInCost=F)

allProj <- bind_rows(mstProj, ilcpProj, .id = "method") %>%
  arrange(desc(sampleType), sampleDens)

# add Hardy QGIS results for the same area

allProj <- bind_rows(
  allProj,
  tibble(sampleType = "", sampleDens = 0, method = "Hardy QGIS",
         output = list(here(data_path_drvd, "for_fig",
                            "HardyQGISTest.gpkg")))) %>%
  mutate(mapTitle =  paste(ifelse(method == 1, "MST",
                                  ifelse(method == 2, "ILCP", method)),
                           sampleType,
                           ifelse(sampleDens == 1e-06 &
                                    sampleType != "centroid", "low density",
                                  ifelse(sampleDens == 1e-05,
                                         "high density", ""))))

# add extra row for legend
allProj <- bind_rows(allProj,
                     slice(allProj, 1))

allMaps <- purrr::map(
  1:nrow(allProj),
  ~ qtm(cutblocks, borders = "#92c5de", fill="#92c5de", borders.lwd = 1,)+
    qtm(demCost, raster.style = "cont", raster.palette = "Greys",
        raster.alpha = 0.25, raster.title = "Scaled Elevation")+
    qtm(roads, lines.col = "#0571b0", lines.lwd = 2,lines.lty="solid")+
    qtm(read_sf(here(allProj$output[[.x]])), lines.col = "#ca0020", lines.lwd = 2)+
    qtm(exRoads, lines.col = "black", lines.lwd = 2)+
    tm_layout(legend.show = .x == nrow(allProj),
              legend.only = .x == nrow(allProj),
              main.title = allProj$mapTitle[[.x]],
              main.title.size = 0.75))

allMaps[[nrow(allProj)]] <- allMaps[[nrow(allProj)]]+
  tm_add_legend(type = "line",
                labels = c("Existing roads", "Projected roads", "Observed roads"),
                col = c("black", "#ca0020", "#0571b0"),lty=c("solid","solid","solid"))+
  tm_add_legend(type = "fill",
                labels = "Cutblocks",
                col = "#92c5de", border.col ="#92c5de")+
  tm_layout(legend.only = TRUE, legend.position = c("center", "center"))

tmap_save(tmap_arrange(allMaps, ncol = 3),
          here("analysis/figures/projection_methods_figureDEM.png"),
          dpi = 300, height = 5, width = fig_widths["two"])



#force resolution arg and compare
paramTable$weightFunction = gsub("resolution = 1","resolution = 100",paramTable$weightFunction,fixed=T)
f2 <- eval(str2lang(paramTable$weightFunction[1]))

# re-run projections for just the small area with different parameters
mstProj <- projectAll(tsbs = tsb, paramTable = paramTable,
                      costSurface = demCost,
                      cutblocks = cutblocks,
                      existingRoads = exRoads,
                      fileLocation = here(data_path_drvd, "for_fig"),roadsInCost=F)

ilcpProj <- projectAll(tsbs = tsb,
                       paramTable = paramTable %>%
                         filter(sampleType == "regular") %>%
                         mutate(method = "ilcp"),
                       costSurface = demCost,
                       cutblocks = cutblocks,
                       existingRoads = exRoads,
                       fileLocation = here(data_path_drvd, "for_fig"),roadsInCost=F)

allProj <- bind_rows(mstProj, ilcpProj, .id = "method") %>%
  arrange(desc(sampleType), sampleDens)

# add Hardy QGIS results for the same area

allProj <- bind_rows(
  allProj,
  tibble(sampleType = "", sampleDens = 0, method = "Hardy QGIS",
         output = list(here(data_path_drvd, "for_fig",
                            "HardyQGISTest.gpkg")))) %>%
  mutate(mapTitle =  paste(ifelse(method == 1, "MST",
                                  ifelse(method == 2, "ILCP", method)),
                           sampleType,
                           ifelse(sampleDens == 1e-06 &
                                    sampleType != "centroid", "low density",
                                  ifelse(sampleDens == 1e-05,
                                         "high density", ""))))

# add extra row for legend
allProj <- bind_rows(allProj,
                     slice(allProj, 1))

allMaps <- purrr::map(
  1:nrow(allProj),
  ~ qtm(cutblocks, borders = "#92c5de", fill="#92c5de", borders.lwd = 1,)+
    qtm(demCost, raster.style = "cont", raster.palette = "Greys",
        raster.alpha = 0.25, raster.title = "Scaled Elevation")+
    qtm(roads, lines.col = "#0571b0", lines.lwd = 2,lines.lty="solid")+
    qtm(read_sf(here(allProj$output[[.x]])), lines.col = "#ca0020", lines.lwd = 2)+
    qtm(exRoads, lines.col = "black", lines.lwd = 2)+
    tm_layout(legend.show = .x == nrow(allProj),
              legend.only = .x == nrow(allProj),
              main.title = allProj$mapTitle[[.x]],
              main.title.size = 0.75))

allMaps[[nrow(allProj)]] <- allMaps[[nrow(allProj)]]+
  tm_add_legend(type = "line",
                labels = c("Existing roads", "Projected roads", "Observed roads"),
                col = c("black", "#ca0020", "#0571b0"),lty=c("solid","solid","solid"))+
  tm_add_legend(type = "fill",
                labels = "Cutblocks",
                col = "#92c5de", border.col ="#92c5de")+
  tm_layout(legend.only = TRUE, legend.position = c("center", "center"))

tmap_save(tmap_arrange(allMaps, ncol = 3),
          here("analysis/figures/projection_methods_figureDEMResolution.png"),
          dpi = 300, height = 5, width = fig_widths["two"])


if(0){
  # parameter table creation for running projections
  sampleDens <- c(low,high,low,high,low)
  sampleType <- c("regular","regular","random","random","centroid")
  paramTable <- tibble(sampleType, sampleDens, method = "mst",
                       runTime = vector("list", length(sampleDens)),
                       output = vector("list", length(sampleDens)),
                       roadDisturbance = vector("list", length(sampleDens)),
                       roadDensity = vector("list", length(sampleDens)),
                       roadPresence = vector("list", length(sampleDens)),
                       distanceToRoad = vector("list", length(sampleDens)),
                       forestryDisturbance = vector("list", length(sampleDens))) %>%
    distinct()

  # add row for ilcp with regular high density sampling
  paramTable <- bind_rows(paramTable, paramTable %>% slice(2) %>% mutate(method = "ilcp"))

  paramTable<-paramTable[nrow(paramTable)-1,]

  paramTable$weightFunction = deparse1(slopePenaltyFn,collapse="\n")

  f2 <- eval(str2lang(paramTable$weightFunction[1]))

  #
  run_projections(
    paramTable,
    cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg"),
    roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
    tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
    costPth = paste0(data_path_drvd, "TSA27/dem_revelstokeCoarse.tif"),
    outPth = paste0(data_path_drvd, "TSA27/dem"),

    #Klement QGIS projection results shapefile
    klementProj = paste0(data_path_drvd, "TSA27/", "klementProjection.shp"),

    aggFact = 1, #factor of aggregation of cost surface. 1 = no aggregation.
    # load_file = "results",
    saveInputs = TRUE
  )
  print(Sys.time())
}
