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
library(here)

# load functions used in script
devtools::load_all()

#densities to test
low <-  0.000001
high <- 0.00001

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

# add row for dlcp with regular high density sampling
paramTable <- bind_rows(paramTable, paramTable %>% slice(2) %>%
                          mutate(method = "dlcp"))


run_projections(
  paramTable,
  cutblocksPth = here(data_path_drvd, "testing_cutblocks.gpkg"),
  roadsPth = here(data_path_drvd, "testing_ex_roads.gpkg"),
  tsaBoundaryPth = here(data_path_drvd, "testing_tsb.gpkg"),
  costPth = here(data_path_drvd, "testing_cost.tif"),
  outPth = here(data_path_drvd, "test_tsb/"),

  #Klement QGIS projection results shapefile
  klementProj = NULL,
  aggFact = 1, #factor of aggregation of cost surface. 1 = no aggregation.
  saveInputs = FALSE
)

resPths <- list.files(here(data_path_drvd, "test_tsb"), pattern = ".gpkg")

pal <- MetBrewer::met.brewer("Redon", 8) %>% .[c(1,3,5,7)]

allMaps <- purrr::map(
  resPths,
  ~ qtm(rast(here(data_path_drvd, "testing_cost.tif")),
        raster.style = "cont", raster.palette = "Greys",
        raster.alpha = 0.5)+
    qtm(read_sf(here(data_path_drvd, "testing_cutblocks.gpkg")),
        fill = NULL, borders = pal[1], borders.lwd = 1.5)+
    qtm(read_sf(here(data_path_drvd, "test_tsb", .x)), lines.col = pal[2], lines.lwd = 2)+
    qtm(read_sf(here(data_path_drvd, "testing_ex_roads.gpkg")),
        lines.col = "black", lines.lwd = 2)+
    tm_layout(legend.show = FALSE,
              main.title = gsub(".gpkg", "", .x),
              main.title.size = 0.75))

tmap_arrange(allMaps)

