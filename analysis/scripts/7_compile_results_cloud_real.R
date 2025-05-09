# Calculate summary metrics from results of benchmarking runs for real cutblocks
# run on cloud using cloud/setup_azure.sh interactively

library(dplyr)
library(sf)
library(roads)
library(terra)
library(purrr)
library(caribouMetrics)

# load functions used in script
source("functionsSource.R")

#set path for data
data_path_raw <- ""
data_path_drvd <- ""

# compile results on cloud

#densities to test
low <- 0.000001
high <- 0.00001

param_tbl <- expand.grid(method = c("ilcp", "mst"), agg = c(1, 3, 10, 50, 100),
                         stringsAsFactors = FALSE) %>%
  mutate(sampleDens = high,
         sampleType = "regular",
         cutblocks_real = "revelstoke_real")

# parameter table creation for running projections
sampleDens <- c(low,high,low,high,low)
sampleType <- c("regular","regular","random","random","centroid")
paramTable <- tibble(method = "mst", sampleType, sampleDens, agg = 10,
                     cutblocks_real = "revelstoke_real") %>%
  distinct()

# add row for ilcp with regular high density sampling
paramTable <- bind_rows(paramTable, paramTable %>% slice(2) %>% mutate(method = "ilcp"))

# add coarse resolution
paramTable2 <- paramTable %>% mutate(agg = 100) %>% bind_rows(paramTable)

paramTable3 <- bind_rows(paramTable2,
                         paramTable %>% mutate(cutblocks_real = "revelstoke")) %>%
  bind_rows(param_tbl) %>%
  distinct()

param_tbl <- paramTable3

# no longer actually running the projections here. This skips to the metrics part
run_projections(param_tbl %>% filter(agg == 10, cutblocks_real == "revelstoke_real") %>%
                  rowwise() %>%
                  mutate(across(everything(), as.character)) %>%
                  mutate(output = file.path(paste0("bench_results/result_",
                                                   paste0(c_across(everything()), collapse = "_"),
                                                   ".rds"))),
                cutblocksPth = paste0(data_path_drvd, "cutblocks_revelstoke_real.gpkg"),
                roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
                tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
                costPth = paste0(data_path_drvd, "dem_revelstokeCoarse.tif"),
                outPth = paste0(data_path_drvd, "TSA27_real_cuts/dem/"),

                #Klement QGIS projection results shapefile
                klementProj = paste0(data_path_drvd, "klementProjection.shp"),

                aggFact = 1, #factor of aggregation of cost surface. 1 = no aggregation.
                load_file = "results", # skip projection, do calculate metrics
                saveInputs = TRUE,
                replaceNA = -65000)

