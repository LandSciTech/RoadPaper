# Compare speeds and memory usage for different methods

# run on cloud on separate nodes using cloud/setup_azure.sh interactively
# First crop cost layer and dissaggregate so that we can then compare more levels of aggregation
# Also record memory usage. Using separate nodes will ensure no memory overlap between runs


###### Source scripts ######
# load in required libraries
library(dplyr)
library(sf)
library(roads)
library(terra)

# load functions used in script
source("functionsSource.R")

# Prepare disaggregated cost surface
# dem <- rast(paste0(data_path_drvd, "TSA27/dem_revelstokeCoarse.tif"))
#
# tsaBoundary <- read_sf(paste0(data_path_raw, "tsa27_boundaries.gpkg"))
#
# # not using original 20m dem because need NAs from old cost
# rev_cost10 <- disagg(dem, 10,
#                      filename = file.path(data_path_drvd, "TSA27/dem_revelstoke_10.tif"))

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


sampleDens <- low
sampleType <- c("centroid")
paramTable4 <- tibble(method = "ilcp", sampleType, sampleDens, agg = c(10, 50, 100),
                     cutblocks_real = "revelstoke_real") %>%
  distinct()

param_tbl <- paramTable3 %>% bind_rows(paramTable4)

cRow <- param_tbl[row_ind,]
row_id <- cRow %>% unlist() %>% paste0(collapse = "_")

message("running row: ", row_id)

inputs <- prepInputs(
  cutblocksPth = paste0("cutblocks_", cRow$cutblocks_real, ".gpkg"),
  roadsPth = paste0("combined_revelstoke_roads.gpkg"),
  tsaBoundaryPth = paste0("tsa27_boundaries.gpkg"),
  costPth = paste0("dem_revelstoke_10.tif"),
  outPth = ".",
  aggFact = cRow$agg, #factor of aggregation of cost surface. 1 = no aggregation.
  saveInputs = FALSE,
  replaceNA = -65000
)


landings <- getLandingsFromTarget(inputs$cutblocks,
                                     landingDens = cRow$sampleDens,
                                     sampleType = cRow$sampleType)

# record memory, time, and graph size
prof <- peakRAM::peakRAM(
  #Running projections
  output <- projectRoads(landings,
                         weightRaster = inputs$tsaCost_st,
                         roads = inputs$roadsExist,
                         roadMethod = cRow$method,
                         weightFunction = gradePenaltyFn,
                         limitWeight = 65000)
)

res_out <- res(output$weightRaster)[1]
out <- data.frame(id = row_id, resolution = res_out,
                  n_verticies = igraph::vcount(output$g),
                  n_edges = igraph::ecount(output$g)) %>%
  bind_cols(prof[,-1])

saveRDS(out, file = paste0("bench_", row_id, ".rds"))
saveRDS(output$roads, file = paste0("result_", row_id, ".rds"))

