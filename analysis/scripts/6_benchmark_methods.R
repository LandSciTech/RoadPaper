# Compare speeds and memory usage for different methods

# Plan: run on cloud on separate nodes
# First crop cost layer and dissaggregate so that we can then compare more levels of aggregation
# in Previous runs ~ 1 hour was max for non aggregated raster
# only compare one density level
# Also record memory usage. Using separate nodes will ensure to memory overlap between runs

row_ind <- commandArgs(trailingOnly = TRUE)

#set path for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

###### Source scripts ######
# load in required libraries
library(dplyr)
library(sf)
library(roads)
library(terra)


# load functions used in script
devtools::load_all()

# Prepare disaggregated cost surface
# bc_cost <- rast(paste0(data_path_raw, "cost_surface_bc_ha.tif"))
#
# tsaBoundary <- read_sf(paste0(data_path_raw, "tsa27_boundaries.gpkg"))
#
# rev_cost <- crop(bc_cost, vect(tsaBoundary), snap = "out")
#
# rev_cost10 <- disagg(rev_cost, 10,
#                      filename = file.path(data_path_drvd, "revelstoke_cost_10.tif"))

param_tbl <- expand.grid(method = c("ilcp", "mst"), agg = c(1, 3, 10, 50, 100),
                         stringsAsFactors = FALSE)

method <- param_tbl$method[row_ind]
agg <- param_tbl$agg[row_ind]
sampleDens <- 0.00001

inputs <- prepInputs(
  cutblocksPth = paste0(data_path_raw, "cutblocks_revelstoke.gpkg"),
  roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
  tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
  costPth = paste0(data_path_drvd, "revelstoke_cost_10.tif"),
  outPth = paste0(data_path_drvd, "TSA27/"),
  aggFact = agg, #factor of aggregation of cost surface. 1 = no aggregation.
  saveInputs = FALSE
)


landings <- getLandingsFromTarget(inputs$cutblocks,
                                     landingDens = sampleDens,
                                     sampleType = "regular")

# record memory, time, graph size and total cost
prof <- peakRAM::peakRAM(
  #Running projections
  output <- projectRoads(landings,
                         cost = inputs$tsaCost_st,
                         roads = inputs$roadsExist,
                         roadMethod = method)
)


n_verticies <- igraph::vcount(output$g)
n_edges <- igraph::ecount(output$g)

out <- data.frame(method = method, resolution = res(output$costSurface)[1],
                  n_verticies = igraph::vcount(output$g),
                  n_edges = igraph::ecount(output$g)) %>%
  bind_cols(prof[,-1])

saveRDS(out, file = paste0(method, "_", agg, ".rds"))
