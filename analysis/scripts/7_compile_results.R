library(dplyr)
library(sf)
library(roads)
library(terra)
library(tmap)
library(purrr)
library(caribouMetrics)



#set path for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"


# compile results after running on cloud
bench_res <- list.files("analysis/data/derived_data/bench_results", pattern = "bench_.*.rds",
                        full.names = TRUE) %>%
  purrr::map(readRDS) %>%
  bind_rows()


param_tbl

run_projections(param_tbl %>% filter(agg == 100) %>%
                  rowwise() %>%
                  mutate(across(everything(), as.character)) %>%
                  mutate(output = file.path(data_path_drvd, "bench_results",
                                            paste0("result_",
                                                   paste0(c_across(everything()), collapse = "_"),
                                                   ".rds"))),
                cutblocksPth = paste0(data_path_drvd, "cutblocks_revelstoke_real.gpkg"),
                roadsPth = paste0(data_path_drvd, "combined_revelstoke_roads.gpkg"),
                tsaBoundaryPth = paste0(data_path_raw, "tsa27_boundaries.gpkg"),
                costPth = paste0(data_path_drvd, "TSA27/dem_revelstokeCoarse.tif"),
                outPth = paste0(data_path_drvd, "TSA27_real_cuts/dem/"),

                #Klement QGIS projection results shapefile
                klementProj = paste0(data_path_drvd, "TSA27_real_cuts/", "klementProjection.shp"),

                aggFact = 100, #factor of aggregation of cost surface. 1 = no aggregation.
                load_file = "results",
                saveInputs = FALSE,
                replaceNA = -65000)
