library(dplyr)
library(sf)
library(roads)
library(terra)
library(tmap)
library(purrr)
library(caribouMetrics)

devtools::load_all()

#set path for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"


# compile results after running on cloud
bench_res <- list.files("analysis/data/derived_data/bench_results", pattern = "bench_.*.rds",
                        full.names = TRUE) %>%
  purrr::map(readRDS) %>%
  bind_rows() %>%
  tidyr::separate(id, into = c("method", "sampleType", "sampleDens", "agg", "cutblocks_real"),
                  sep = "_",
                  extra = "merge", convert = TRUE)

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

param_tbl %>% left_join(bench_res, by = c("method", "sampleType", "sampleDens",
                                          "agg", "cutblocks_real")) %>%
  tibble::rowid_to_column() %>%
  filter(is.na(resolution)) %>%
  pull(rowid)

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

                aggFact = 10, #factor of aggregation of cost surface. 1 = no aggregation.
                load_file = "results",
                saveInputs = FALSE,
                replaceNA = -65000)
