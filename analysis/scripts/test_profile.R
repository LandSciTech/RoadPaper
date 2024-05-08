# Test speed on a smaller landscape and do profiling
# This script is sourced from the supplement.Rmd file.

library(terra)
library(dplyr)
library(sf)
library(roads)
library(igraph)
library(ggplot2)

#speed/memory benchmarking
data_path_raw <- "analysis/data/raw_data/"
out_path <- "analysis/figures/"

size <- 1000
lnds <- 10

landscape <- rast(here::here(paste0(data_path_raw, "cost_surface_bc_ha.tif")))

base_point <- c((1881188-159588)/2, (1748188-173788)/2)

make_test_set <- function(size, landscape, base_point, nlnds){
  the_res <- res(landscape)[1]
  ext_100 <- ext(c(base_point, base_point+size*the_res)[c(1,3,2,4)])
  landscape_100 <- crop(landscape, ext_100)
  landscape_100 <- mask(landscape_100, landscape_100, updatevalue = 100000)
  rds_100 <- st_linestring(x = matrix(c(ext_100[1], ext_100[3], ext_100[2], ext_100[4]),
                                      ncol = 2, byrow = TRUE)) %>%
    list() %>%
    st_as_sfc() %>% st_as_sf()
  st_crs(rds_100) <- st_crs(landscape)
  set.seed(10000)
  lnds_100 <- terra::spatSample(landscape_100, nlnds, method = "random", as.points = TRUE) %>%
    st_as_sf()

  return(list(cost = landscape_100, roads = rds_100, landings = lnds_100))
}

dat <- make_test_set(size, landscape, base_point, lnds)

projectRoads(dat$landings, dat$cost, dat$roads, roadsInCost = FALSE)



Rprof(here::here("analysis/data/derived_data/ilcp_100_Rprof.out"), interval = 0.01)
projectRoads(dat$landings, dat$cost, dat$roads, roadsInCost = FALSE, roadMethod = "ilcp")
Rprof(NULL)

Rprof(here::here("analysis/data/derived_data/mst_100_Rprof.out"), interval = 0.01)
projectRoads(dat$landings, dat$cost, dat$roads, roadsInCost = FALSE, roadMethod = "mst")
Rprof(NULL)
