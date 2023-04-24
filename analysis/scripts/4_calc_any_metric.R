# Script to run an arbitrary metric on the raster road results for all methods

library(dplyr)
library(sf)
library(roads)
library(tmap)
library(purrr)
library(here)
library(terra)
library(ggplot2)

# set paths for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

# Step 1: -----------------------------------------------------------------
# Define a function to calculate the desired metric raster. It should take at
# least a vector road and a template raster.

# For example this function will create a raster that is 1 within d meters of
# the road and 0 otherwise and then mask it based on the study area boundary
roadDisturbanceFootprint <- function(x, r, b, d) {

  tsaTemplate <- terra::classify(
    r,
    rcl = matrix(c(-1, 0, 0, 0, Inf, 1), byrow = TRUE, ncol = 3)
  )

  x <- st_buffer(x, dist = d)

  x <- terra::rasterize(terra::vect(x), r, background = 0)

  x <- terra::classify(
    x,
    rcl = matrix(c(-1, 0, 0, 0, Inf, 1), byrow = TRUE, ncol = 3)
  )

  #x <- merge(x, tsaTemplate)

  x <- terra::mask(x, terra::vect(b))
  x

}


# Step 2: -----------------------------------------------------------------
# Create a raster of the metric for vector roads produced by each projection
# method and the observed roads

# file names of road networks
files <- c(list.files(here(data_path_drvd, "TSA27"),
                      pattern = "^rand|^reg|^klem.*shp",
                      full.names = TRUE),
           here(data_path_drvd, "combined_revelstoke_roads.gpkg"))

names(files) <- c(list.files(here(data_path_drvd, "TSA27"),
                             pattern = "^rand|^reg|^klem.*shp"),
                  "observed") %>%
  {gsub("\\..*", "", .)}

# raster template
rast_temp <- rast(here(data_path_drvd, "TSA27", "input_cost.tif"))

# study area boundary
bound <- read_sf( here(data_path_raw, "tsa27_boundaries.gpkg"))

# Cutblocks
cutblocks <- read_sf(here(data_path_drvd, "TSA27", "input_cutblocks.gpkg"))

metric_rasts <- map(files, read_sf) %>%
  map(roadDisturbanceFootprint, r = rast_temp, b = bound, d = 250)


# Step 3:  ----------------------------------------------------------------
# Calculate the mean of the metric for each raster

metric_means_overall <- metric_rasts %>%
  map_dfr(~terra::global(.x, "mean", na.rm = TRUE)[1,1]) %>%
  mutate(metric = "Footprint 250", Scale = "Overall")

metric_means_in_cuts <- metric_rasts %>%
  map_dfr(~ terra::mask(.x, cutblocks) %>%
            terra::global("mean", na.rm = TRUE) %>% .[1,1]) %>%
  mutate(metric = "Footprint 250", Scale = "Cutblocks")

metric_means_out_cuts <- metric_rasts %>%
  map_dfr(~ terra::mask(.x, cutblocks, inverse = TRUE) %>%
            terra::global("mean", na.rm = TRUE) %>% .[1,1]) %>%
  mutate(metric = "Footprint 250", Scale = "Outside cutblocks")

metric_means <- bind_rows(metric_means_overall, metric_means_in_cuts,
                          metric_means_out_cuts)

metric_means %>%
  tidyr::pivot_longer(cols = -c(Scale, observed, metric), names_to = "method",
                      values_to = "response") %>%
  mutate(prop_dif = (response - observed)/observed) %>%
  ggplot(aes(method, prop_dif))+
  geom_col()+
  facet_grid(Scale ~ metric)+
  coord_flip()+
  labs(x = "Method", y = "(Projected - Observed)/Observed")
