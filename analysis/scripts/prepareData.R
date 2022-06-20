# Prepare data for projections

library(bcdata)

# Data from BC gov
# * TSA boundaries
# * digital road atlas and forest tenure road FLNRORD datasets.
# * Harvest Areas of BC (Consolidated Cutblocks)

# Do we have or can we get data and code for building the cost raster?

# bcdc_search("timber supply area")

bc_tsas <-bcdc_get_data("8daa29da-d7f4-401c-83ae-d962e3a28980")


grid <- st_make_grid(tsaBoundary, n = 5, offset = c(1472229, 655853.0))

tsa_parts <- st_union(tsaBoundary) %>% st_intersection(grid) %>% st_as_sf() %>%
  st_make_valid() %>%
  mutate(ID = 1:n()) %>%
  split(factor(1:nrow(.)))



