# Prepare data for projections

library(bcdata)
library(dplyr)
library(sf)

# Data from BC gov
# * TSA boundaries
# * digital road atlas and forest tenure road FLNRORD datasets.
# * Harvest Areas of BC (Consolidated Cutblocks)

# Do we have or can we get data and code for building the cost raster?

# bcdc_search("timber supply area")

tsa_27 <- bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
  filter(TSA_NUMBER == "27") %>%
  collect()

# list of IDs
c(TSAs = "8daa29da-d7f4-401c-83ae-d962e3a28980",
  DRA = "bb060417-b6e6-4548-b837-f9060d94743e",
  for_rds = "243c94a1-f275-41dc-bc37-91d8a2b26e10",
  cutblocks = "b1b647a6-f271-42e0-9cd0-89ec24bce9f7")

dra_rds <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  filter(INTERSECTS(tsa_27)) %>%
  collect() %>%
  st_filter(tsa_27)

for_rds <- bcdc_query_geodata("243c94a1-f275-41dc-bc37-91d8a2b26e10") %>%
  filter(INTERSECTS(tsa_27)) %>%
  collect() %>%
  st_filter(tsa_27)

cutblocks <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
  filter(INTERSECTS(tsa_27)) %>%
  collect() %>%
  st_filter(tsa_27)

dra_rds2 <- mutate(dra_rds,
                   ROAD_CLASS = ifelse(ROAD_CLASS %in% c("freeway", "highway",
                                                         "arterial", "collector", "ramp",
                                                         "yield", "local", "lane",
                                                         "runway", "driveway"),
                                       "primary", ROAD_CLASS))

# try to compare to figure out how to combine
qtm(dra_rds2 %>% filter(!ROAD_CLASS %in% c("skid", "resource", "trail", "unclassified")),
      lines.col = "red")+
  qtm(for_rds)


qtm(for_rds_buf)+
  qtm(dra_rds2, lines.col = "red")

# need to use for_rds in order to have award date
# removing resource roads from DRA doesn't work because that leaves some
# disconnected forestry road areas

# Try the buffering method to remove roads from DRA that are in forestry roads.
# Need 50m buffer because sometimes they show the same road in slightly
# different locations
for_rds_buf <- st_buffer(for_rds, 50) %>% st_union() %>%
  st_make_valid()

dra_rds_dif <- st_difference(dra_rds, for_rds_buf)

comb_rds <- bind_rows(for_rds, dra_rds_dif) %>%
  filter(is.na(ROAD_CLASS) | ROAD_CLASS != "trail")

write_sf(comb_rds, "analysis/data/derived_data/combined_revelstoke_roads.gpkg")

# Another option might be this "integrated roads dataset". Their method is
# similar to my buffering one but they only join roads within 7m of each other
# which would likely leave many duplicates
bcdc_get_record("9568a219-819d-417a-be68-8431b6fb5de0")

# this appears to be Kyle's cost surface code but I am not sure if all the
# data sets are publicly available
# https://github.com/bcgov/clus/blob/main/R/Params/road_cost_surface.Rmd



