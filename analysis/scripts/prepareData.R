# Prepare data for projections

library(bcdata)
library(dplyr)
library(sf)
library(here)
library(purrr)

#set paths for data
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"


# Download data -----------------------------------------------------------

# Data from BC gov
# * TSA boundaries
# * digital road atlas and forest tenure road FLNRORD datasets.
# * Harvest Areas of BC (Consolidated Cutblocks)

# Do we have or can we get data and code for building the cost raster?

# bcdc_search("timber supply area")

tsa_27 <- bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
  filter(TSA_NUMBER == "27") %>%
  collect()

# using .gpkg because writing to .shp abbreviates the column names
write_sf(tsa_27, here(data_path_raw, "tsa27_boundaries.gpkg"))

# list of IDs
rec_ids <- c(TSAs = "8daa29da-d7f4-401c-83ae-d962e3a28980",
  DRA = "bb060417-b6e6-4548-b837-f9060d94743e",
  for_rds = "243c94a1-f275-41dc-bc37-91d8a2b26e10",
  cutblocks = "b1b647a6-f271-42e0-9cd0-89ec24bce9f7")

# use a buffered version of the tsa boundary to include some nearby roads
tsa_27_buf <- st_buffer(tsa_27, 2000)

dra_rds <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  filter(INTERSECTS(tsa_27)) %>%
  collect() %>%
  st_filter(tsa_27_buf)

write_sf(dra_rds, here(data_path_raw, "dra_roads_revelstoke.gpkg"))

for_rds <- bcdc_query_geodata("243c94a1-f275-41dc-bc37-91d8a2b26e10") %>%
  filter(INTERSECTS(tsa_27)) %>%
  collect() %>%
  st_filter(tsa_27_buf)

write_sf(for_rds, here(data_path_raw, "forest_tenure_roads_revelstoke.gpkg"))

cutblocks <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
  filter(INTERSECTS(tsa_27)) %>%
  collect() %>%
  st_filter(tsa_27, .predicate = st_covered_by)

write_sf(cutblocks, here(data_path_raw, "cutblocks_revelstoke.gpkg"))

# store metadata for the downloaded raw data

get_rec_df <- function(id) {
  rec <- bcdc_get_record(id)
  rec[c("title", "id", "record_last_modified", "license_title", "license_url")] %>%
    as.data.frame()
}

meta_data <- map_dfr(rec_ids, get_rec_df) %>%
  mutate(record_url = paste0("https://catalogue.data.gov.bc.ca/dataset/", id),
         download_date = format(Sys.time(), "%Y-%m-%d"))

write.csv(meta_data, here(data_path_raw, "raw_data_readme.csv"))

# Combining road data sets ------------------------------------------------

# try to compare to figure out how to combine
# dra_rds2 <- mutate(dra_rds,
#                    ROAD_CLASS = ifelse(ROAD_CLASS %in% c("freeway", "highway",
#                                                          "arterial", "collector", "ramp",
#                                                          "yield", "local", "lane",
#                                                          "runway", "driveway"),
#                                        "primary", ROAD_CLASS))

# qtm(dra_rds2 %>% filter(!ROAD_CLASS %in% c("skid", "resource", "trail", "unclassified")),
#       lines.col = "red")+
#   qtm(for_rds)
#
#
# qtm(for_rds_buf)+
#   qtm(dra_rds2, lines.col = "red")

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

# need to limit roads included to only those from max harvest year or earlier
maxHarvYear <- cutblocks$HARVEST_YEAR %>% max %>% as.character() %>%
  paste0("-12-31") %>%  as.Date(format = "%Y-%m-%d")

# set roads with no year as existing in 1990
roadsYear <- as.Date("1990-01-01")
comb_rds$AWARD_DATE[is.na(comb_rds$AWARD_DATE)] <- roadsYear
comb_rds <- filter(comb_rds, AWARD_DATE <= maxHarvYear)

write_sf(comb_rds, here(data_path_drvd, "combined_revelstoke_roads.gpkg"))

# Another option might be this "integrated roads dataset". Their method is
# similar to my buffering one but they only join roads within 7m of each other
# which would likely leave many duplicates
bcdc_get_record("9568a219-819d-417a-be68-8431b6fb5de0")



# Cost surface? -----------------------------------------------------------

# this appears to be Kyle's cost surface code but I am not sure if all the
# data sets are publicly available
# https://github.com/bcgov/clus/blob/main/R/Params/road_cost_surface.Rmd



# Repeat for Fort Nelson --------------------------------------------------

tsa_8 <- bcdc_query_geodata("8daa29da-d7f4-401c-83ae-d962e3a28980") %>%
  filter(TSA_NUMBER == "08") %>%
  collect()

# using .gpkg because writing to .shp abbreviates the column names
write_sf(tsa_8, here(data_path_raw, "tsa8_boundaries.gpkg"))

# use a buffered version of the tsa boundary to include some nearby roads
tsa_8_buf <- st_buffer(tsa_8, 2000)

dra_rds <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  filter(INTERSECTS(tsa_8)) %>%
  collect() %>%
  st_filter(tsa_8_buf)

write_sf(dra_rds, here(data_path_raw, "dra_roads_ft_nelson.gpkg"))

for_rds <- bcdc_query_geodata("243c94a1-f275-41dc-bc37-91d8a2b26e10") %>%
  filter(INTERSECTS(tsa_8)) %>%
  collect() %>%
  st_filter(tsa_8_buf)

write_sf(for_rds, here(data_path_raw, "forest_tenure_roads_ft_nelson.gpkg"))

cutblocks <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
  filter(INTERSECTS(tsa_8)) %>%
  collect() %>%
  st_filter(tsa_8, .predicate = st_covered_by)

write_sf(cutblocks, here(data_path_raw, "cutblocks_ft_nelson.gpkg"))

# Combine roads

for_rds_buf <- st_buffer(for_rds, 50) %>% st_union() %>%
  st_make_valid()

dra_rds_dif <- st_difference(dra_rds, for_rds_buf)

comb_rds <- bind_rows(for_rds, dra_rds_dif) %>%
  filter(is.na(ROAD_CLASS) | ROAD_CLASS != "trail")

# need to limit roads included to only those from max harvest year or earlier
maxHarvYear <- cutblocks$HARVEST_YEAR %>% max %>% as.character() %>%
  paste0("-12-31") %>%  as.Date(format = "%Y-%m-%d")

# set roads with no year as existing in 1990
roadsYear <- as.Date("1990-01-01")
comb_rds$AWARD_DATE[is.na(comb_rds$AWARD_DATE)] <- roadsYear
comb_rds <- filter(comb_rds, AWARD_DATE <= maxHarvYear)

write_sf(comb_rds, here(data_path_drvd, "combined_ft_nelson_roads.gpkg"))
