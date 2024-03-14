# Prepare data for projections

library(bcdata)
library(dplyr)
library(sf)
library(here)
library(purrr)
library(osfr)

#set paths for data
data_path <- "analysis/data/"
data_path_raw <- "analysis/data/raw_data/"
data_path_drvd <- "analysis/data/derived_data/"

# Download method: Raw data can be downloaded using bcdata but it may have been
# updated since the paper was published so use OSF repository data to recreate
# the paper results. Note that we could not get permission to store data from
# the Digital Road Atlas so it will be downloaded with bcdata either way and the
# cost raster was provided by Kyle Lockheed and will be downloaded from OSF
# either way. Also note that only data for Revelstoke is included on OSF
down_meth <- "osf"
# down_meth <- "bcdata"

# Download data -----------------------------------------------------------

if(down_meth == "osf"){

  # Download the zip file of raw files from OSF
  osf_proj <- osf_retrieve_node("https://osf.io/8vmqh/")

  osf_ls_files(osf_proj) %>% filter(name == "raw_data.zip") %>%
    osf_download(path = data_path)

  # extract it to the data folder of the current project
  unzip("data/raw_data.zip", exdir = data_path)

  # load tsa boundaries to use to get road data
  tsa_27 <- read_sf(here(data_path_raw, "tsa27_boundaries.gpkg"))

  # use a buffered version of the tsa boundary to include some nearby roads
  tsa_27_buf <- st_buffer(tsa_27, 2000)

  for_rds <- read_sf(here(data_path_raw, "forest_tenure_roads_revelstoke.gpkg"))

  cutblocks <- read_sf(here(data_path_raw, "cutblocks_revelstoke.gpkg"))

  # Download current DRA roads because they could not be stored on OSF
  dra_rds <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
    filter(INTERSECTS(tsa_27)) %>%
    collect() %>%
    st_filter(tsa_27_buf)

  # don't overwrite if already exists
  try(write_sf(dra_rds, here(data_path_raw, "dra_roads_revelstoke.gpkg"),
               delete_layer = FALSE))

}

if(down_meth == "bcdata"){
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

  # DEM
  bcdc_search("digital elevation model")
  dem <- bcdc_get_record("7b4fef7e-7cae-4379-97b8-62b03e9ac83d")
  dem_dat <- bcdc_get_data("829937c4-6551-45a9-9f4b-4a1688a4190e")


  # This is the map with the tile names:
  # https://www2.gov.bc.ca/assets/gov/data/geographic/topography/250kgrid.pdf

  # map tiles for revelstoke 82N, 82M, 82L, 82K
  list.dem <- c("82n", "82m", "82l", "82k", "83d")
  dem_path <- file.path(data_path_raw, "bc_dem_tiles")
  dir.create(dem_path)
  for (i in list.dem) { # loop though list to grab data for each tile;
    # there are maximum 32 'sub-tiles' within each
    for(j in 1:16){
      j <- sprintf("%02d", j)
      try ({# some tiles don't exist; the 'try' command skips them in some cases,
        # but I noticed in other cases it fails, so be aware
        # e
        downloader::download(paste0 ("https://pub.data.gov.bc.ca/datasets/175624/",
                                     i, "/0", i, j,"_e.dem.zip"),
                             dest = paste0 (i, j, "_e.dem.zip"),
                             mode = "wb")
        unzip(paste0 (i, j, "_e.dem.zip"),
              exdir = dem_path)
        file.remove (paste0 (i, j, "_e.dem.zip"))
        #w
        downloader::download(paste0 ("https://pub.data.gov.bc.ca/datasets/175624/",
                                     i, "/0", i, j,"_w.dem.zip"),
                             dest = paste0 (i, j, "_w.dem.zip"),
                             mode = "wb")
        unzip(paste0 (i, j, "_w.dem.zip"),
              exdir = dem_path)
        file.remove (paste0 (i, j, "_w.dem.zip"))
      })
    }
  }

  filenames_82 <- list.files (dem_path, pattern = ".dem$", full.names = TRUE)

  vrt_82 <- terra::vrt(filenames_82)

  rev_dem <- terra::crop(vrt_82, tsa_27_buf %>% sf::st_transform(sf::st_crs(vrt_82)))

  rev_dem <- rev_dem*1
  # project to WGS84
  rev_dem2 <- terra::project(rev_dem, "epsg:3005")
  terra::writeRaster(rev_dem2,
                     filename = file.path(data_path_drvd, "dem_revelstoke.tif"))


}

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
# bcdc_get_record("9568a219-819d-417a-be68-8431b6fb5de0")



# Cost surface -----------------------------------------------------------

# The cost surface was provided by Kyle Lochhead. The code to create it is
# available here:
# https://github.com/bcgov/clus/blob/main/R/Params/road_cost_surface.Rmd

# download cost surface from OSF
osf_proj <- osf_retrieve_node("https://osf.io/8vmqh/")

osf_ls_files(osf_proj) %>% filter(name == "cost_surface_bc_ha.tif") %>%
  osf_download(path = "data/raw_data")

# Repeat for Fort Nelson --------------------------------------------------
if(down_meth == "bcdata"){
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

}
