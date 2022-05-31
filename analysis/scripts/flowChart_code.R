# creating images for flow chart



data_path <- "data/"

source("davidfolder/roadsLibrarySource.R")


cutblocks <- st_read(paste0(data_path, "fort_nelson/post1990_cutblocksTSA8_1.shp"))

cutblocks1 <- st_read(paste0(data_path, "fort_nelson/tsa8_cutblocks.shp"))

cutblocksPrior <- filter(cutblocks1, HARVESTYR <= 1990)

roadsExist <- st_read(paste0(data_path, "fort_nelson/tsa8_existingRoads.shp"))

tsaBoundary <- st_read(paste0(data_path, "fort_nelson/tsa8_boundaries.shp"))

tsaCost_st <- raster::raster(paste0(data_path, "fort_nelson/projections_gis/tsaCost_st_FortNelson.tif"))

cutblocks <- st_make_valid(cutblocks)

testLandingDens_mst <- function(cutblockPolygons, landingDens, sampleType) {
  
  variousLandingNum <- getLandingsFromTarget(cutblockPolygons, landingDens, sampleType)
  
  resultProj_mst <- projectRoads(landings = variousLandingNum,
                                 cost = tsaCost_st,
                                 roads = roadsExist,
                                 roadMethod = "mst")
  
  message(landingDens)
  
  return(resultProj_mst)
  
}


landingDensList <- list(0.000001, 0.00001, 0.0001)


source("davidfolder/runProjections.R")

mst_roadsTSA16 <- st_as_sf(lcp_roads$roads)
st_write(mst_roadsTSA16, "data/fort_nelson/flow_chart_layers/lcp_roads.shp") 

writeRaster(centroid_setMean_merge, "data/fort_nelson/flow_chart_layers/centroid_setMean_merge.tif")



lcp_roads <- roads::projectRoads(landings = cutblocks,
                                 cost = tsaCost_st,
                                 roads = roadsExist,
                                 roadMethod = "lcp")








