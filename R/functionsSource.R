#############################################################################

#################################################################################


# functions for roads


# rasterize line density

rasterizeLineDensity <- function(x, r) {
   spst_im <- spatstat.geom::pixellate(x = spatstat.geom::as.psp(sf::st_geometry(x)),
                                      W = maptools::as.im.RasterLayer(r),
                                      DivideByPixelArea = F)
  spst_rast <- raster::raster(spst_im)/(res(r)[1]*res(r)[2]/10000)
  spst_rast <- round(spst_rast, digits = 1)
  spst_rast <- raster::`crs<-`(spst_rast, value = raster::crs(r))

  return(terra::rast(spst_rast))
}



# Function to run projections at multiple landing densities


testLandingDens_mst <- function(cutblockPolygons, landingDens, sampleType) {

  variousLandingNum <- getLandingsFromTarget(cutblockPolygons, landingDens, sampleType)

  resultProj_mst <- projectRoads(landings = variousLandingNum,
                                 cost = tsaCost_st,
                                 roads = roadsExist,
                                 roadMethod = "mst")

  message(landingDens)

  return(resultProj_mst)

}


roadDisturbanceFootprint <- function(x, r, b) {
 # x = cRow$output[[1]]; r = tsaCost_st ; b = tsaBoundary

   tsaTemplate <- terra::classify(
     r,
     rcl = matrix(c(-1, 0, 0, 0, Inf, 1), byrow = TRUE, ncol = 3)
   )

  x <- st_buffer(x, dist = 1000)

  x <- terra::rasterize(terra::vect(x), r, background = 0)

  x <- terra::classify(
    x,
    rcl = matrix(c(-1, 0, 0, 0, Inf, 1), byrow = TRUE, ncol = 3)
  )

  #x <- merge(x, tsaTemplate)

  x <- terra::mask(x, terra::vect(b))
  x

}


fineResProject <- function(TSAsubset, cutblockPoylgons, sampleDensity,
                           sampleType, costRaster, existingRoads,
                           projectionMethod) {

  subCutblocks <- st_filter(cutblockPoylgons, TSAsubset)
  existingRoads <- st_filter(existingRoads, TSAsubset)
  subLandings <- getLandingsFromTarget(subCutblocks,
                                       landingDens = sampleDensity,
                                       sampleType = sampleType)
  if(nrow(subCutblocks) == 0){
    return(NULL)
  }

  subProjectionResults <- projectRoads(subLandings,
                                       cost = costRaster,
                                       roads = existingRoads,
                                       roadMethod = projectionMethod)
  gc(verbose = TRUE)

  return(subProjectionResults$roads)

}



# this function is to project using subset list and table of all different parameters
projectParamTable<-function(tsbs,paramTable) {

  for(i in 1:nrow(paramTable)) {

    cRow = paramTable[i,]

    projectionsList <- map(tsbs,
                           fineResProject,
                           cutblockPoylgons = cutblocks,
                           sampleDensity = cRow$sampleDens,
                           sampleType = cRow$sampleType,
                           costRaster = tsaCost_st,
                           existingRoads = roadsExist,
                           projectionMethod = "mst")

    projections <- do.call(rbind, projectionsList)


    paramTable$output[[i]] <- projections

  }

  return(paramTable)

}




#Josie's distance function

getDistKernelFromMax <- function(kdim) {
  # kdim=2
  kdim <- ceiling(kdim)
  wDim <- kdim * 2 + 1
  locSeq <- seq(-kdim, kdim)
  y <- matrix(locSeq, wDim, wDim)
  xx <- t(y)
  d <- (xx^2 + y^2)^0.5
  return(d)
}

uniformKernel <- function(dmax, cellDim = 1, useAveDist = F) {
  if (useAveDist) {
    # https://math.stackexchange.com/questions/3019165/average-distance-from-center-of-circle
    dmax <- 3 * dmax / 2
  }
  hdim <- ceiling(dmax / cellDim)
  weights <- getDistKernelFromMax(hdim)
  weights <- weights <= dmax / cellDim
  weights <- weights / sum(weights)
  return(weights)
}

getDistFromSource <- function(src,maxDist,kwidth=3,dissag=F){
  if(dissag){
    mwidth=res(src)[1]
    src=disaggregate(src,fact=kwidth)
  }else{
    mwidth=res(src)[1]*kwidth
  }
  src[src>0]=1
  mm = uniformKernel(kwidth,useAveDist=F)
  nSteps=ceiling(maxDist/mwidth)
  dd = src;dd=1-dd;dd[dd!=0]=NA
  cPop = src;cPop[is.na(cPop)]=0
  for(s in 1:nSteps){
    ssO2 = pfocal(as.matrix(cPop),mm,reduce_function="SUM", transform_function="MULTIPLY")
    ssD2 = cPop
    values(ssD2)=ssO2
    dd[is.na(dd)&(ssD2>0)]=s*mwidth
    cPop=ssD2;cPop[cPop>0]=1
  }
  if(dissag){
    dd=aggregate(dd,fact=kwidth)
  }
  dd[is.na(src)]=NA
  return(dd)
}




# metrics function

# sememingly not in use
# getAllMetricMeans <- function(roads, costSurface, boundary, cutblocks,
#                               notAggregatedCostSurface) {
#
#   roadDisturbanceResult <- roadDisturbanceFootprint(roads,
#                                                     costSurface,
#                                                     boundary)
#   overallMeanRDF <- cellStats(roadDisturbanceResult, "mean")
#   cutoverRDF <- mask(roadDisturbanceResult, cutblocks)
#   cutoverMeanRDF <- cellStats(cutoverRDF, "mean")
#   roadDensityResult <- rasterizeLineDensity(roads, costSurface)
#   roadDensityResult <- mask(roadDensityResult, boundary)
#   overallMeanRD <- cellStats(roadDensityResult, "mean")
#   cutoverRD <- mask(roadDensityResult, cutblocks)
#   cutoverMeanRD <- cellStats(cutoverRD, "mean")
#   roadPresence <- roadDensityResult
#   values(roadPresence)[values(roadPresence) > 0] <- 1
#   overallMeanRP <- cellStats(roadPresence, "mean")
#   cutoverRP <- mask(roadPresence, cutblocks)
#   cutoverMeanRP <- cellStats(cutoverRP, "mean")
#   cutblocksRaster <- rasterize(cutblocks, notAggregatedCostSurface)
#   disturbanceAnthro <- disturbanceMetrics(landCover = notAggregatedCostSurface,
#                                           linFeat = roads,
#                                           projectPoly = boundary,
#                                           anthroDist = cutblocksRaster)
#   overallMeanFDF <- cellStats(disturbanceAnthro@processedData$anthroBuff,
#                               "mean")
#   src <- roadPresence
#   src <- trim(src)
#   maxDist=10000 #in units of res
#   fastRough <- getDistFromSource(src, maxDist, kwidth = 3, dissag = F)
#   overallMeanDIST <- cellStats(fastRough, "mean")
#   cutoverDIST <- mask(fastRough, cutblocks)
#   cutoverMeanDIST <- cellStats(cutoverDIST, "mean")
#   meanList <- list(overallMeanRP,
#                    overallMeanRDF,
#                    overallMeanFDF,
#                    overallMeanDIST,
#                    overallMeanRD,
#                    cutoverMeanRP,
#                    cutoverMeanRDF,
#                    cutoverMeanDIST,
#                    cutoverMeanRD)
#   names(meanList) <- c("road presence overall mean",
#                        "road disturbance footprint overall mean",
#                        "forestry disturbance footprint overall mean",
#                        "distance to nearest road overall mean",
#                        "road density overall mean",
#                        "road presence cutover mean",
#                        "road disturbance footprint cutover mean",
#                        "distance to nearest road cutover mean",
#                        "road density cutover mean")
#   return(meanList)
#
# }


# function to generate all projections and raster layers for metrics

projectAll<-function(tsbs,paramTable, costSurface,
                     cutblocks, existingRoads, fileLocation) {

  for(i in 1:nrow(paramTable)) {
    # i = 1
    cRow = paramTable[i,]

    projectionsList <- map(tsbs,
                           fineResProject,
                           cutblockPoylgons = cutblocks,
                           sampleDensity = cRow$sampleDens,
                           sampleType = cRow$sampleType,
                           costRaster = costSurface,
                           existingRoads = existingRoads,
                           projectionMethod = "mst")

    projections <- do.call(rbind, projectionsList)


    paramTable$output[[i]] <- paste0(fileLocation, "_", cRow$sampleType, "_",
                                     cRow$sampleDens, ".shp")

    # save the projected roads to a file
    sf::write_sf(projections, paramTable$output[[i]])

    rm(projectionsList, projections)
    gc(verbose = TRUE)

  }
  paramTable
}

calcMetrics <- function(paramTable, klementProj, cutblocks,
                        nonAggregatedCostSurface, observedRoads, boundary,
                        costSurface){

  observedRow <- c("observed", NA, NA, NA, NA, NA, NA, NA)

  paramTable <- rbind(paramTable, observedRow)

  paramTable$output[[nrow(paramTable)]] <- observedRoads

  klementRow <- c("klementQGIS", NA, NA, NA, NA, NA, NA, NA)

  paramTable <- rbind(paramTable, klementRow)

  paramTable$output[[nrow(paramTable)]] <- klementProj

  cutblocksRaster <- terra::rasterize(terra::vect(cutblocks), nonAggregatedCostSurface)

  for(i in 1:nrow(paramTable)) {
    # i = 1
    cRow = paramTable[i,]

    out <- list(sf::read_sf(cRow$output))

    roadDisturbanceResults <- map(out,
                                  roadDisturbanceFootprint,
                                  r = costSurface,
                                  b = boundary)

    paramTable$roadDisturbance[[i]] <- roadDisturbanceResults[[1]]

    roadDensityResults <- map(out,
                              rasterizeLineDensity,
                              r = as(costSurface, "Raster"))

    paramTable$roadDensity[[i]] <- roadDensityResults[[1]]

    roadPresenceResults <- paramTable$roadDensity[[i]]

    roadPresenceResults <- terra::classify(
      roadPresenceResults,
      rcl = matrix(c(-1, 0, 0, 0, Inf, 1), byrow = TRUE, ncol = 3)
    )

    paramTable$roadPresence[[i]] <- roadPresenceResults

    forestryDisturanceResults <- disturbanceMetrics(
      out,
      landCover = as(nonAggregatedCostSurface, "Raster"),
      projectPoly = boundary,
      anthroDist = as(cutblocksRaster, "Raster")
    )

    paramTable$forestryDisturbance[[i]] <- terra::rast(forestryDisturanceResults@processedData$Anthro)


    src <- terra::trim(roadPresenceResults[[1]]) # thinks this is a list

    maxDist=10000 #in units of res
    distanceToRoadResults <- getDistFromSource(src, maxDist, kwidth = 3, dissag = F)

    paramTable$distanceToRoad[[i]] <- distanceToRoadResults


  }

  return(paramTable)

}


### Function to get means of metric rasters

getMetricMeans <- function(paramTable, cutblocks){

  metricsTable <- tibble(sampleType = paramTable$sampleType,
                         sampleDens = paramTable$sampleDens,
                         areaMean = "overall",
                         roadDisturbanceMean = vector("numeric", nrow(paramTable)),
                         roadDensityMean = vector("numeric", nrow(paramTable)),
                         roadPresenceMean = vector("numeric", nrow(paramTable)),
                         distanceToRoadMean = vector("numeric", nrow(paramTable)),
                         forestryDisturbanceMean = vector("numeric", nrow(paramTable)))

  for(i in 1:nrow(paramTable)) {
  # i = 1
    cRow = paramTable[i,]

    roadDisturbanceMean <- terra::global(cRow$roadDisturbance[[1]], "mean", na.rm = TRUE)
    metricsTable$roadDisturbanceMean[i] <- roadDisturbanceMean

    roadDensityMean <- terra::global(cRow$roadDensity[[1]], "mean", na.rm = TRUE)
    metricsTable$roadDensityMean[i] <- roadDensityMean

    roadPresenceMean <- terra::global(cRow$roadPresence[[1]], "mean", na.rm = TRUE)
    metricsTable$roadPresenceMean[i] <- roadPresenceMean

    distanceToRoadMean <- terra::global(cRow$distanceToRoad[[1]], "mean", na.rm = TRUE)
    metricsTable$distanceToRoadMean[i] <- distanceToRoadMean

    forestryDisturbanceMean <- terra::global(cRow$forestryDisturbance[[1]], "mean", na.rm = TRUE)
    metricsTable$forestryDisturbanceMean[i] <- forestryDisturbanceMean

  }

  metricsTable1 <- tibble(sampleType = paramTable$sampleType,
                          sampleDens = paramTable$sampleDens,
                          areaMean = "cutover",
                          roadDisturbanceMean = vector("numeric", nrow(paramTable)),
                          roadDensityMean = vector("numeric", nrow(paramTable)),
                          roadPresenceMean = vector("numeric", nrow(paramTable)),
                          distanceToRoadMean = vector("numeric", nrow(paramTable)),
                          forestryDisturbanceMean = vector("numeric", nrow(paramTable)))
  cutblocks <- terra::vect(cutblocks)

  for(i in 1:nrow(paramTable)) {

    cRow = paramTable[i,]

    croppedroadDisturbance <- mask(cRow$roadDisturbance[[1]], cutblocks)
    croppedroadDensity <- mask(cRow$roadDensity[[1]], cutblocks)
    croppedroadPresence <- mask(cRow$roadPresence[[1]], cutblocks)
    croppeddistanceToRoad <- mask(cRow$distanceToRoad[[1]], cutblocks)
    croppedforestryDisturbance <- mask(cRow$forestryDisturbance[[1]], cutblocks)

    croppedroadDisturbanceMean <- terra::global(croppedroadDisturbance, "mean", na.rm = TRUE)
    metricsTable1$roadDisturbanceMean[i] <- croppedroadDisturbanceMean

    croppedroadDensityMean <- terra::global(croppedroadDensity, "mean", na.rm = TRUE)
    metricsTable1$roadDensityMean[i] <- croppedroadDensityMean

    croppedroadPresenceMean <- terra::global(croppedroadPresence, "mean", na.rm = TRUE)
    metricsTable1$roadPresenceMean[i] <- croppedroadPresenceMean

    croppeddistanceToRoadMean <- terra::global(croppeddistanceToRoad, "mean", na.rm = TRUE)
    metricsTable1$distanceToRoadMean[i] <- croppeddistanceToRoadMean

    croppedforestryDisturbanceMean <- terra::global(croppedforestryDisturbance, "mean", na.rm = TRUE)
    metricsTable1$forestryDisturbanceMean[i] <- croppedforestryDisturbanceMean

  }

  metricsTable <- rbind(metricsTable, metricsTable1)

  return(metricsTable)

}


###
#' compute agreement metrics
#'
#' @param obs_rast raster of observed disturbance 0/100
#' @param proj_rast raster of projected disturbance 0/1
#' @param prex_rast raster of pre-existing disturbance 0/10
#'
#' @return a table
#' @noRd
calcAgree <- function(obs_rast, proj_rast, prex_rast){
  proj_rast <- terra::crop(proj_rast, obs_rast)
  res <- obs_rast + prex_rast + proj_rast

  lu_tbl <- tibble::tribble(~value, ~agreement,
                            0, "Agree roadless",
                            1, "False positive",
                            10, "Pre-existing roads",
                            11, "Pre-existing roads",
                            100, "False negative",
                            101, "Agree roaded",
                            110, "Pre-existing roads",
                            111, "Pre-existing roads")

  res_tbl <- terra::freq(res) %>% as.data.frame() %>%
    left_join(lu_tbl, by = "value")
}

#' compute agreement metrics for all metrics
#'
#' @param obs_rast raster of observed disturbance 0/1
#' @param proj_rast a list of rasters of projected disturbance 0/1
#' @param prex_rast raster of pre-existing disturbance 0/1
#' @param boundary polygon boundary to mask rasters


agreeMetricsAll <- function(paramTable, prex_rast, prex_vect, boundary, cutblocks){
  obs_tbl <- paramTable %>% filter(sampleType == "observed") %>%
    dplyr::select(roadDisturbance, roadPresence, forestryDisturbance) %>%
    tidyr::pivot_longer(everything(), names_to = "metric", values_to = "obs_rast") %>%
    mutate(obs_rast = obs_rast %>%
             map(~terra::subst(.x, from = 1, to = 100)) %>%
             map(~terra::mask(.x, terra::vect(boundary))) %>%
             map(~terra::crop(.x, prex_rast)))

  prex_tbl <- tibble(
    metric = c("roadDisturbance", "roadPresence", "forestryDisturbance"),
    prex_rast = list(roadDisturbanceFootprint(prex_vect, !is.na(prex_rast), boundary),
                     prex_rast,
                     disturbanceMetrics(linFeat = prex_vect,
                                        landCover = as(!is.na(prex_rast), "Raster"),
                                        projectPoly = boundary,
                                        anthroDist = terra::rasterize(terra::vect(cutblocks), prex_rast) %>%
                                          as("Raster"))@processedData$Anthro %>% terra::rast())
  ) %>%
    mutate(prex_rast = prex_rast %>%
             map(~terra::subst(.x, from = 1, to = 10)) )

  paramTable %>% filter(sampleType != "observed") %>%
    dplyr::select(sampleType, sampleDens, roadDisturbance, roadPresence, forestryDisturbance) %>%
    tidyr::pivot_longer(-contains("Sample"), names_to = "metric", values_to = "rast") %>%
    left_join(obs_tbl, by = "metric") %>%
    left_join(prex_tbl, by = "metric") %>%
    mutate(res = pmap(lst(obs_rast, proj_rast = rast, prex_rast), calcAgree)) %>%
    tidyr::unnest(res) %>%
    dplyr::select(-rast, -obs_rast, -layer, -prex_rast)

}

