#############################################################################

#################################################################################


# functions for roads


# rasterize line density

rasterizeLineDensity <- function(x, r) {
  r[] <- 1:ncell(r)

  rPoly <- spex::polygonize(r) %>% purrr::set_names("ID", "geometry") %>%
    st_set_agr("constant")

  rp2 <- st_intersection(rPoly, st_set_agr(x, "constant")) %>%
    mutate(length = st_length(geometry) %>% units::drop_units()) %>%
    dplyr::select(ID, length, geometry) %>% st_drop_geometry() %>%
    group_by(ID) %>%
    summarise(length = round(sum(length, na.rm = TRUE)/(res(r)[1]*res(r)[2]/10000), digits = 1))

  rp2 <- left_join(rPoly %>% st_drop_geometry(), rp2, by = "ID") %>%
    mutate(length = tidyr::replace_na(length, 0))

  r[] <- rp2$length

  return(r)
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

   tsaTemplate <- r
  values(tsaTemplate)[values(tsaTemplate) > 0] <- 0


  x <- st_buffer(x, dist = 1000)

  x <- rasterize(x, r)

  values(x)[values(x) > 0] <- 1

  x <- merge(x, tsaTemplate)

  x <- mask(x, b)

}


fineResProject <- function(TSAsubset, cutblockPoylgons, sampleDensity,
                           sampleType, costRaster, existingRoads,
                           projectionMethod) {

  subCutblocks <- st_filter(cutblockPoylgons, TSAsubset)
  existingRoads <- st_filter(existingRoads, TSAsubset)
  subLandings <- getLandingsFromTarget(subCutblocks,
                                       landingDens = sampleDensity,
                                       sampleType = sampleType)
  subProjectionResults <- projectRoads(subLandings,
                                       cost = costRaster,
                                       roads = existingRoads,
                                       roadMethod = projectionMethod)
  subProjectionResults_roads <- subProjectionResults$roads
  return(subProjectionResults_roads)

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


getAllMetricMeans <- function(roads, costSurface, boundary, cutblocks,
                              notAggregatedCostSurface) {

  roadDisturbanceResult <- roadDisturbanceFootprint(roads,
                                                    costSurface,
                                                    boundary)
  overallMeanRDF <- cellStats(roadDisturbanceResult, "mean")
  cutoverRDF <- mask(roadDisturbanceResult, cutblocks)
  cutoverMeanRDF <- cellStats(cutoverRDF, "mean")
  roadDensityResult <- rasterizeLineDensity(roads, costSurface)
  roadDensityResult <- mask(roadDensityResult, boundary)
  overallMeanRD <- cellStats(roadDensityResult, "mean")
  cutoverRD <- mask(roadDensityResult, cutblocks)
  cutoverMeanRD <- cellStats(cutoverRD, "mean")
  roadPresence <- roadDensityResult
  values(roadPresence)[values(roadPresence) > 0] <- 1
  overallMeanRP <- cellStats(roadPresence, "mean")
  cutoverRP <- mask(roadPresence, cutblocks)
  cutoverMeanRP <- cellStats(cutoverRP, "mean")
  cutblocksRaster <- rasterize(cutblocks, notAggregatedCostSurface)
  disturbanceAnthro <- disturbanceMetrics(landCover = notAggregatedCostSurface,
                                          linFeat = roads,
                                          projectPoly = boundary,
                                          anthroDist = cutblocksRaster)
  overallMeanFDF <- cellStats(disturbanceAnthro@processedData$anthroBuff,
                              "mean")
  src <- roadPresence
  src <- trim(src)
  maxDist=10000 #in units of res
  fastRough <- getDistFromSource(src, maxDist, kwidth = 3, dissag = F)
  overallMeanDIST <- cellStats(fastRough, "mean")
  cutoverDIST <- mask(fastRough, cutblocks)
  cutoverMeanDIST <- cellStats(cutoverDIST, "mean")
  meanList <- list(overallMeanRP,
                   overallMeanRDF,
                   overallMeanFDF,
                   overallMeanDIST,
                   overallMeanRD,
                   cutoverMeanRP,
                   cutoverMeanRDF,
                   cutoverMeanDIST,
                   cutoverMeanRD)
  names(meanList) <- c("road presence overall mean",
                       "road disturbance footprint overall mean",
                       "forestry disturbance footprint overall mean",
                       "distance to nearest road overall mean",
                       "road density overall mean",
                       "road presence cutover mean",
                       "road disturbance footprint cutover mean",
                       "distance to nearest road cutover mean",
                       "road density cutover mean")
  return(meanList)

}


# function to generate all projections and raster layers for metrics

projectAll<-function(tsbs,paramTable, costSurface, boundary,
                     cutblocks, nonAggregatedCostSurface,
                     existingRoads, observedRoads) {

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


    paramTable$output[[i]] <- projections

  }

  observedRow <- c("observed", NA, NA, NA, NA, NA, NA, NA)

  paramTable <- rbind(paramTable, observedRow)

  paramTable$output[[6]] <- roads

  klementRow <- c("klementQGIS", NA, NA, NA, NA, NA, NA, NA)

  paramTable <- rbind(paramTable, klementRow)

  paramTable$output[[7]] <- klementProj

  cutblocksRaster <- rasterize(cutblocks, nonAggregatedCostSurface)

  for(i in 1:nrow(paramTable)) {
    # i = 1
    cRow = paramTable[i,]


    roadDisturbanceResults <- map(cRow$output,
                                  roadDisturbanceFootprint,
                                  r = costSurface,
                                  b = boundary)

    paramTable$roadDisturbance[[i]] <- roadDisturbanceResults[[1]] #good

    roadDensityResults <- map(cRow$output,
                              rasterizeLineDensity,
                              r = costSurface)

    paramTable$roadDensity[[i]] <- roadDensityResults[[1]] #good

    roadPresenceResults <- paramTable$roadDensity[[i]]

    values(roadPresenceResults)[values(roadPresenceResults) > 0] <- 1

    paramTable$roadPresence[[i]] <- roadPresenceResults[[1]] #doesn't show up as raster in table

    forestryDisturanceResults <- disturbanceMetrics(cRow$output,
                                                    landCover = nonAggregatedCostSurface,
                                                    projectPoly = boundary,
                                                    anthroDist = cutblocksRaster)

    paramTable$forestryDisturbance[[i]] <- forestryDisturanceResults@processedData$anthroBuff # this isn't working, can't put it in the table


    src <- trim(roadPresenceResults[[1]]) # thinks this is a list

    maxDist=10000 #in units of res
    distanceToRoadResults <- getDistFromSource(src, maxDist, kwidth = 3, dissag = F)

    paramTable$distanceToRoad[[i]] <- distanceToRoadResults


  }

  return(paramTable)

}


### Function to get means of metric rasters

getMetricMeans <- function(paramTable, cutblocks){

  sampleType <- c("regular","regular","random","random","centroid", "observed","klementQGIS")

  areaMean <- c("overall","overall","overall","overall","overall","overall", "overall")

  metricsTable <- tibble(sampleType, areaMean, roadDisturbanceMean = vector("numeric", length(sampleType)),
                         roadDensityMean = vector("numeric", length(sampleType)),
                         roadPresenceMean = vector("numeric", length(sampleType)),
                         distanceToRoadMean = vector("numeric", length(sampleType)),
                         forestryDisturbanceMean = vector("numeric", length(sampleType)))

  for(i in 1:nrow(paramTable)) {
  # i = 1
    cRow = paramTable[i,]

    roadDisturbanceMean <- cellStats(cRow$roadDisturbance[[1]], "mean")
    metricsTable$roadDisturbanceMean[i] <- roadDisturbanceMean

    roadDensityMean <- cellStats(cRow$roadDensity[[1]], "mean")
    metricsTable$roadDensityMean[i] <- roadDensityMean

    roadPresenceMean <- cellStats(cRow$roadPresence[[1]], "mean")
    metricsTable$roadPresenceMean[i] <- roadPresenceMean

    distanceToRoadMean <- cellStats(cRow$distanceToRoad[[1]], "mean")
    metricsTable$distanceToRoadMean[i] <- distanceToRoadMean

    forestryDisturbanceMean <- cellStats(cRow$forestryDisturbance[[1]], "mean")
    metricsTable$forestryDisturbanceMean[i] <- forestryDisturbanceMean

  }

  sampleType <- c("regular","regular","random","random","centroid", "observed","klementQGIS")

  areaMean <- c("cutover","cutover", "cutover","cutover","cutover","cutover","cutover")

  metricsTable1 <- tibble(sampleType, areaMean, roadDisturbanceMean = vector("numeric", length(sampleType)),
                         roadDensityMean = vector("numeric", length(sampleType)),
                         roadPresenceMean = vector("numeric", length(sampleType)),
                         distanceToRoadMean = vector("numeric", length(sampleType)),
                         forestryDisturbanceMean = vector("numeric", length(sampleType)))


  for(i in 1:nrow(paramTable)) {

    cRow = paramTable[i,]

    croppedroadDisturbance <- mask(cRow$roadDisturbance[[1]], cutblocks)
    croppedroadDensity <- mask(cRow$roadDensity[[1]], cutblocks)
    croppedroadPresence <- mask(cRow$roadPresence[[1]], cutblocks)
    croppeddistanceToRoad <- mask(cRow$distanceToRoad[[1]], cutblocks)
    croppedforestryDisturbance <- mask(cRow$forestryDisturbance[[1]], cutblocks)

    croppedroadDisturbanceMean <- cellStats(croppedroadDisturbance, "mean")
    metricsTable1$roadDisturbanceMean[i] <- croppedroadDisturbanceMean

    croppedroadDensityMean <- cellStats(croppedroadDensity, "mean")
    metricsTable1$roadDensityMean[i] <- croppedroadDensityMean

    croppedroadPresenceMean <- cellStats(croppedroadPresence, "mean")
    metricsTable1$roadPresenceMean[i] <- croppedroadPresenceMean

    croppeddistanceToRoadMean <- cellStats(croppeddistanceToRoad, "mean")
    metricsTable1$distanceToRoadMean[i] <- croppeddistanceToRoadMean

    croppedforestryDisturbanceMean <- cellStats(croppedforestryDisturbance, "mean")
    metricsTable1$forestryDisturbanceMean[i] <- croppedforestryDisturbanceMean

  }

  metricsTable <- rbind(metricsTable, metricsTable1)

  return(metricsTable)

  }





