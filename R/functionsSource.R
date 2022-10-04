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
                           projectionMethod, sim = NULL) {

  subLandings <- getLandingsFromTarget(cutblockPoylgons,
                                       landingDens = sampleDensity,
                                       sampleType = sampleType)
  if(nrow(subCutblocks) == 0){
    return(NULL)
  }

  if(!is.null(sim)){
    # if sim list supplied use it so don't redo getGraph but change all the
    # other parts to the current inputs
    sim$costSurface <- costRaster

    sim$roads <- existingRoads

    sim$roadMethod <- projectionMethod

    subProjectionResults <- projectRoads(subLandings, sim = sim)
  } else{
    subProjectionResults <- projectRoads(subLandings,
                                         cost = costRaster,
                                         roads = existingRoads,
                                         roadMethod = projectionMethod)
  }


  gc(verbose = TRUE)

  return(subProjectionResults)

}

# function to generate all projections and raster layers for metrics

projectAll <- function(tsbs,paramTable, costSurface,
                       cutblocks, existingRoads, fileLocation, method) {

  for(i in 1:nrow(paramTable)) {
    # i = 1
    cRow = paramTable[i,]
    start <- Sys.time()

    projectionsList <- vector("list", nrow(paramTable))

    # use sim object so graph not re-calced, will be NULL for first iter
    projectionsList[[i]] <- fineResProject(tsbs,
                                           cutblockPoylgons = cutblocks,
                                           sampleDensity = cRow$sampleDens,
                                           sampleType = cRow$sampleType,
                                           costRaster = costSurface,
                                           existingRoads = existingRoads,
                                           projectionMethod = method,
                                           sim = projectionsList[[1]])


    projections <- projectionsList[[i]]$roads

    end <- Sys.time()

    paramTable$output[[i]] <- paste0(fileLocation, cRow$sampleType, "_",
                                     cRow$sampleDens, ".shp")
    paramTable$runTime[[i]] <- as.numeric(end - start)

    # save the projected roads to a file
    sf::write_sf(projections, paramTable$output[[i]])

    rm(projections)
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

  if(!is.null(klementProj)){
    klementRow <- c("klementQGIS", NA, NA, NA, NA, NA, NA, NA)

    paramTable <- rbind(paramTable, klementRow)

    paramTable$output[[nrow(paramTable)]] <- klementProj
  }

  cutblocksRaster <- terra::rasterize(terra::vect(cutblocks), nonAggregatedCostSurface)

  for(i in 1:nrow(paramTable)) {
    # i = 1
    print(i)
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
    distanceToRoadResults <- getDistFromSource(src, maxDist, kwidth = 3, method = "pfocal")

    paramTable$distanceToRoad[[i]] <- distanceToRoadResults
  }

  return(paramTable)

}


### Function to get means of metric rasters

getMetricMeans <- function(paramTable, cutblocks){

  metricsTable <- tibble(sampleType = paramTable$sampleType,
                         sampleDens = paramTable$sampleDens,
                         runTime = paramTable$runTime,
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
                          runTime = paramTable$runTime,
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
  prex_rast <- terra::crop(prex_rast, obs_rast)
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


agreeMetricsAll <- function(paramTable, prex_rast, prex_vect, boundary, cutblocks, nonAggregatedCostSurface){
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
                                        landCover = as(nonAggregatedCostSurface, "Raster"),
                                        projectPoly = boundary,
                                        anthroDist = terra::rasterize(terra::vect(cutblocks), nonAggregatedCostSurface) %>%
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

# load and filter inputs

prepInputs <- function(cutblocksPth, roadsPth, tsaBoundaryPth, costPth,
                       outPth, aggFact,
                       saveInputs = FALSE){
  if(!dir.exists(outPth)){
    dir.create(outPth)
  }
  ######### load in data for projections ########################################

  #forest harvest cutblocks
  cutblocks <- st_make_valid(st_read(cutblocksPth))
  #modern observed roads
  roads <- st_read(roadsPth)
  #boundary for running projection
  tsaBoundary <- st_read(tsaBoundaryPth)
  tsbs <- list(st_union(tsaBoundary))
  #cost surface raster layer
  bc_cost_surface <- terra::rast(costPth)

  ###### Parameters #############################################################

  # set years for projection start (the projection is set to go from 1990 onwards)
  roadsYear <- as.Date("1990-01-01")
  # set cutblock year
  cutblocksYear <- 1990

  #if lakes block path then set high value for lakes (NA) on cost surface.
  lakeValue <- 65000 #often this isn't needed but Revelstoke TSA requires this.

  ###############################################################################

  #filter roads by year to make existing forestry road network
  roadsExist <- filter(roads, AWARD_DATE <= roadsYear)

  cutblocksPrior <- filter(cutblocks, HARVEST_YEAR <= cutblocksYear)
  cutblocks <- filter(cutblocks, HARVEST_YEAR > cutblocksYear)

  ### prepare cost surface layer
  tsaCost <- crop(bc_cost_surface, tsaBoundary)

  if(aggFact > 1){
    tsaCost <- terra::aggregate(tsaCost, fact = aggFact, fun = terra::mean)
  }

  # burn roads into cost raster
  roadsExist_rast <- terra::rasterize(terra::vect(roadsExist), terra::rast(tsaCost),
                                      background = 0) == 0

  tsaCost_st <- tsaCost * roadsExist_rast
  roadsExist <- roadsExist %>%  st_transform(st_crs(tsaCost_st))

  # setting lake values high because they are blocking paths if NA - not necessary for all landscapes
  tsaCost_st <- terra::subst(tsaCost_st, from = NA, to = lakeValue)

  # This doesn't work because some areas dont have any existing roads
  # # Break the area into smaller parts to process separately to avoid memory problems
  # grid <- st_make_grid(tsaBoundary, n = 5, offset = c(1472229, 655853.0))
  #
  # tsa_parts <- st_union(tsaBoundary) %>% st_intersection(grid) %>% st_as_sf() %>%
  #   st_make_valid() %>%
  #   mutate(ID = 1:n()) %>%
  #   split(factor(1:nrow(.)))

  if(saveInputs){
    terra::writeRaster(tsaCost_st, paste0(outPth, "input_cost.tif"), overwrite = TRUE)
    sf::write_sf(roadsExist,  paste0(outPth, "input_roads.gpkg"))
    sf::write_sf(cutblocks,  paste0(outPth, "input_cutblocks.gpkg"))
  }

  out <- dplyr::lst(tsaCost_st, roadsExist, cutblocks, tsbs, bc_cost_surface,
                    tsaBoundary, cutblocksPrior, roadsExist_rast)
}


# run all projections and summarise results for one tsa
run_projections <- function(cutblocksPth, roadsPth, tsaBoundaryPth, costPth,
                            outPth, klementProj, low, high, aggFact, method = "mst",
                            saveInputs = FALSE){

  inputs <- prepInputs(cutblocksPth, roadsPth, tsaBoundaryPth, costPth,
             outPth, aggFact, saveInputs = saveInputs)

  tsaCost_st <- inputs$tsaCost_st
  roadsExist <- inputs$roadsExist
  cutblocks <- inputs$cutblocks
  tsbs <- inputs$tsbs
  bc_cost_surface <- inputs$bc_cost_surface
  tsaBoundary <- inputs$tsaBoundary

  # parameter table creation for running projections
  sampleDens <- c(low,high,low,high,low)
  sampleType <- c("regular","regular","random","random","centroid")
  paramTable <- tibble(sampleType, sampleDens,
                       runTime = vector("list", length(sampleDens)),
                       output = vector("list", length(sampleDens)),
                       roadDisturbance = vector("list", length(sampleDens)),
                       roadDensity = vector("list", length(sampleDens)),
                       roadPresence = vector("list", length(sampleDens)),
                       distanceToRoad = vector("list", length(sampleDens)),
                       forestryDisturbance = vector("list", length(sampleDens))) %>%
    distinct()

  #Running projections
  allResults <- projectAll(tsbs = tsbs, paramTable = paramTable,
                           costSurface = tsaCost_st,
                           cutblocks = cutblocks,
                           existingRoads = roadsExist,
                           fileLocation = outPth,
                           method = method)

  # recreate allResults after a restart using saved files
  # allResults <- paramTable %>%
  #   mutate(output = paste0(outPth, sampleType, "_",
  #                          sampleDens, ".shp"))

  # Using David's saved results
  # allResults <- paramTable %>%
  #   mutate(output = paste0(data_path_drvd, "combinedTSBRoads", "_",
  #                          dplyr::case_when(sampleType == "centroid" ~ "C",
  #                                    sampleType == "random" & sampleDens == 1e-04 ~ "RA2",
  #                                    sampleType == "random" & sampleDens == 1e-06 ~ "RA1",
  #                                    sampleType == "regular" & sampleDens == 1e-04 ~ "RE2",
  #                                    sampleType == "regular" & sampleDens == 1e-06 ~ "RE1"),
  #                          ".shp"))



  # creating raster layers of the various metrics
  allMetrics <- calcMetrics(paramTable = allResults,
                            boundary = tsaBoundary,
                            nonAggregatedCostSurface = bc_cost_surface,
                            observedRoads = roadsPth,
                            klementProj = klementProj,
                            cutblocks = cutblocks,
                            costSurface = tsaCost_st)

  #Retrieving mean values for cutover and overall
  meanTable <- getMetricMeans(allMetrics, cutblocks)

  meanTable <-  mutate(meanTable,
                       sampleDens = case_when(
                         sampleType == "centroid" ~ "centroid",
                         sampleType == "observed" ~ "observed",
                         sampleType == "klementQGIS" ~ "klementQGIS",
                         sampleDens == low ~ "low sample density",
                         sampleDens == high ~"high sample density"))


  meanTable #resulting table with all mean values from the metrics (overall & cutover)

  meanTable <- mutate(meanTable, across(where(is.list), unlist))

  write.csv(meanTable, paste0(outPth, "mean_table.csv"), row.names = FALSE)

  # compare spatially explicit agreement
  agreeTable <- agreeMetricsAll(allMetrics, prex_rast = inputs$roadsExist_rast == 0,
                                prex_vect = roadsExist, boundary = tsaBoundary,
                                cutblocks = inputs$cutblocksPrior,
                                nonAggregatedCostSurface = bc_cost_surface)

  write.csv(agreeTable, paste0(outPth, "agree_table.csv"), row.names = FALSE)
}
