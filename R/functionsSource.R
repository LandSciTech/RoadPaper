#############################################################################

#################################################################################


# functions for roads

# rasterize line density
rasterizeLineDensity <- function(x, r, ptDensity = 1) {
  if(any(c("POINT", "MULTIPOINT") %in%
         sf::st_geometry_type(x, by_geometry = TRUE))){
    lfPt <- sf::st_collection_extract(x, "POINT")
    x <- sf::st_collection_extract(x, "LINESTRING")
  } else {
    lfPt <- slice(x, 0)
  }

  line_len <- terra::rasterizeGeom(terra::vect(x), r, fun = "length")

  cell_area <- terra::cellSize(r)/10000

  r <- round(line_len/cell_area, digits = 1)


  if(!is.null(ptDensity)){
    if(ptDensity > 2+2*2^0.5){
      warning("ptDensity is greater than the expected max of 4.828.",
              " see ?rasterizeLineDensity for details",
              call. = FALSE)
    }

    if(nrow(lfPt) > 0){
      lfR <- terra::rasterizeGeom(terra::vect(lfPt), r, fun = "count")

      lfR <- lfR * ptDensity * terra::res(r)[1]

      lfR <- round(lfR / (terra::res(r)[1] * terra::res(r)[2] / 10000), digits = 1)
      r <- r + lfR
    }
  }

  return(r)
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


# function to generate all projections and raster layers for metrics

projectAll <- function(tsbs,paramTable, weightRaster,
                       cutblocks, existingRoads, fileLocation, roadsInWeight=T, ...) {

  if(!is.element("weightFunction",names(paramTable))){
    paramTable$weightFunction = deparse1(formals(projectRoads)$weightFunction,collapse="\n")
  }

  for(i in 1:nrow(paramTable)) {
    # i = 1
    cRow = paramTable[i,]
    start <- Sys.time()

    subLandings <- getLandingsFromTarget(cutblocks,
                                         landingDens = cRow$sampleDens,
                                         sampleType = cRow$sampleType)

    projections <- projectRoads(landings = subLandings,
                                weightRaster =  weightRaster,
                                roads = existingRoads,
                                roadMethod = cRow$method,
                                weightFunction = eval(str2lang(cRow$weightFunction)),
                                roadsInWeight = roadsInWeight, ...)

    projections <- projections$roads

    end <- Sys.time()

    paramTable$output[[i]] <- file.path(fileLocation, paste0(cRow$sampleType, "_",
                                     cRow$sampleDens, "_", cRow$method, ".gpkg"))
    paramTable$runTime[[i]] <- as.numeric(end - start)

    # save the projected roads to a file
    sf::write_sf(projections, paramTable$output[[i]])

    rm(projections)
    gc(verbose = TRUE)

  }
  paramTable
}

calcMetrics <- function(paramTable, klementProj, cutblocks,
                        nonAggregatedweightRaster, observedRoads, boundary,
                        cutblocksPth, existingRoads,
                        weightRaster){
  # row for observed
  observedRow <- c("observed", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

  paramTable <- rbind(paramTable, observedRow)

  paramTable$output[[nrow(paramTable)]] <- observedRoads

  # row for QGIS Plugin
  if(!is.null(klementProj)){
    klementRow <- c("klementQGIS", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

    paramTable <- rbind(paramTable, klementRow)

    paramTable$output[[nrow(paramTable)]] <- klementProj
  }

  # row for if only consider roads in disturbance
  paramTable <- rbind(paramTable, c("cutOnly", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))

  cutblocksRaster <- terra::rasterize(terra::vect(cutblocks), nonAggregatedweightRaster)

  for(i in 1:nrow(paramTable)) {
    # i = 1
    print(i)
    cRow = paramTable[i,]

    if(cRow$sampleType == "cutOnly"){
      out <- existingRoads
    } else {
      out <- sf::read_sf(cRow$output)
    }

    roadDensityResults <- rasterizeLineDensity(out, r = weightRaster)
    paramTable$roadDensity[[i]] <- roadDensityResults

    roadPresenceResults <- terra::rasterize(terra::vect(out), weightRaster,
                                            background = 0)

    paramTable$roadPresence[[i]] <- roadPresenceResults

    src <- terra::trim(roadPresenceResults[[1]]) # thinks this is a list
    maxDist=10000 #in units of res
    distanceToRoadResults <- getDistFromSource(src, maxDist, kwidth = 3, method = "pfocal")
    paramTable$distanceToRoad[[i]] <- distanceToRoadResults

    forestryDisturanceResults <- disturbanceMetrics(
      out,
      landCover = nonAggregatedweightRaster,
      projectPoly = boundary,
      anthroDist = cutblocksRaster
    )
    paramTable$forestryDisturbance[[i]] <- forestryDisturanceResults@processedData$Anthro

    roadDisturbanceResults <- roadDisturbanceFootprint(out, r = weightRaster, b = boundary)
    paramTable$roadDisturbance[[i]] <- roadDisturbanceResults
  }

  return(paramTable)

}


### Function to get means of metric rasters

getMetricMeans <- function(paramTable, cutblocks){

  # Overall
  metricsTable <- tibble(sampleType = paramTable$sampleType,
                         sampleDens = paramTable$sampleDens,
                         runTime = paramTable$runTime,
                         method = paramTable$method,
                         areaMean = "overall")

  metricsAll <- paramTable %>% select(roadDisturbance:forestryDisturbance) %>%
    unlist() %>%
    map_dbl(~terra::global(.x, "mean", na.rm = TRUE)[1,1]) %>%
    as_tibble(rownames = "name") %>%
    tidyr::separate(name, into = c("name", "order"), sep = -1) %>%
    mutate(name = paste0(name, "Mean")) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "value")

  metricsTable <- bind_cols(metricsTable, metricsAll)

  # Inside Cutblocks
  metricsTable1 <- tibble(sampleType = paramTable$sampleType,
                          sampleDens = paramTable$sampleDens,
                          runTime = paramTable$runTime,
                          method = paramTable$method,
                          areaMean = "cutover")
  cutblocks <- terra::vect(cutblocks)

  metricsCut <- paramTable %>% select(roadDisturbance:forestryDisturbance) %>%
    unlist() %>%
    map_dbl(~ terra::mask(.x, cutblocks) %>%
              terra::global("mean", na.rm = TRUE) %>% .[1,1]) %>%
    as_tibble(rownames = "name") %>%
    tidyr::separate(name, into = c("name", "order"), sep = -1) %>%
    mutate(name = paste0(name, "Mean")) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "value")

  metricsTable1 <- bind_cols(metricsTable1, metricsCut)

  # Outside Cutblocks
  metricsTable2 <- tibble(sampleType = paramTable$sampleType,
                          sampleDens = paramTable$sampleDens,
                          runTime = paramTable$runTime,
                          method = paramTable$method,
                          areaMean = "notCutover")

  metricsNCut <- paramTable %>% select(roadDisturbance:forestryDisturbance) %>%
    unlist() %>%
    map_dbl(~ terra::mask(.x, cutblocks, inverse = TRUE) %>%
              terra::global("mean", na.rm = TRUE) %>% .[1,1]) %>%
    as_tibble(rownames = "name") %>%
    tidyr::separate(name, into = c("name", "order"), sep = -1) %>%
    mutate(name = paste0(name, "Mean")) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "value")

  metricsTable2 <- bind_cols(metricsTable2, metricsNCut)

  metricsTable <- rbind(metricsTable, metricsTable1, metricsTable2)

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
calcAgree <- function(obs_rast, proj_rast, prex_rast, return_res = FALSE){

  if(is(proj_rast, "character")){
    return(NULL)
  }

  proj_rast <- terra::crop(proj_rast, obs_rast)
  prex_rast <- terra::crop(prex_rast, obs_rast)
  res <- obs_rast + prex_rast + proj_rast

  if(return_res){
    return(res)
  }

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


agreeMetricsAll <- function(paramTable, prex_rast, prex_vect, boundary, cutblocks, nonAggregatedweightRaster){
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
                                        landCover = as(nonAggregatedweightRaster, "Raster"),
                                        projectPoly = boundary,
                                        anthroDist = terra::rasterize(terra::vect(cutblocks), nonAggregatedweightRaster) %>%
                                          as("Raster"))@processedData$Anthro %>% terra::rast())
  ) %>%
    mutate(prex_rast = prex_rast %>%
             map(~terra::subst(.x, from = 1, to = 10)) )

  paramTable %>% filter(sampleType != "observed") %>%
    dplyr::select(sampleType, sampleDens, method, roadDisturbance, roadPresence, forestryDisturbance) %>%
    tidyr::pivot_longer(-c(contains("Sample"), method), names_to = "metric", values_to = "rast") %>%
    left_join(obs_tbl, by = "metric") %>%
    left_join(prex_tbl, by = "metric") %>%
    mutate(res = pmap(lst(obs_rast, proj_rast = rast, prex_rast), calcAgree)) %>%
    tidyr::unnest(res) %>%
    dplyr::select(-rast, -obs_rast, -layer, -prex_rast)

}

# load and filter inputs

prepInputs <- function(cutblocksPth, roadsPth, tsaBoundaryPth, costPth,
                       outPth, aggFact, replaceNA = NULL,
                       saveInputs = FALSE){
  if(!dir.exists(outPth)){
    dir.create(outPth)
  }
  ######### load in data for projections ########################################

  #forest harvest cutblocks
  cutblocks <- st_make_valid(read_sf(cutblocksPth))
  #modern observed roads
  roads <- read_sf(roadsPth)
  #boundary for running projection
  tsaBoundary <- read_sf(tsaBoundaryPth)
  tsbs <- list(st_union(tsaBoundary))
  #cost surface raster layer
  bc_cost_surface <- terra::rast(costPth)

  ###### Parameters #############################################################

  # set years for projection start (the projection is set to go from 1990 onwards)
  roadsYear <- as.Date("1990-01-01")
  # set cutblock year
  cutblocksYear <- 1990

  ###############################################################################

  #filter roads by year to make existing forestry road network
  roadsExist <- filter(roads, AWARD_DATE <= roadsYear)

  cutblocksPrior <- filter(cutblocks, HARVEST_YEAR <= cutblocksYear)
  cutblocks <- filter(cutblocks, HARVEST_YEAR > cutblocksYear)

  ### prepare cost surface layer
  tsaCost <- crop(bc_cost_surface, st_buffer(tsaBoundary, 2000))

  if(aggFact > 1){
    tsaCost <- terra::aggregate(tsaCost, fact = aggFact, fun = terra::mean, na.rm = TRUE)
  }

  if(!is.null(replaceNA)){
    tsaCost <- terra::mask(tsaCost, tsaCost, updatevalue = replaceNA)
  }

  # burn roads into cost raster
  roadsExist_rast <- terra::rasterize(terra::vect(roadsExist), terra::rast(tsaCost),
                                      background = 0) == 0

  tsaCost_st <- tsaCost * roadsExist_rast
  roadsExist <- roadsExist %>%  st_transform(st_crs(tsaCost_st))

  # This doesn't work because some areas dont have any existing roads
  # # Break the area into smaller parts to process separately to avoid memory problems
  # grid <- st_make_grid(tsaBoundary, n = 5, offset = c(1472229, 655853.0))
  #
  # tsa_parts <- st_union(tsaBoundary) %>% st_intersection(grid) %>% st_as_sf() %>%
  #   st_make_valid() %>%
  #   mutate(ID = 1:n()) %>%
  #   split(factor(1:nrow(.)))

  if(saveInputs){
    terra::writeRaster(tsaCost_st, file.path(outPth, "input_cost.tif"), overwrite = TRUE)
    sf::write_sf(roadsExist,  file.path(outPth, "input_roads.gpkg"))
    sf::write_sf(cutblocks,  file.path(outPth, "input_cutblocks.gpkg"))
  }

  out <- dplyr::lst(tsaCost_st, roadsExist, cutblocks, tsbs, bc_cost_surface,
                    tsaBoundary, cutblocksPrior, roadsExist_rast)
}


# run all projections and summarise results for one tsa
run_projections <- function(paramTable,cutblocksPth, roadsPth, tsaBoundaryPth, costPth,
                            outPth, klementProj, aggFact, method = "mst",
                            saveInputs = FALSE, load_file = NULL, replaceNA = NULL){

  inputs <- prepInputs(cutblocksPth, roadsPth, tsaBoundaryPth, costPth,
             outPth, aggFact, saveInputs = saveInputs, replaceNA = replaceNA)

  tsaCost_st <- inputs$tsaCost_st
  roadsExist <- inputs$roadsExist
  cutblocks <- inputs$cutblocks
  tsbs <- inputs$tsbs
  bc_cost_surface <- inputs$bc_cost_surface
  tsaBoundary <- inputs$tsaBoundary

  #Running projections
  if(is.null(load_file)){
    allResults <- projectAll(tsbs = tsbs, paramTable = paramTable,
                             weightRaster = tsaCost_st,
                             cutblocks = cutblocks,
                             existingRoads = roadsExist,
                             fileLocation = outPth)
  } else {
    # recreate allResults after a restart using saved files
    if(file.exists(file.path(outPth, "mean_table.csv"))){
      runT <- read.csv(file.path(outPth, "mean_table.csv"), na.strings = c("", "NA")) %>%
        filter(areaMean == "overall") %>%
        semi_join(paramTable %>% mutate(order = 1:n()) %>%
                    select(sampleType, order, method, runTime),
                  by = join_by(sampleType, order, method)) %>%
        pull(runTime)
    } else {
      runT = NA_real_
    }

    allResults <- paramTable %>%
      mutate(output = file.path(outPth, paste0( sampleType, "_",
                             sampleDens, "_", method, ".gpkg"))) %>%
      mutate(runTime = runT)
  }

  if(!is.null(load_file)){
    doResults <- load_file == "results"
  } else {
    doResults <- FALSE
  }

  if(is.null(load_file) | doResults){
    # creating raster layers of the various metrics
    allMetrics <- calcMetrics(paramTable = allResults,
                              boundary = tsaBoundary,
                              nonAggregatedweightRaster = bc_cost_surface,
                              observedRoads = roadsPth,
                              klementProj = klementProj,
                              cutblocks = cutblocks,
                              cutblocksPth = cutblocksPth,
                              existingRoads = roadsExist,
                              weightRaster = tsaCost_st)

    # save the metrics for further study
    dplyr::select(allMetrics, -c(sampleType, sampleDens, method, runTime, output)) %>%
      colnames() %>%
      purrr::cross2(1:nrow(allMetrics)) %>%
      purrr::walk(~writeRastNotNull(allMetrics[[.x[[1]]]][[.x[[2]]]],
                                    file.path(outPth, paste0(.x[[1]], allMetrics$sampleType[.x[[2]]], "_",
                                                             allMetrics$sampleDens[.x[[2]]], "_",
                                                             allMetrics$method[.x[[2]]], ".tif"))))
  }

  # load metrics from saved files
  if(!is.null(load_file)){
    if(load_file == "metrics"){
      allMetrics <- allResults %>%
        bind_rows(data.frame(sampleType = c("observed", "cutOnly")))

      if(!is.null(klementProj)){
        allMetrics <- allMetrics %>%
          bind_rows(data.frame(sampleType = c("klementQGIS")))
      }

      met_res <- allMetrics %>%
        dplyr::select(-c(sampleType, sampleDens, method, runTime, output, weightFunction)) %>%
        colnames() %>%
        purrr::cross2(1:nrow(allMetrics)) %>%
        set_names(purrr::map_chr(., ~paste0(.x[[1]], "_", .x[[2]]))) %>%
        purrr::map(~terra::rast(file.path(outPth,
                                          paste0(.x[[1]], allMetrics$sampleType[.x[[2]]], "_",
                                                 allMetrics$sampleDens[.x[[2]]], "_",
                                                 allMetrics$method[.x[[2]]], ".tif")))) %>%
        tibble::as_tibble_col() %>%
        mutate(name = names(value)) %>%
        tidyr::separate(name, into = c("name", "order")) %>%
        tidyr::pivot_wider(names_from = "name", values_from = "value")

      allMetrics <- allMetrics %>%
        select(sampleType, sampleDens, method, runTime, output) %>%
        bind_cols(met_res)
    }
  }


  #Retrieving mean values for cutover and overall
  meanTable <- getMetricMeans(allMetrics, cutblocks)

  meanTable <-  mutate(meanTable,
                       sampleDens = case_when(
                         sampleType == "centroid" ~ "centroid",
                         sampleType == "observed" ~ "observed",
                         sampleType == "klementQGIS" ~ "klementQGIS",
                         sampleDens == low ~ "low sample density",
                         sampleDens == high ~"high sample density"))

  meanTable <- mutate(meanTable, across(where(is.list), unlist))

  meanTable #resulting table with all mean values from the metrics (overall & cutover)

  write.csv(meanTable, file.path(outPth, "mean_table.csv"), row.names = FALSE)

  # compare spatially explicit agreement
  agreeTable <- agreeMetricsAll(allMetrics, prex_rast = inputs$roadsExist_rast == 0,
                                prex_vect = roadsExist, boundary = tsaBoundary,
                                cutblocks = inputs$cutblocksPrior,
                                nonAggregatedweightRaster = bc_cost_surface)

  write.csv(agreeTable, file.path(outPth, "agree_table.csv"), row.names = FALSE)
}

writeRastNotNull <- function(rast, filename){
  if(is(rast, "character")){
    return(NULL)
  }
  terra::writeRaster(rast, filename, overwrite = TRUE)

}

doAspatPlot <- function(tab){
  meanTable_long <- tab %>% dplyr::select(-runTime, -order) %>%
    pivot_longer(-c(sampleType, sampleDens, areaMean, method),
                 names_to = "metric", values_to = "response")%>%
    mutate(sampleType = ifelse(sampleType == sampleDens, NA_character_,
                               sampleType),
           sampleDens = paste(method, sampleType, sampleDens) %>%
             factor(levels = c("mst NA centroid",
                               "NA NA observed",
                               "mst random low sample density",
                               "mst regular low sample density",
                               "mst random high sample density",
                               "mst regular high sample density",
                               "ilcp regular high sample density",
                               "NA NA klementQGIS",
                               "NA NA NA"),
                    labels = c("MST centroid",
                               "Observed",
                               "MST random low density",
                               "MST regular low density",
                               "MST random high density",
                               "MST regular high density",
                               "ILCP regular high density",
                               "Hardy QGIS",
                               "Cutblocks only")),
           metric = factor(metric,
                           levels = c("roadDensityMean", "roadPresenceMean",
                                      "distanceToRoadMean",
                                      "forestryDisturbanceMean",
                                      "roadDisturbanceMean"),
                           labels = c("Road\ndensity", "Road\npresence",
                                      "Distance\nto road",
                                      "Forestry\ndisturbance\nfootprint",
                                      "Road\ndisturbance\nfootprint")))

  observed_values <- filter(meanTable_long, sampleDens == "Observed")
  projected_values <- filter(meanTable_long, sampleDens != "Observed")

  # get proportional difference from observed
  prop_dif <- projected_values %>%
    left_join(observed_values %>% select(areaMean, metric, response),
              by = c("areaMean", "metric"),
              suffix = c("_proj", "_obs")) %>%
    mutate(prop_dif = (response_proj-response_obs)/response_obs,
           areaMean = factor(areaMean, labels = c("Cutblocks", "Outside\ncutblocks",
                                                  "Overall")))

  prop_dif %>%
    ggplot(aes(x = sampleDens, prop_dif, fill = sampleDens))+
    geom_hline(aes(yintercept = 0), color = "grey")+
    geom_col(position = position_dodge2(preserve = "single",
                                        width = 0.75))+
    facet_grid(areaMean~ metric, scales = "free")+
    theme_classic()+
    scale_fill_manual(values = met.brewer(pal_nm, 8) %>% .[c(8, 1:7)],
                      name = "Sampling\nMethod", guide = "none")+
    theme(text = element_text(size = 10), axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "right",
          axis.text.x=element_text(size=11, angle=50, vjust=1, hjust=1))+
    coord_flip()
}

doSpatPerf <- function(tab){
  matchData_sum <- tab %>% ungroup() %>%
    mutate(agreement = factor(agreement,
                              levels = c("False negative", "False positive",
                                         "Agree roaded","Agree roadless",
                                         "Pre-existing roads")),
           sampleDens = paste(method, sampleType, sampleDens) %>%
             str_replace("1e-06", "low density") %>%
             str_replace("1e-05", "high density") %>%
             str_replace("centroid low density", "centroid") %>%
             str_replace("klementQGIS NA", "klementQGIS") %>%
             factor(levels = c("mst centroid",
                               "mst random low density",
                               "mst regular low density",
                               "mst random high density",
                               "mst regular high density",
                               "ilcp regular high density",
                               "NA klementQGIS",
                               "NA cutOnly NA"),
                    labels = c("MST centroid",
                               "MST random low density",
                               "MST regular low density",
                               "MST random high density",
                               "MST regular high density",
                               "ILCP regular high density",
                               "Hardy QGIS",
                               "Cutblocks only")),
           metric = factor(metric,
                           levels = c("roadPresence",
                                      "forestryDisturbance",
                                      "roadDisturbance"),
                           labels = c("Road presence",
                                      "Forestry disturbance\nfootprint",
                                      "Road disturbance\nfootprint"))) %>%
    group_by(metric, sampleDens, agreement) %>%
    summarise(count = sum(count), .groups = "drop_last") %>%
    mutate(perc = count/sum(count)*100)


  # performance table
  perf_tbl <- matchData_sum %>%
    pivot_wider(c(metric, sampleDens), names_from = agreement,
                values_from = count, values_fill = 0) %>%
    mutate(sensitivity = `Agree roaded`/(`Agree roaded` + `False negative`),
           precision = `Agree roaded`/(`Agree roaded` + `False positive`),
           F_measure = (2*precision*sensitivity)/(precision+sensitivity)) %>%
    select(metric, sampleDens, sensitivity, precision, F_measure)
}

doSpatPlot <- function(tab){
  tab %>%
    pivot_longer(c(sensitivity, precision, F_measure), names_to = "perf_meas",
                 values_to = "value") %>%
    mutate(perf_meas = factor(perf_meas,
                              labels = c("F-measure", "Precision", "Sensitivity")),
           value = value  +0.01) %>%   # adding so 0 shows in figure
    ggplot(aes(sampleDens, value, fill = sampleDens))+
    geom_col()+
    scale_fill_manual(values = met.brewer(pal_nm, 8) %>% .[c(8, 1:7)],
                      name = "Sampling\nMethod", guide = "none")+
    facet_grid(metric ~ perf_meas)+
    coord_flip()+
    ylim(0, 1.01)+
    theme_classic()+
    labs(y = "Performance", x = NULL)
}
