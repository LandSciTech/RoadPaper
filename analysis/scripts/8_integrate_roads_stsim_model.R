# Project roads based on STSIM model in SyncroSim
# If decommisionTime is not NA, roads are decommisioned after decommisionTime timesteps.

# This script extracts harvest blocks from SyncroSim and projects roads to the
# center of each harvest block, and decomissions roads after 5 timesteps.

# Before running this script you will need to download and install SyncroSim
# (https://syncrosim.com/download/) and open the raw_data/CaribouForest/Caribou Forest.ssim
# file to launch SyncroSim.

decommisionTime = 5

#devtools::install_github("LandSciTech/roads")
library(rsyncrosim)
library(roads)
library(terra)
library(sf)
library(fasterize)
library(caribouMetrics)
Sys.setenv(TZ = "EST")

# path to SyncroSim library, including file name of .ssim file
sslib <- "C:/Users/HughesJo/Documents/gitprojects/RoadPaper/analysis/data/raw_data/CaribouForest/Caribou Forest"

# output directory for projected roads
roadDir <- "C:/Users/HughesJo/Documents/gitprojects/RoadPaper/analysis/data/raw_data/CaribouForest/caribouRoads"

if(!file.exists(roadDir)){
  dir.create(roadDir,recursive=T)
}

GetDataSheetExpectData <- function(name, ssimObj) {
  ds <- datasheet(ssimObj, name)
  if (nrow(ds) == 0) {
    stop(paste0("No data for: ", name))
  }
  return(ds)
}

GetSingleValueExpectData <- function(df, name) {
  v <- df[, name]
  if (is.na(v)) {
    stop(paste0("Missing data for: ", name))
  }
  return(v)
}

# track time
startAll <- Sys.time()

# Environment
e <- list(LibraryFilePath = sslib, ProjectId = 1, ScenarioId = 12530)


GLOBAL_Session <- session()
GLOBAL_Library <- ssimLibrary(name = e$LibraryFilePath, session = GLOBAL_Session)
# GLOBAL_Project <- project(GLOBAL_Library, project = as.integer(e$ProjectId))
GLOBAL_Scenario <- scenario(GLOBAL_Library, scenario = as.integer(e$ScenarioId))
GLOBAL_RunControl <- GetDataSheetExpectData("stsim_RunControl", GLOBAL_Scenario)

GLOBAL_MaxIteration <- GetSingleValueExpectData(GLOBAL_RunControl, "MaximumIteration")
GLOBAL_MinIteration <- GetSingleValueExpectData(GLOBAL_RunControl, "MinimumIteration")
GLOBAL_MinTimestep <- GetSingleValueExpectData(GLOBAL_RunControl, "MinimumTimestep")
GLOBAL_MaxTimestep <- GetSingleValueExpectData(GLOBAL_RunControl, "MaximumTimestep")
GLOBAL_TotalIterations <- (GLOBAL_MaxIteration - GLOBAL_MinIteration + 1)
GLOBAL_TotalTimesteps <- (GLOBAL_MaxTimestep - GLOBAL_MinTimestep + 1)

# Simulation
myStratum <- datasheetSpatRaster(GLOBAL_Scenario, datasheet = "stsim_InitialConditionsSpatial",
                                 column = "StratumFileName")

# create demo road raster
myExt <- ext(myStratum)

initialRoads <- cbind(object = 1, part = 1, rbind(c(myExt$xmin, myExt$ymin), c(myExt$xmax, myExt$ymax))) %>%
  vect("lines")
initialRoads <- rasterize(initialRoads, myStratum)

# Only showing one iteration but could be used for multiple
iteration=1

newBlocks <- datasheetSpatRaster(
  GLOBAL_Scenario, datasheet = "stsim_OutputSpatialTransition",
  iteration = iteration,
  timestep = max(1, GLOBAL_MinTimestep):GLOBAL_MaxTimestep,
  subset = expression(TransitionGroupId == "Harvest")
)

# sort timestep names because sorted alphabetically rather than numerically
lastElement <- strsplit(names(newBlocks)[[1]], ".", fixed = T)[[1]]

lastElement <- lastElement[length(lastElement)]
if (grepl("it", lastElement, fixed = T)) {
  warning("Encountered odd spatial transition layer naming pattern. Use transformerInnerDebugSyncroSim.R in stsim-roads addon package to investigate further.")
  names(newBlocks) <- gsub(paste0(".", lastElement), "", names(newBlocks), fixed = T)
}

tag <- strsplit(names(newBlocks)[[1]], ".ts", fixed = T)[[1]][1]

eTimes <- sort(as.numeric(gsub(paste0(tag, ".ts"), "", names(newBlocks), fixed = T)))

sortNames <- paste0(tag, ".ts", eTimes)
missingBits <- setdiff(sortNames, names(newBlocks))
if (length(missingBits) > 0) {
  stop("Something is wrong. fix it.", paste(names(newBlocks), collapse = ","))
}

# block values don't reflect contiguous harvest areas so set all to 1
newBlocks <- subset(newBlocks, sortNames)
newBlocks[newBlocks > 0] <- 1
newBlocks[newBlocks <= 0] <- NA

# For example cost on current roads and newBlocks is 0, otherwise 1. 1000 for water bodies.
cost <- myStratum
cost <- mask(cost, initialRoads, maskvalues = 1, updatevalue = 0)
# cost <- mask(cost, newBlocks[[1]], maskvalues = 1, updatevalue = 0)
cost[is.na(cost)] <- 1000

sim <- list()
cumRoads <- cost
cumRoads[!is.na(cumRoads)] <- 0
timing <- data.frame(timestep = GLOBAL_MinTimestep:GLOBAL_MaxTimestep, elapsed = 0, nlandings = 0)

for (timestep in GLOBAL_MinTimestep:GLOBAL_MaxTimestep) {
  # iteration = 1;timestep=1
  startTS <- Sys.time()
  cm <- paste0(tag, ".ts", timestep)
  if (!is.element(cm, names(newBlocks))) {
    message("skipping ", cm)
    next
  } else {
    message("working on ", cm)
  }

  clandings <- getLandingsFromTarget(newBlocks[[cm]])

  if (!is.na(decommisionTime)||length(sim) == 0) {
    sim <- projectRoads(clandings, cost, initialRoads, roadMethod = "mst")
  } else {
    sim <- projectRoads(clandings, sim = sim)
  }

  outRoadName <- paste0("roads.it", iteration, ".ts", timestep)

  outRoadPath <- paste0(roadDir, "/", outRoadName, ".tif")

  writeRaster(sim$roads, outRoadPath, overwrite = T)

  if(!is.na(decommisionTime)){
    #age cum roads
    cumRoads[cumRoads>0] <- cumRoads[cumRoads>0]+1

    # remove roads over decommisionTime years old from network
    cumRoads <- mask(cumRoads, cumRoads > decommisionTime, maskvalues = 1, updatevalue = 0)

    #set cost low but not 0 to make new roads follow old paths without burning old roads into the current network.
    cost[sim$roads>0]<-0.0001
  }

  #reset currently used roads to 1.
  #Note that when decommisionTime is NA sim$roads includes the whole cumulative network
  cumRoads[sim$roads>0]<-1

  endTS <- Sys.time()

  elapsedTS <- as.numeric(endTS - startTS)

  timing[timestep, 2] <- elapsedTS
  timing[timestep, 3] <- nrow(clandings)

  if(timestep %in% seq(1:10)){
    png(paste0("analysis/figures/stsim_roads_dt",decommisionTime, cm, ".png"))
    plot(myStratum,
         col = data.frame(value = c(1),
                          color = c("grey95")),
         legend = FALSE, main = paste0("Roads at timestep ", timestep))

    plotBlock <- newBlocks[[cm]]
    plotBlock[plotBlock == 0] <- NA
    plot(plotBlock, add = TRUE, legend = FALSE,
         col = data.frame(values = c(TRUE, FALSE),
                          color = c("grey75", "white")))

    plotRoads <- cumRoads
    plotRoads[!plotRoads] <- NA
    colAll <- rev(c('#bae4bc','#7bccc4','#43a2ca','#0868ac','black'))
    if(is.na(decommisionTime)){
      plot(plotRoads, add = TRUE, legend = FALSE,col="black")
    }else{
      if(timestep<decommisionTime){
        colAll <- colAll[1:timestep]
      }
      plot(plotRoads, add = TRUE, legend = "bottomright",col=colAll)
    }
    dev.off()
  }
}

endAll <- Sys.time()

durationAll <- endAll - startAll

timing

mean(timing$elapsed)

durationAll
# plot the roads for each timestep
# rdRastFls <- list.files(roadDir, full.names = TRUE)
# rdRastFls <- rdRastFls[c(1, 3:10, 2)]
# rdRastLst <- lapply(rdRastFls, rast)
# purrr::walk((GLOBAL_MinTimestep+1):GLOBAL_MaxTimestep,
#        function(i) plot(rdRastLst[[i]], main = paste0("Roads at timestep ", i)))
