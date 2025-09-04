# Project roads based on STSIM model in SyncroSim

# This script extracts harvest blocks from SyncroSim and projects roads to the
# center of each harvest block

# Before running this script you will need to download and install SyncroSim
# (https://syncrosim.com/download/) and open the raw_data/CaribouForest/Caribou Forest.ssim
# file to launch SyncroSim.


library(rsyncrosim)
library(roads)
library(terra)
library(sf)
library(fasterize)
library(caribouMetrics)
Sys.setenv(TZ = "EST")

# path to SyncroSim library, including file name of .ssim file
sslib <- "C:/Users/EndicottS/Documents/gitprojects/RoadPaper/analysis/data/raw_data/CaribouForest/Caribou Forest"

# output directory for projected roads
roadDir <- "C:/Users/EndicottS/Documents/gitprojects/RoadPaper/analysis/data/raw_data/CaribouForest/caribouRoads"

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
# e=list(LibraryFilePath="C:/Users/HughesJo/Documents/InitialWork/OntarioChurchill/Base/JHMiscD/ChurchillBC",ProjectId=1,ScenarioId=762)
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
# envBeginSimulation(GLOBAL_TotalIterations * GLOBAL_TotalTimesteps)

# datasheet(GLOBAL_Scenario)
# need to load a base road layer from somewhere JH has one?

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
cumHarv <- cost
cumHarv[!is.na(cumHarv)] <- 0
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

  cumHarv[newBlocks[[cm]] > 0] <- 1
  clandings <- getLandingsFromTarget(newBlocks[[cm]])

  if (length(sim) == 0) {
    sim <- projectRoads(clandings, cost, initialRoads, roadMethod = "mst")
  } else {
    sim <- projectRoads(clandings, sim = sim)
  }

  outRoadName <- paste0("roads.it", iteration, ".ts", timestep)

  outRoadPath <- paste0(roadDir, "/", outRoadName, ".tif")

  writeRaster(sim$roads, outRoadPath, overwrite = T)

  endTS <- Sys.time()

  elapsedTS <- as.numeric(endTS - startTS)

  timing[timestep, 2] <- elapsedTS
  timing[timestep, 3] <- nrow(clandings)

  if(timestep %in% c(1, 5, 10)){
    png(paste0("analysis/figures/stsim_roads_", cm, ".png"))
    plot(sim$weightRaster,
         col = data.frame(value = c(0, 1, 1000),
                          color = c("black", "grey80", "white")),
         legend = FALSE, main = paste0("Roads at timestep ", timestep))

    plotBlock <- newBlocks[[cm]]
    plotBlock[plotBlock == 0] <- NA
    plot(plotBlock, add = TRUE, legend = FALSE,
         col = data.frame(values = c(TRUE, FALSE),
                          color = c("grey40", "white")))

    plotRoads <- sim$roads
    plotRoads[!plotRoads] <- NA
    plot(plotRoads, add = TRUE, col = "red", legend = FALSE)
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
