##### Metric table bar plots
# load libraries
source("davidfolder/roadsLibrarySource.R")
# data (from "meanTable") in the runningProjections script
overallData <- read.csv(paste0(data_path, "revelstoke/revelstoke_overall_means.csv"))
cutoverData <- read.csv(paste0(data_path, "revelstoke/revelstoke_cutover_means.csv"))

# organize data for ggplot (overall results)
observed_valuesOVERALL <- filter(overallData, responseType == "observed")
overallData_bars <- filter(overallData, sampleDensity %in% c("low sample density",
                                                             "high sample density", 
                                                             "centroid",
                                                             "klementQGIS"))
sampleType <- factor(overallData_bars$sampleDensity,
                     levels = c("centroid",
                                "low sample density",
                                "high sample density",
                                "klementQGIS"))
# Create overall bar plot
overallPlot <- ggplot(overallData_bars, aes(x = sampleType, response, fill = responseType))+
  geom_col(position = "dodge", width = 0.75)+
  geom_hline(aes(yintercept = response), 
             observed_valuesOVERALL, color = "black", linetype = "dashed")+
  facet_wrap(~factor(metric, levels=c("road density", "road presence", "distance to road", "forestry disturbance footprint","road disturbance footprint")), ncol = 1, scales = "free")+
  ggtitle("Overall")+
  theme_classic()+
  scale_fill_manual(values = c("#D55E00", "#F0E442", "#0072B2", "#009E73"))+
  theme(text = element_text(size = 15), axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")

# Organize data for ggplot (cutover results)
observed_valuesCUTOVER <- filter(cutoverData, responseTypeCutover == "observed")
cutoverData_bars <- filter(cutoverData, sampleDensityCutover %in% c("low sample density",
                                                                    "high sample density", 
                                                                    "centroid",
                                                                    "klementQGIS"))
sampleTypeCutover <- factor(cutoverData_bars$sampleDensityCutover,
                            levels = c("centroid",
                                       "low sample density",
                                       "high sample density",
                                       "klementQGIS"))

# Create cutover bar plot
cutoverPlot <- ggplot(cutoverData_bars, aes(sampleTypeCutover, responseCutover, 
                                            fill = responseTypeCutover, na.rm = TRUE))+ 
  geom_col(position = "dodge", width = 0.75)+
  geom_hline(aes(yintercept = responseCutover), 
             observed_valuesCUTOVER, color = "black", linetype = "dashed")+
  facet_wrap(~factor(metricCutover, levels=c("road density", "road presence", "distance to road","forestry disturbance footprint","road disturbance footprint")), ncol = 1, scales = "free")+
  ggtitle("Cutover")+
  theme_classic()+
  scale_fill_manual(values = c("#D55E00", "#F0E442", "#0072B2", "#009E73"))+
  theme(text = element_text(size = 15), axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position = "none")

cutoverPlot
overallPlot
# Organize into one panel for paper
grid.arrange(cutoverPlot, overallPlot, ncol=2)


