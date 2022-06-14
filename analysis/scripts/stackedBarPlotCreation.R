# Creating stacked bar plots for matching roaded and roadless

# data
data_path_drvd <- "analysis/data/derived_data/"

library(tidyverse)

# load csv
matchData <- read.csv(paste0(data_path_drvd, "agree_table_1e-06.csv"))

matchData <- matchData %>% ungroup() %>%
  mutate(agreement = factor(agreement,
                            levels = c("False negative", "False positive",
                                       "Agree roaded","Agree roadless",
                                       "Pre-existing roads")),
         sampleDens = paste(sampleType, sampleDens) %>%
           str_replace("1e-06", "low density") %>%
           str_replace("1e-04", "high density") %>%
           str_replace("centroid low density", "centroid") %>%
           str_replace("klementQGIS NA", "klementQGIS") %>%
           factor(levels = c("centroid",
                             "random low density",
                             "random high density",
                             "regular low density",
                             "regular high density",
                             "klementQGIS"),
                  labels = c("Centroid", "Random low density", "Random high density",
                             "Regular low density",
                             "Regular high density",
                             "Klement QGIS")),
         metric = factor(metric,
                         levels = c("roadPresence",
                                    "forestryDisturbance",
                                    "roadDisturbance"),
                         labels = c("Road presence",
                                    "Forestry disturbance footprint",
                                    "Road disturbance footprint"))) %>%
  group_by(metric, sampleDens, agreement) %>%
  summarise(count = sum(count), .groups = "drop_last") %>%
  mutate(perc = count/sum(count)*100)

stackedBar <- ggplot(matchData, aes(x = sampleDens, y =perc, fill = agreement))+
  geom_col(position = "stack", width = 0.75)+
  theme_classic()+
  theme(axis.text.x=element_text(color = "black", size=11, angle=50, vjust=1, hjust=1))+
  scale_fill_manual(values = c("#D55E00", "#F0E442", "#0072B2", "#009E73", "#999999"))+
  theme(text = element_text(size = 15), axis.title.x = element_blank())+
  theme(legend.position="none")+
  labs(y = "Percent of Landscape")+
  facet_wrap(~metric)


stackedBar_1km



### load csv for plot(road presence)

matchData_roadPresence <- read.csv("data/revelstoke/nonAggregated/results/roadPresenceMatch.csv")

Match_roadPresence <- factor(matchData_roadPresence$MatchType,
                levels = c("Project roadless & observe roaded",
                           "Project roaded & observe roadless",
                           "Agree roaded after 1990",
                           "Agree roadless",
                           "Roaded prior to 1990"))

stackedBar_roadPresence <- ggplot(matchData_roadPresence, aes(x = factor(Projection.Method, levels = c("Centroid", "Random Low Density", "Regular Low Density", "Random High Density", "Regular High Density", "klementQGIS")), Percent, fill = Match_roadPresence))+
  geom_col(position = "stack", width = 0.75)+
  theme_classic()+
  theme(axis.text.x=element_text(color = "black", size=11, angle=50, vjust=1, hjust=1))+
  scale_fill_manual(values = c("#D55E00", "#F0E442", "#0072B2", "#009E73", "#999999"))+
  theme(text = element_text(size = 15), axis.title.x = element_blank())+
  theme(legend.position = "none")+
  theme(axis.title.y = element_blank())+
  ggtitle("Road presence")+
  theme(plot.title = element_text(size = 16))


stackedBar_roadPresence



### load csv for plot(Forest disturbance footprint)

matchData_caribou <- read.csv("data/revelstoke/nonAggregated/results/forestryFootprint_matching.csv")

Match_caribou <- factor(matchData_caribou$MatchType,
                levels = c("Project roadless & observe roaded",
                           "Project roaded & observe roadless",
                           "Agree roaded after 1990",
                           "Agree roadless",
                           "Roaded prior to 1990"))

stackedBar_forestryFootprint <- ggplot(matchData_caribou, aes(x = factor(Projection.Method, levels = c("Centroid", "Random Low Density", "Regular Low Density", "Random High Density", "Regular High Density", "klementQGIS")), Percent, fill = Match_caribou))+
  geom_col(position = "stack", width = 0.75)+
  theme_classic()+
  theme(axis.text.x=element_text(color = "black", size=11, angle=50, vjust=1, hjust=1))+
  scale_fill_manual(values = c("#D55E00", "#F0E442", "#0072B2", "#009E73", "#999999"))+
  theme(text = element_text(size = 15), axis.title.x = element_blank())+
  theme(legend.position = "none")+
  theme(axis.title.y = element_blank())+
  ggtitle("Forestry disturbance footprint")+
  theme(plot.title = element_text(size = 16))


stackedBar_forestryFootprint


grid.arrange(stackedBar_1km,stackedBar_forestryFootprint,stackedBar_roadPresence, nrow = 1)
