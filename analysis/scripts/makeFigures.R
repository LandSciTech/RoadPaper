##### Metric table bar plots
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggpubr)
library(MetBrewer)

data_path_drvd <- "analysis/data/derived_data/"

fig_path <- "analysis/figures/"

fig_dev <- "jpeg"

pal_nm <- "Egypt"


# Figure 3 #====================================================================

# data (from "meanTable") in the runProjections script
meanTable <- read.csv(paste0(data_path_drvd, "mean_table.csv"))

# organize data for ggplot
meanTable_long <- meanTable %>%
  pivot_longer(-c(sampleType, sampleDens, areaMean),
               names_to = "metric", values_to = "response")%>%
  mutate(sampleDens = factor(sampleDens,
                             levels = c("centroid",
                                        "low sample density",
                                        "high sample density",
                                        "klementQGIS"),
                             labels = c("Centroid", "Low\ndensity", "High\ndensity",
                                        "Klement\nQGIS")),
         metric = factor(metric,
                         levels = c("roadDensityMean", "roadPresenceMean",
                                    "distanceToRoadMean",
                                    "forestryDisturbanceMean",
                                    "roadDisturbanceMean"),
                         labels = c("Road density", "Road presence",
                                    "Distance to road",
                                    "Forestry disturbance footprint",
                                    "Road disturbance footprint")))



observed_values <- filter(meanTable_long, sampleType == "observed")
projected_values <- filter(meanTable_long, !is.na(sampleDens))
# Create bar plot
fig3 <- projected_values %>% group_by(areaMean) %>% nest() %>% ungroup() %>%
  mutate(obs = observed_values %>% group_by(areaMean) %>% nest() %>% pull(data)) %>%
  group_by(areaMean) %>%
  mutate(data = map(data, ~mutate(.x, area = areaMean)),
         plot = map2(data, obs,
                    ~ ggplot(.x, aes(x = sampleDens, response, fill = sampleType))+
                      geom_col(position = position_dodge2(preserve = "single",
                                                         width = 0.75))+
                      geom_hline(aes(yintercept = response),
                                 .y, color = "black", linetype = "dashed")+
                      facet_wrap(~metric, scales = "free_y", ncol = 1)+
                      theme_classic()+
                      scale_fill_manual(values = met.brewer(pal_nm, 4))+
                      theme(text = element_text(size = 15), axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            legend.position = "none")+
                      ggtitle(str_to_title(areaMean)))) %>%
  pull(plot) %>%
  {ggarrange(plotlist = .)}

fig3

ggsave(paste0(fig_path, "Figure3_road_metrics.", fig_dev), fig3,
       width = 7.5, height = 9)

# Figure 4 #====================================================================

# load csv
matchData <- read.csv(paste0(data_path_drvd, "agree_table.csv"))

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
  scale_fill_manual(values = c(met.brewer(pal_nm, 4, direction = -1)[c(1,4,3,2)],
                               "#999999"))+
  theme(text = element_text(size = 15), axis.title.x = element_blank())+
  theme(legend.position="bottome", legend.direction = "horizontal")+
  labs(y = "Percent of Landscape")+
  facet_wrap(~metric)


stackedBar

ggsave(paste0(fig_path, "Figure4_agree_metrics.", fig_dev), stackedBar,
       width = 7.5, height = 6)

