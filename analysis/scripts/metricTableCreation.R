##### Metric table bar plots
library(ggplot2)
library(tidyverse)
library(ggpubr)

data_path_drvd <- "analysis/data/derived_data/"

fig_path <- "analysis/figures/"

fig_dev <- "jpeg"

# data (from "meanTable") in the runProjections script
meanTable <- read.csv(paste0(data_path_drvd, "mean_table_1e-06.csv"))

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
                      scale_fill_manual(values = c("#D55E00", "#F0E442", "#0072B2", "#009E73"))+
                      theme(text = element_text(size = 15), axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            legend.position = "none")+
                      ggtitle(str_to_title(areaMean)))) %>%
  pull(plot) %>%
  {ggarrange(plotlist = .)}

ggsave(paste0(fig_path, "Figure3_road_metrics.", fig_dev), fig3,
       width = 7.5, height = 9)



