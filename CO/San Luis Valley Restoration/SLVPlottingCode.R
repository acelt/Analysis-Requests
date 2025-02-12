library(tidyverse)
library(RColorBrewer)
library(cowplot)

slvindicators <- indicators%>%
  st_filter(., folayer%>%dplyr::filter(FieldOffice == "SAN LUIS VALLEY FIELD OFFICE"))%>%
  mutate(BenchmarkGroup = factor(case_when(HGMClass!="Riverine" ~ "Herbaceous Slope Wetland",
                                           HGMClass == "Riverine" & WetlandType == "Riparian Shrubland" ~ "Riparian Shrubland",
                                           TRUE ~ "Herbaceous Riparian"),
                                 levels = c("Riparian Shrubland", "Herbaceous Riparian", "Herbaceous Slope Wetland")))

slvsppindicators <-sppindicators%>%
  st_filter(., folayer%>%dplyr::filter(FieldOffice == "SAN LUIS VALLEY FIELD OFFICE"))

lhs_indicators <- c("SppInv_Richness_Cnt", "AH_NativeCover", "AH_NonnativeCover", "AH_NoxiousCover", "AH_HydrophyteCover", "AH_HydroFACCover", "AH_AnnualCover", "AH_PerennialCover", "AH_GraminoidCover", "AH_ForbCover", "AH_ShrubCover", "AH_TreeCover", "AH_WaterCover", "BareSoilCover", "Hgt_Water_Avg", "SppInv_CValue_Avg")

slvmeans <- slvindicators%>%
  st_drop_geometry()%>%
  group_by(BenchmarkGroup)%>%
  summarize_at(.vars = lhs_indicators, .funs = ~mean(., na.rm = T))

slvplots <- list()
for (indicator in 1:length(lhs_indicators)){
  p <- ggplot(slvindicators, aes(y = .data[[lhs_indicators[indicator]]], x = BenchmarkGroup)) +
    geom_point(data = slvmeans, aes(color = BenchmarkGroup), size = 3) +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, size = 10, hjust = 1), axis.title.x = element_blank()) +
    expand_limits(y = 0) +
    scale_color_brewer(palette = "Dark2",)

  slvplots[[indicator]] <- p

  ggsave(filename = paste(lhs_indicators[indicator], "_ByBenchmarkGroup.jpg", sep = ""), path = "./Plots/SanLuisValley", plot = p, unit = "in", scale = 1.5, width = 4, height = 3)
}

slv_plots_1 <- plot_grid(plot_grid(slvplots[[1]] + theme(legend.position = "none"),
                                   slvplots[[2]] + theme(legend.position = "none"),
                                   slvplots[[3]] + theme(legend.position = "none"),
                                   slvplots[[4]] + theme(legend.position = "none")),
                         get_legend(slvplots[[1]]+theme(legend.position = "bottom")),
                         nrow = 2, rel_heights = c(0.9,0.1))
ggsave(filename = paste("RichnessNativity_groupplot", "_ByBenchmarkGroup.jpg", sep = ""), path = "./Plots/SanLuisValley", plot = slv_plots_1, units = "in", width = 7, height = 8, scale = 1.2)
