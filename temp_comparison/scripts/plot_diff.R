# Load required libraries
# ----------------------------------------------------------------------
library(readxl)
library(udunits2)
library(dplyr)
library(tidyr)
library(ggplot2)

if(!dir.exists(paste0("../plots/"))){
  dir.create(paste0("../plots/"), recursive = T)
}

variables <- c("NPP", # C flux, kg C /m2/h
               "TVeg", "E",  # flux, kg/m2/h
               "TotLivBiom", "AGB", # biomass, kg biomass /m2
               "LAI", "WUE", "TET") # ratios, unitless

diff_list <- list()
for (v in variables) {
  data_in <- read.csv(paste0("/data/output/pecan_runs/temp_comp_ms/comparison_diff_", v, ".csv")) %>%
    pivot_wider(names_from = percentile, values_from = c(hn_rn, hnrn_rn, hn_hnrn)) %>%
    pivot_longer(-day, names_to = c("diff", "percentile"), 
                 names_pattern = "(.*)_(.*)") %>%
    pivot_wider(names_from = "percentile") %>%
    mutate(trait = v) %>%
    rename(p025 = "25",
           p050 = "50",
           p250 = "250",
           p500 = "500",
           p750 = "750",
           p095 = "950",
           p975 = "975") %>%
    mutate(sig_05 = ifelse(p095 < 0 | p050 > 0, TRUE, NA))
  
  diff_list[[v]] <- data_in
}


# Create complete dataframe for plotting; add labels
diff_df <- do.call(rbind.data.frame, diff_list) %>%
  mutate(label = case_when(trait == "NPP" ~ "Delta*NPP",
                           trait == "TotLivBiom" ~ "Delta*TotBiomass",
                           trait == "AGB" ~ "Delta*AGB",
                           trait == "LAI" ~ "Delta*LAI",
                           trait == "E" ~ "Delta*E",
                           trait == "TVeg" ~ "Delta*T",
                           trait == "WUE" ~ "Delta*WUE",
                           trait == "TET" ~ "Delta*T:ET"),
         type = case_when(diff == "hn_hnrn" ~ "Adaptation~effect",
                          diff == "hnrn_rn" ~ "Temp~effect",
                          diff == "hn_rn" ~ "Combined~effect"))
diff_df$label <- factor(diff_df$label, levels = unique(diff_df$label))
diff_df$type <- factor(diff_df$type, levels = c("Temp~effect", 
                                                "Adaptation~effect", 
                                                "Combined~effect"))
diff_df$diff <- factor(diff_df$diff, levels = c("hnrn_rn", "hn_hnrn", "hn_rn"))
# Plot panel of figures
fig_diff <- ggplot(diff_df, aes(x = day)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = p500, color = diff)) +
  geom_ribbon(aes(ymin = p050, ymax = p095, fill = diff), alpha = 0.25) +
  geom_point(aes(y = sig_05*p500, color =diff)) +
  facet_grid(cols = vars(type),
             rows = vars(label),
             labeller = label_parsed,
             scales = "free_y",
             switch = "y") +
  scale_x_continuous("Day of Experiment") + 
  scale_color_manual(values = c("red", "blue", "darkorchid"), 
                     labels = c("hnrn - rn", "hn - hnrn", "hn - rn")) +
  scale_fill_manual(values = c("red", "blue", "darkorchid"), 
                    labels = c("hnrn - rn", "hn - hnrn", "hn - rn")) +
  theme_bw(base_size = 8) +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank())
fig_diff

ggsave(filename = "all_diff.jpg", 
       plot = fig_diff, 
       path = "temp_comparison/plots",
       height = 6, 
       width = 6, 
       units = "in", 
       dpi = 300)

