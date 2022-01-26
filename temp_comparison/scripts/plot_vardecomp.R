# Variance decomposition plots for high night temperature biomass results. 

library(ggplot2)
library(dplyr)
library(cowplot)

if(!dir.exists(paste0("../plots/"))){
  dir.create(paste0("../plots/"), recursive = T)
}

variables <- c("NPP", # C flux, kg C /m2/h
               "TotLivBiom", "AGB", # biomass, kg biomass /m2
               "Evap", "TVeg")  # flux, kg/m2/h


# Plot variance decomposition of biomass
vd_list <- list()
for(v in variables) {
  load(paste0("/data/output/pecan_runs/temp_comp_ms/var_decomp_", v, ".Rdata"))
  tot_sd <- vd %>%
    group_by(treatment) %>%
    summarize(tot_sd_convert = sum(sd_convert))
  data_in <- vd %>%
    left_join(tot_sd) %>%
    mutate(sd_percent = sd_convert / tot_sd_convert * 100,
           variable = v)
           
  vd_list[[v]] <- data_in
}

# Combine to dataframe for plotting
vd_df <- do.call(rbind.data.frame, vd_list)
vd_df$variable <- factor(vd_df$variable, levels = variables)
vd_df$treatment <- factor(vd_df$treatment, levels = c("rn", 
                                                      "hnrn", 
                                                      "hn"))

cv <- ggplot(data = filter(vd_df, variable == "NPP")) +
  geom_pointrange(aes(x = trait.labels, y = coef.vars, ymin = 0, ymax = coef.vars, 
                      col = treatment), 
                  alpha = 0.5, size = 0.75, position = position_dodge(width = c(-0.4))) +
  coord_flip() +
  ggtitle("CV %") +
  scale_color_manual(values = c("cornflowerblue", 
                                "burlywood", 
                                "coral")) +
  geom_hline(aes(yintercept = 0), size = 0.1) +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        legend.position = c(0.9, 0.8),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_blank())


el <- ggplot(data = vd_df) +
  geom_pointrange(aes(x = trait.labels, y = elasticities, ymin = 0, ymax = elasticities, 
                      col = treatment), 
                  alpha = 0.5, size = 0.75, position = position_dodge(width = c(-0.4))) +
  coord_flip() +
  facet_wrap(~variable,
             ncol = 1,
             strip.position = "left") +
  ggtitle("Elasticity") +
  scale_color_manual(values = c("cornflowerblue", 
                                "burlywood", 
                                "coral")) +
  geom_hline(aes(yintercept = 0), size = 0.1)  +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.placement = "outside") +
  guides(col = FALSE)

vdecomp <- ggplot(data = vd_df) +
  geom_pointrange(aes(x = trait.labels, y = sd_percent, ymin = 0, ymax = sd_percent, 
                      col = treatment), 
                  alpha = 0.5, size = 0.75, position = position_dodge(width = c(-0.4))) +
  coord_flip() +
  facet_wrap(~variable,
             ncol = 1,
             strip.position = "left") +
  ggtitle("% SD Explained") +
  scale_color_manual(values = c("cornflowerblue", 
                                "burlywood", 
                                "coral")) +
  geom_hline(aes(yintercept = 0), size = 0.1)  +
  scale_y_continuous(breaks = pretty(vd_df$sd_percent, n = 3)) +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        strip.text.y = element_blank()) +
  guides(color = "none")

bottom <- plot_grid(el, vdecomp, rel_widths = c(1.7,1))
all <- plot_grid(cv, bottom, ncol = 1, rel_heights = c(0.75, 3))

ggsave(filename = "all_vd.jpg", 
       plot = all, 
       path = "temp_comparison/plots",
       height = 8, 
       width = 6, 
       units = "in", 
       dpi = 300)
