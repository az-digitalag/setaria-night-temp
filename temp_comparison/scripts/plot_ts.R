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

# Organize 3 treatments into same figure
treatments <- c("rn", "hn", "hnrn")
variables <- c("NPP", # C flux, kg C /m2/h
               "Evap", "TVeg",  # flux, kg/m2/h
               "TotLivBiom", "AGB", # biomass, kg biomass /m2
               "LAI", "WUE", "TET") # ratios, unitless

ts_list <- list()
for (v in variables) {
  ts_df <- c()
  for(trt in treatments) {
    data_in <- read.csv(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                               "/ensemble_ts_summary_", v, ".csv")) %>%
      mutate(treatment = trt) %>%
      relocate(treatment)
    ts_df <- rbind(ts_df, data_in)
  }
  ts_list[[v]] <- ts_df
}

# Bring in validation biomass
load("~/sentinel-detection/data/cleaned_data/biomass/chamber_biomass.Rdata")

biomass_valid <- chamber_biomass %>%
  filter(genotype == "ME034V-1" &
           treatment %in% c(NA, "control") &
           temp %in% c("31/22", "31/31") &
           light == 250 &
           !is.na(stem_DW_mg) &
           !is.na(leaf_DW_mg) &
           !is.na(panicle_DW_mg)) %>%
  rename(plant_id = plantID) %>%
  mutate(location = case_when(temp == "31/22" ~ "rn", 
                              temp == "31/31" ~ "hn"),
         stem_DW_g = ud.convert(stem_DW_mg, "mg", "g"),
         leaf_DW_g = ud.convert(leaf_DW_mg, "mg", "g"),
         panicle_DW_g = ud.convert(panicle_DW_mg, "mg", "g")) %>%
  # select closest dates to validation data, which were sown 2019-01-03 in GCH158
  filter(sowing_date >= as.POSIXct("2018-11-01") & sowing_date <= as.POSIXct("2019-03-01")) %>%
  dplyr::select(location, plant_id, sowing_date, transplant_date, harvest_date,
                stem_DW_g, leaf_DW_g, panicle_DW_g) %>%
  rename(treatment = location) %>%
  mutate(agb_kg_m2 = ud.convert((stem_DW_g + leaf_DW_g + panicle_DW_g)/103, "g/cm2", "kg/m2"),
         day = difftime(harvest_date, sowing_date, units = "days")) 

# Plot measured biomass against validation biomass estimates
fig_biomass_ts <- ggplot() +
  geom_line(data = ts_list[["AGB"]], aes(day, y = median, color = treatment)) +
  geom_ribbon(data = ts_list[["AGB"]], aes(day, ymin = lcl_50, ymax = ucl_50, fill = treatment), 
              alpha = 0.25) +
  geom_point(data = biomass_valid, aes(day, y = agb_kg_m2, color = treatment)) +
  scale_x_continuous("Day of experiment") + 
  scale_y_continuous(expression(paste("Abovground biomass (kg ",  m^-2, ")"))) +
  theme_classic()

ggsave(filename = "biomass_ts.jpg", 
       plot = fig_biomass_ts, 
       path = "temp_comparison/plots",
       height = 5, 
       width = 7, 
       units = "in", 
       dpi = 300)

### Plot all 8 variables and 3 treatments together
ts_all <- do.call(rbind, ts_list) %>%
  mutate(treatment = factor(treatment, levels = treatments),
         trait = rep(variables, each = 240),
         trait = factor(trait, levels = variables), 
         label = case_when(trait == "NPP" ~ "NPP~(kg~C~m^-2~d^-1)",
                           trait == "TotLivBiom" ~ "TotBiomass~(kg~m^-2)",
                           trait == "AGB" ~ "AGB~(kg~m^-2)",
                           trait == "LAI" ~ "LAI~(m^2~m^-2)",
                           trait == "Evap" ~ "ET~(kg~m^-2~d^-1)",
                           trait == "TVeg" ~ "T~(kg~m^-2~d^-1)",
                           trait == "WUE" ~ "WUE~(kg~C~kg^-1~H[2]*O)",
                           trait == "TET" ~ "T:ET"))
ts_all$label <- factor(ts_all$label, levels = unique(ts_all$label))

fig_all <- ggplot(ts_all) +
  geom_line(aes(x = day, y = median, color = treatment)) +
  geom_ribbon(aes(day, ymin = lcl_50, ymax = ucl_50, fill = treatment), 
              alpha = 0.25) +
  facet_wrap(~label, labeller = label_parsed, ncol = 2,
             scales = "free_y")+
  scale_x_continuous("Day of Experiment") + 
  theme_bw() +
  theme(strip.background = element_blank(),
        axis.title.y = element_blank())


ggsave(filename = "all_ts.jpg", 
       plot = fig_all, 
       path = "temp_comparison/plots",
       height = 8, 
       width = 6, 
       units = "in", 
       dpi = 300)

### Plot derived WUE and T/ET  and 3 treatments together
der_list <- list()
der_list[["WUE"]] <- ts_list[["NPP"]]