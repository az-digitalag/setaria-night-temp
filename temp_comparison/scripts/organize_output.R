# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------
library(readxl)
library(udunits2)
library(dplyr)
library(tidyr)

# Summarize (mn, md, sd, CI_50, and CI_95) across ensembles for all 3 treatments
# And 9 variables
# 3 C fluxes (GPP, NPP, AutoResp) [kg C m^-2 s^-1]
# 3 C pools (TotLivBiom, AGB, LAI) [kg C m^-2 or m^2 m^-2]
# 3 energy/water fluxes (stomatal_conductance, Evap, Transp) [kg m^-2 s^-1]
# (Is Transp == TVeg?)

treatments <- c("rn", "hn", "hnrn")

# Separate lists of ensembles variables vs. sensitivity variables
ens.variables <- c("GPP", "NPP", "AutoResp",
                   "TotLivBiom", "AGB", "LAI", 
                   "stomatal_conductance", "Evap", "Transp")
sens.variables <- c("AGB", "TVeg")

# Functions for conversion of biomass and transpiration units
convert_units <- function(x, variable) {
  if (variable %in% c("GPP", "NPP", "AutoResp")) {
    # Convert to kg/m2/h
    return(ud.convert(x, "kg/m2/s", "kg/m2/h"))
  } else if (variable %in% c("TotLivBiom", 
                             "AGB")) {
    # undo  biomass to C conversion in PEcAn, already converted to kg/m2
    return(x / 0.4)
  } else if (variable %in% c("stomatal_conductance", 
                             "Evap", 
                             "TVeg",
                             "Transp")) {
    # Convert to kg/m2/h
    return(ud.convert(x, "kg/m2/s", "kg/m2/h"))
  }
}

for(trt in treatments){
  
  # Load in daily summary of median biocro output, to obtain correct timestamps
  load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
              "/out/SA-median/biocro_output.RData"))
  timescale <- data.frame(day = rep(biocro_result$doy, each = 24), hour = 0:23)
  rm(biocro_result)
  
  for(v in ens.variables){
    
    # Load in wide format of ensemble outputs
    load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                "/ensemble.ts.NOENSEMBLEID.", v, ".2019.2019.Rdata"))
    
    # Rearrange to long format and summarize across ensembles
    if (v %in% c("TotLivBiom", "AGB", "LAI")) { # Pools, take midday values only
      daily <- data.frame(timescale, t(ensemble.ts[[v]])) %>% 
        pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                     names_prefix = "X", values_to = "output") %>% 
        mutate(output = convert_units(output, variable = v)) %>% 
        filter(hour == 12) %>% 
        group_by(day) %>% 
        summarise(mean = mean(output, na.rm = TRUE), 
                  median = median(output, na.rm = TRUE), 
                  sd = sd(output, na.rm = TRUE), 
                  lcl_50 = quantile(output, probs = c(0.25), na.rm = TRUE), 
                  ucl_50 = quantile(output, probs = c(0.75), na.rm = TRUE),
                  lcl_95 = quantile(output, probs = c(0.025), na.rm = TRUE), 
                  ucl_95 = quantile(output, probs = c(0.975), na.rm = TRUE))
    } else if (v %in% c("GPP", "NPP", "AutoResp", 
                        "stomatal_conductance", "Evap", "Transp")) { # Fluxes, take daily sums first
      daily <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
        pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                     names_prefix = "X", values_to = "output") %>%
        mutate(output = convert_units(output, variable = v),
               ensemble = as.numeric(ensemble)) %>%
        group_by(day, ensemble) %>%
        summarise(output = sum(output)) %>%
        group_by(day) %>%
        summarise(mean = mean(output, na.rm = TRUE), 
                  median = median(output, na.rm = TRUE), 
                  sd = sd(output, na.rm = TRUE), 
                  lcl_50 = quantile(output, probs = c(0.25), na.rm = TRUE), 
                  ucl_50 = quantile(output, probs = c(0.75), na.rm = TRUE),
                  lcl_95 = quantile(output, probs = c(0.025), na.rm = TRUE), 
                  ucl_95 = quantile(output, probs = c(0.975), na.rm = TRUE))
    }
    
    write.csv(daily, 
              paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                     "/ensemble_ts_summary_", v, ".csv"),
              row.names = F)
    rm(ensemble.ts)
  }
}


# Summarize pair-wise differences across treatments, by ensemble

for(v in ens.variables){
  
  # Load in daily summary of median biocro output, to obtain correct timestamps
  load(paste0("/data/output/pecan_runs/temp_comp_ms/rn/out/SA-median/biocro_output.RData"))
  timescale <- data.frame(day = rep(biocro_result$doy, each = 24), hour = 0:23)
  rm(biocro_result)
  
  # Load all 3 treatments, summarize to daily depending on variable
  load(paste0("/data/output/pecan_runs/temp_comp_ms/rn/ensemble.ts.NOENSEMBLEID.", 
              v, ".2019.2019.Rdata"))
  if (v %in% c("TotLivBiom", "AGB")) {
    rn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>% 
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      filter(hour == 12) %>% 
      group_by(day, ensemble) %>%
      dplyr::select(-hour)
  } else if (v == "TVeg") {
    rn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>%
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      group_by(day, ensemble) %>%
      summarise(output = sum(output))
  }
  rm(ensemble.ts)
  
  load(paste0("/data/output/pecan_runs/temp_comp_ms/hn/ensemble.ts.NOENSEMBLEID.", 
              v, ".2019.2019.Rdata"))
  if (v %in% c("TotLivBiom", "AGB")) {
    hn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>% 
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      filter(hour == 12) %>% 
      group_by(day, ensemble)%>%
      dplyr::select(-hour)
  } else if (v == "TVeg") {
    hn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>%
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      group_by(day, ensemble) %>%
      summarise(output = sum(output))
  }
  
  # Combine to single df and calculate differences across treatments
  all <- bind_cols(rn, hn["output"]) %>%
    rename(rn = 3,
           hn = 4) %>%
    # One-sided t-tests predictions
    mutate(hn_rn = hn - rn)
  
  # Summarize whether across ensembles, the differences are significant each day
  diff_stat <- all %>%
    dplyr::select(-rn, -hn) %>%
    group_by(day) %>%
    # Reports the 2.5, 5, 50, 95, and 97.5th percentile of each set of differences
    summarize(hn_rn = quantile(hn_rn, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))) %>%
    mutate(percentile = c("025", "050", "250", "500", "750", "950", "975")) %>%
    relocate(day, percentile)
  
  write.csv(diff_stat, 
            paste0("/data/output/pecan_runs/temp_comp_ms/comparison_diff_", v, ".csv"),
            row.names = F)
}

# Collate variance decomposition variables across treatments

for(v in sens.variables) {
  vd <- c()
  for(trt in treatments) {
    load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                "/sensitivity.results.NOENSEMBLEID.", v, ".2019.2019.Rdata"))
    pft <- names(sensitivity.results)
    v_df1 <- sensitivity.results[[pft]]$variance.decomposition.output
    v_df2 <- data.frame(trait = names(v_df1$coef.vars), data.frame(v_df1))
    v_df3 <- v_df2 %>% 
      mutate(trait.labels = factor(as.character(PEcAn.utils::trait.lookup(trait)$figid)),
             units = PEcAn.utils::trait.lookup(trait)$units, 
             coef.vars = coef.vars * 100, 
             sd = sqrt(variances),
             sd_convert = convert_units(sd, variable = v),
             treatment = trt) %>%
      relocate(treatment)
    rm(sensitivity.results)
    
    vd <- rbind.data.frame(vd, v_df3)
    
  }
  save(vd, file = paste0("/data/output/pecan_runs/temp_comp_ms/var_decomp_", v, ".Rdata"))
}

