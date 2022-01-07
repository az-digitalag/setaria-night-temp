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
# 2 energy/water fluxes (Evap, Transp) [kg m^-2 s^-1]
# (Evap = ET, so Transp/Evap is E/ET)


treatments <- c("rn", "hn", "hnrn")
variables <- c("GPP", "NPP", "AutoResp",
               "TotLivBiom", "AGB", "LAI", 
               "Evap", "Transp")


# Functions for conversion of biomass and transpiration units
convert_units <- function(x, variable) {
  if(variable %in% c("GPP", "NPP", "AutoResp")) {
    # Convert to kg C /m2/h
    return(ud.convert(x, "kg/m2/s", "kg/m2/h"))
  } else if(variable %in% c("TotLivBiom", "AGB")) {
    # undo biomass to C conversion in PEcAn, kg biomass /m2
    return(x / 0.4)
  } else if(variable %in% c("Evap", 
                             "Transp")) {
    # Convert to kg/m2/h
    return(ud.convert(x, "kg/m2/s", "kg/m2/h"))
  } else if(variable == "LAI") { # no conversion needed for LAI
    return(x)
  }
}

for(trt in treatments){
  
  # Load in daily summary of median biocro output, to obtain correct timestamps
  load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
              "/out/SA-median/biocro_output.RData"))
  timescale <- data.frame(day = rep(biocro_result$doy, each = 24), hour = 0:23)
  rm(biocro_result)
  
  for(v in variables){
    
    # Load in wide format of ensemble outputs
    load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                "/ensemble.ts.NOENSEMBLEID.", v, ".2019.2019.Rdata"))
    
    # Rearrange to long format and summarize across ensembles
    if (v %in% c("TotLivBiom", "AGB", "LAI")) { # Pools or ratios, take midday values only
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
                        "Evap", "Transp")) { # Fluxes, take daily sums first
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

# Calculate additional derived variables
# CUE = NPP/GPP
# WUE = NPP/Transp
# TET = Transp/Evap

dvars <- c("CUE", "WUE", "TET")

for(trt in treatments){
  
  # Load in daily summary of median biocro output, to obtain correct timestamps
  load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
              "/out/SA-median/biocro_output.RData"))
  timescale <- data.frame(day = rep(biocro_result$doy, each = 24), hour = 0:23)
  rm(biocro_result)
  
  for(d in dvars){
    if (d == "CUE")) {
    # Load in wide format of ensemble outputs
      load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                "/ensemble.ts.NOENSEMBLEID.NPP.2019.2019.Rdata"))
      npp <- ensemble.ts
      
      load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                  "/ensemble.ts.NOENSEMBLEID.GPP.2019.2019.Rdata"))
      gpp <- ensemble.ts
      
      cue <- data.frame(timescale, t(npp[["NPP"]]/gpp[["GPP"]]))%>% 
        pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                     names_prefix = "X", values_to = "output") %>% 
        filter(hour == 12) %>% 
        group_by(day) %>% 
        summarise(mean = mean(output, na.rm = TRUE), 
                  median = median(output, na.rm = TRUE), 
                  sd = sd(output, na.rm = TRUE), 
                  lcl_50 = quantile(output, probs = c(0.25), na.rm = TRUE), 
                  ucl_50 = quantile(output, probs = c(0.75), na.rm = TRUE),
                  lcl_95 = quantile(output, probs = c(0.025), na.rm = TRUE), 
                  ucl_95 = quantile(output, probs = c(0.975), na.rm = TRUE))
      
      write.csv(cue, 
                paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                       "/ensemble_ts_summary_", d, ".csv"),
                row.names = F)
    } else if(d == "WUE") {
      load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                  "/ensemble.ts.NOENSEMBLEID.Transp.2019.2019.Rdata"))
      transp <- ensemble.ts
      
      wue <- data.frame(timescale, t(npp[["NPP"]]/transp[["Transp"]]))%>% 
        pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                     names_prefix = "X", values_to = "output") %>% 
        filter(hour == 12) %>% 
        group_by(day) %>% 
        summarise(mean = mean(output, na.rm = TRUE), 
                  median = median(output, na.rm = TRUE), 
                  sd = sd(output, na.rm = TRUE), 
                  lcl_50 = quantile(output, probs = c(0.25), na.rm = TRUE), 
                  ucl_50 = quantile(output, probs = c(0.75), na.rm = TRUE),
                  lcl_95 = quantile(output, probs = c(0.025), na.rm = TRUE), 
                  ucl_95 = quantile(output, probs = c(0.975), na.rm = TRUE))
      
      write.csv(wue, 
                paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                       "/ensemble_ts_summary_", d, ".csv"),
                row.names = F)
    } else if(d == "TET") {
      load(paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                  "/ensemble.ts.NOENSEMBLEID.Evap.2019.2019.Rdata"))
      evap <- ensemble.ts
      
      tet <- data.frame(timescale, t(transp[["Transp"]]/evap[["Evap"]]))%>% 
        pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                     names_prefix = "X", values_to = "output") %>% 
        filter(hour == 12) %>% 
        group_by(day) %>% 
        summarise(mean = mean(output, na.rm = TRUE), 
                  median = median(output, na.rm = TRUE), 
                  sd = sd(output, na.rm = TRUE), 
                  lcl_50 = quantile(output, probs = c(0.25), na.rm = TRUE), 
                  ucl_50 = quantile(output, probs = c(0.75), na.rm = TRUE),
                  lcl_95 = quantile(output, probs = c(0.025), na.rm = TRUE), 
                  ucl_95 = quantile(output, probs = c(0.975), na.rm = TRUE))
      
      write.csv(tet, 
                paste0("/data/output/pecan_runs/temp_comp_ms/", trt, 
                       "/ensemble_ts_summary_", d, ".csv"),
                row.names = F)
    }
    
    rm(ensemble.ts)
    gc()
  }
}

# Summarize pair-wise differences across treatments, by ensemble
# hn_rn = high night - regular night, effect of different params and temps
# hnrn_rn = high night control temp - regular night, effect of high temp alone
# hn_hnrn = high night - high night control temp, effect of high temp params alone

for(v in variables){
  
  # Load in daily summary of median biocro output, to obtain correct timestamps
  load(paste0("/data/output/pecan_runs/temp_comp_ms/rn/out/SA-median/biocro_output.RData"))
  timescale <- data.frame(day = rep(biocro_result$doy, each = 24), hour = 0:23)
  rm(biocro_result)
  
  # Load all 3 treatments, summarize to daily depending on variable
  load(paste0("/data/output/pecan_runs/temp_comp_ms/rn/ensemble.ts.NOENSEMBLEID.", 
              v, ".2019.2019.Rdata"))
  if (v %in% c("TotLivBiom", "AGB", "LAI")) {
    rn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>% 
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      filter(hour == 12) %>% 
      group_by(day, ensemble) %>%
      dplyr::select(-hour)
  } else if (v %in% c("GPP", "NPP", "AutoResp", 
                      "Evap", "Transp")) {
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
  if (v %in% c("TotLivBiom", "AGB", "LAI")) {
    hn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>% 
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      filter(hour == 12) %>% 
      group_by(day, ensemble)%>%
      dplyr::select(-hour)
  } else if (v %in% c("GPP", "NPP", "AutoResp", 
                      "Evap", "Transp")) {
    hn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>%
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      group_by(day, ensemble) %>%
      summarise(output = sum(output))
  }
  
  load(paste0("/data/output/pecan_runs/temp_comp_ms/hnrn/ensemble.ts.NOENSEMBLEID.", 
              v, ".2019.2019.Rdata"))
  if (v %in% c("TotLivBiom", "AGB", "LAI")) {
    hnrn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>% 
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      filter(hour == 12) %>% 
      group_by(day, ensemble) %>%
      dplyr::select(-hour)
  } else if (v %in% c("GPP", "NPP", "AutoResp", 
                      "Evap", "Transp")) {
    hnrn <- data.frame(timescale, t(ensemble.ts[[v]])) %>%
      pivot_longer(cols = starts_with("X"), names_to = "ensemble",
                   names_prefix = "X", values_to = "output") %>%
      mutate(output = convert_units(output, variable = v),
             ensemble = as.numeric(ensemble)) %>%
      group_by(day, ensemble) %>%
      summarise(output = sum(output))
  }
  rm(ensemble.ts)
  
  # Combine to single df and calculate differences across treatments
  all <- bind_cols(rn, hn["output"], hnrn["output"]) %>%
    rename(rn = 3,
           hn = 4,
           hnrn = 5) %>%
    # One-sided t-tests predictions
    mutate(hn_rn = hn - rn,
           hnrn_rn = hnrn - rn,
           hn_hnrn = hn - hnrn)
  
  # Summarize whether across ensembles, the differences are significant each day
  diff_stat <- all %>%
    dplyr::select(-rn, -hn, -hnrn) %>%
    group_by(day) %>%
    # Reports the 2.5, 5, 50, 95, and 97.5th percentile of each set of differences
    summarize(hn_rn = quantile(hn_rn, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)),
              hnrn_rn = quantile(hnrn_rn, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)),
              hn_hnrn = quantile(hn_hnrn, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))) %>%
    mutate(percentile = c("025", "050", "250", "500", "750", "950", "975")) %>%
    relocate(day, percentile)
  
  write.csv(diff_stat, 
            paste0("/data/output/pecan_runs/temp_comp_ms/comparison_diff_", v, ".csv"),
            row.names = F)
}

# Collate variance decomposition variables across treatments

for(v in variables) {
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

