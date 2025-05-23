# Data compilation for analysis
# Katy Dynarski, March 2024

# This script contains code to make the different dataframes that will be used in downstream analyses

# 0 - Load packages and data ----
library(here)
library(DBI)
library(RSQLite)
library(zoo)
library(janitor)
library(aqp)
library(soilDB)
library(sf)
library(tidyverse)

soc_pedon <- read.csv(here("data_processed", "04_soc_stock_pedon.csv"))
soc_horizon <- read.csv(here("data_processed", "04_soc_stock_horizon.csv"))
prism <- read.csv(here("data_processed", "dsp4sh_soc_prism3.csv"))
dsp4sh6 <- dbConnect(SQLite(), here("data_raw", "dsp4sh6.db")) 

# 1 - Dataframe of project information ----
# Clean up PRISM data for joining
prism_clean <- prism %>%
  select(DSP_Pedon_ID, pedon_x, pedon_y, PRISM_ppt_30yr_normal_800mM4_annual_bil, PRISM_tmean_30yr_normal_800mM5_annual_bil) %>%
  clean_names() %>%
  distinct() %>%
  rename(map = prism_ppt_30yr_normal_800m_m4_annual_bil,
         mat = prism_tmean_30yr_normal_800m_m5_annual_bil)

# Make dataframe of project overview with climate information from PRISM
project_dat <- soc_pedon %>%
  select(dsp_pedon_id, dsp_plot_id, label_full, label, lu, till, trt, project, soil, site, pedon_x, pedon_y) %>%
  left_join(prism_clean, by=c("dsp_pedon_id", "pedon_x", "pedon_y")) %>%
  mutate(climate = ifelse(mat>15, ifelse(map>900, "warm_wet", "warm_dry"), ifelse(map>900, "cool_wet", "cool_dry"))) %>%
  group_by(soil) %>%
  fill(climate, .direction = "downup") %>% # fill in missing climate grouping for rows that don't have lat/long
  ungroup()
write_csv(project_dat, here("data_processed", "05_project_data.csv"))

# Make reduced dataframe of site climate information for later joining
site_clim <- project_dat %>%
  select(project, soil, dsp_pedon_id, pedon_x, pedon_y, mat, map, climate)

# 2 - Pull in clay% data from KSSL ----
# Convert tables into dataframes
db <- lapply(setNames(nm = dbListTables(dsp4sh6)), dbReadTable, conn = dsp4sh6)

# pull out KSSL data
kssl <- db$kssllabmst %>% 
  clean_names()

clay <- kssl %>%
  select(natural_key, clay_tot_psa, silt_tot_psa, sand_tot_psa, tex_psda, ph_h2o) %>%
  rename(kssl_labsampnum = natural_key)

# 3 - Attach climate data to pedon and horizon data ----
# Attach climate to pedon data
pedon_clim <- soc_pedon %>%
  left_join(select(site_clim, dsp_pedon_id, project, soil, mat, map, climate), by=c("dsp_pedon_id", "project", "soil")) %>%
  group_by(soil) %>%
  fill(climate, .direction = "downup")  # fill in missing climate grouping for rows that don't have lat/long

# Write CSV
write_csv(pedon_clim, here("data_processed", "05_soc_pedon_clim.csv"))

# Attach climate data and clay% to horizon data
horizon_clim <- soc_horizon %>%
  left_join(select(site_clim, dsp_pedon_id, project, soil, mat, map, climate), by=c("dsp_pedon_id", "project", "soil")) %>%
  group_by(soil) %>%
  fill(climate, .direction = "downup") %>% # fill in missing climate grouping for rows that don't have lat/long
  left_join(clay, by="kssl_labsampnum") # join in KSSL clay % particle size analysis data

write_csv(horizon_clim, here("data_processed", "05_soc_horizon_clim.csv"))

# 4 - Make filtered pedon and horizon dataframes that exclude soils that aren't consistent between treatments ----

# Check data to see if soil series are consistent between treatments: 
soc_pedon %>% distinct(soil, label, project)
# Moist projects have treatments replicated across soil series, except for University of Minnesota project (Marquis, Readlyn, and Kenyon series)

# For looking at soc stocks to 100 cm -
soils_exclude <- c("Kenyon", "Marquis", "Readlyn")

soc_pedon_filt <- pedon_clim %>%
  filter(!soil %in% soils_exclude)
# What this data should be used for: mixed linear models to look at influence of management on SOC stocks, accounting for variability due to soil series
# Don't apply filtering/exclusion to anything but mixed linear models

write_csv(soc_pedon_filt, here("data_processed", "05_soc_pedon_filt.csv"))

# Make a dataframe of the horizon data excluding the same soils
soc_horizon_filt <- horizon_clim %>%
  filter(!soil %in% soils_exclude)
write_csv(soc_horizon_filt, here("data_processed", "05_soc_horizon_filt.csv"))
# What this data should be used for: depth plots and mixed linear models to look at influence of management on SOC stocks, accounting for variability due to soil series

# 6 - Dataframe of indicator data in surface horizons only ----
# Want average values across 0-10 cm (i.e. average 0-5 and 5-10 for each pedon) for all indicators
surf_all <- horizon_clim %>%
  filter(hrzdep_b == "5" | hrzdep_b=="10") %>%
  group_by(dsp_pedon_id) %>%
  summarize(across(c(soc_pct,bulk_density, tn_pct:yoder_agg_stab_mwd, p_h:ace, clay_pct, clay_tot_psa, ph_h2o), mean)) %>%
  left_join(select(site_clim, dsp_pedon_id, mat, map, climate), by="dsp_pedon_id") %>%
  left_join(select(project_dat, dsp_pedon_id:site), by="dsp_pedon_id") %>%
  mutate(clay_combined = ifelse(is.na(clay_pct), clay_tot_psa, clay_pct),
         clay_src = ifelse(is.na(clay_pct), 
                           ifelse(is.na(clay_tot_psa), "NA", "kssl"), "coop"),
         ph_combined = ifelse(is.na(p_h), ph_h2o, p_h),
         ph_src = ifelse(is.na(p_h), 
                           ifelse(is.na(ph_h2o), "NA", "kssl"), "coop"))
  
write_csv(surf_all, here("data_processed", "05_surface_horizons.csv"))
# What this data should be used for: analyzing sensitivity of indicators in surface horizons

# 5 - Dataframe for meta-analysis with both SOC stock data (pedon) and surface indicators for matching pedons ----
# want to use this to calculate response ratios :)
surf_sub <- surf_all %>%
  select(dsp_pedon_id, soc_pct, 
         bulk_density, tn_pct:yoder_agg_stab_mwd, p_h:ace, clay_combined, clay_src)

meta_df <- pedon_clim %>%
  left_join(surf_sub, by=c("dsp_pedon_id"))

write_csv(meta_df, here("data_processed", "05_meta_df.csv"))

# 6 - Ecological sites ----
eco_sites <- db$plotoverview %>% 
  clean_names() %>%
  select(project, dsp_plot_id, ecological_site, ecological_site_id, ecological_state, ecological_state_id)

write_csv(eco_sites, here("data_processed", "05_eco_sites.csv"))

# 7 - SSURGO soil texture ----
# need table of unique locations
unique_locations <- project_dat %>%
  select(project, label, trt, soil, pedon_x, pedon_y) %>%
  group_by(project, label, trt) %>%
  slice(1)

# convert to sf object
pedon_pts <- st_as_sf(unique_locations, coords = c("pedon_x", "pedon_y"), crs = 4326)

# SDA query
# First get map unit keys
mu <- SDA_spatialQuery(pedon_pts, what = 'mukey', geomIntersection = TRUE, byFeature=TRUE)
# bind to pedons
pedon_pts_mu = cbind(pedon_pts, mu)

comp <- sprintf("mukey IN %s", format_SQL_in_statement(mu$mukey))
# query to return mukey
comp_s <- fetchSDA(WHERE = comp, duplicates = TRUE, childs = FALSE, nullFragsAreZero = TRUE, stringsAsFactors = FALSE, rmHzErrors = FALSE)
plot(comp_s[1:10], color = "claytotal_r", label = 'compname', cex.names = 0.75)

comp_texture <- as(comp_s, "data.frame") %>%
  filter(compname %in% unique_locations$soil) %>%
  select(compname, comppct_r, hzname, hzdept_r, hzdepb_r, texture, sandtotal_r, silttotal_r, claytotal_r) %>%
  filter(hzname!="Oe") %>%
  group_by(compname) %>%
  slice(1) # select only top horizon

ssurgo_texture <- unique_locations %>%
  ungroup() %>%
  left_join(select(comp_texture, compname, texture, sandtotal_r, silttotal_r, claytotal_r), by=c("soil" = "compname")) %>%
  distinct(project, soil, texture, sandtotal_r, silttotal_r, claytotal_r) %>%
  rename(sand_pct = sandtotal_r, silt_pct = silttotal_r, clay_pct = claytotal_r) %>%
  filter(!is.na(sand_pct)) %>%
  mutate(texture_source = "mapped")

write_csv(ssurgo_texture, here("data_processed", "05_ssurgo_texture.csv"))
