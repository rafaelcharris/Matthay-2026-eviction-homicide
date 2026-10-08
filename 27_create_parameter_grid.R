# Create grid of parameters
rm(list = ls())
setwd("/gpfs/data/matthe01lab/01_EvictionMoratoria/")
library(tidyverse)

subgroups = c("age1", "age2", "age3", "age4",  "female", "asian", "aian", "nhpi", "black", "female", "male", "hisp", "white")


# parameter grid
param_grid <- expand_grid(
  # Which dataset am I going to use?
  #df = "eviction_wide_extended_city_all_states.rds",
  df =  "eviction_dataset_wide_state_all_counties.rds",
  #       "eviction_dataset_wide_state_only_big_counties.rds"),
  #       "eviction_dataset_wide_5000.rds", "eviction_dataset_wide_50_000.rds")
  #       "dt_wide_smaller_without_top5_eif_difs.rds", "dt_wide_smaller_without_top10_eif_difs.rds", "dt_wide_smaller_without_top20_eif_difs.rds"), # main one is: eviction_dataset_wide_smaller.rds
  #df = "eviction_dataset_wide_st_with_variation.rds",
  #df = apply(expand.grid("eviction_dataset_wide_state_", subgroups, "all_counties.rds"), 1, paste, collapse =""),
  first_period    = 4, # up to 24. Usually just 4
  end_period      = 24, # up to 24. This can be something like 5:24 if I want it at everypoint
  k               = 4,
  outcome         = c("mean_ipv_state_rate", "mean_ipv_firearm_state_rate", "mean_ipv_nonfirearm_state_rate"),
  #outcome         = c("mean_ipv_firearm_city_rate", "mean_ipv_nonfirearm_city_rate"),      # ipv_cty_rate if crossectional
  #policy          = "lift_moratoria_obin", #, "lift_moratoria_obin_ceiling", "lift_moratoria_obin_floor"),
  #policy          = "lift_moratoria_obin",lift_eviction_policy ## This is for the city levbel only
  policy          = "lift_moratoria_obin", ## This is for the State levbel only
  shift_label     =  c("observed", "may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
  # OLD: "always_lift1", "always_lift0", "observed", 
  # added July 29th: "may-2020", "jun-2020",
  # "oct_2020","apr_2021", "oct_2021"  
  # "jul_2020","jan_2021", "jul_2021"  
  learners        = list(c("SL.mean", "SL.glmnet", "SL.earth")), 
                    #list(c("SL.mean", "SL.glmnet", "SL.earth", "SL.xgboost"))),
  folds = 1,
  #subgroup      = str_c(c("age1", "age2", "age3", "age4",  "female", "asian", "aian", "nhpi", "black", "female", "male", "hisp", "white"), "all_counties"),
  #                 "stages1_2", "stages3_5", "st_with_variation"),
  subgroup       =  c("stages1_2", "stages3_5"), # "only_big_counties",
  #subgroup       =  "exclude_two_months",# "only_big_counties",
  mtp            = FALSE  # must align with shift_label order
) %>%
  mutate(mtp = ifelse(shift_label == "observed", TRUE, mtp),
         subgroup = ifelse(grepl("firearm", outcome), "all_counties", subgroup)
         #subgroup = ifelse(df == "eviction_dataset_wide_state_all_counties.rds","all_counties", subgroup)
         ) %>%
  distinct() %>%
  rowwise() %>%
  #filter(grepl(str_c("_", subgroup),  df)) %>%
  # * I need to use more than 1 fold when including xgboost 
  #mutate(folds = ifelse("SL.xgboost" %in% unlist(learners), 5, 1)) %>%
  ungroup() %>%
  mutate(job_id = row_number())
# the line underneath only applies if you are using subgroups
         #df = str_c(str_replace(df, ".rds", ""), subgroup, ".rds"))

saveRDS(param_grid, "02_data/00_auxiliary/grid_states_stages_firearm.rds")

View(param_grid)

