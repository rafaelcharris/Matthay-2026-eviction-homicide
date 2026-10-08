############################################################################################
# LAST UPDATE:  	
# DATE CREATED: July 15th 2025
# PROJECT:      Eviction Moratoria 
# PROGRAMMER:   Rafael Charris 
# RESEARCHER:   Ellicott Matthay 
# PURPOSE:      Show the contrasts for the tmle models 
#               
# NOTES:
#       - for the tmle models here to run I think I have to install the CRAN version
#         and not the github version
############################################################################################

rm(list = ls())

library(tidyverse)
library(lmtp, lib.loc = "/gpfs/data/matthe01lab/01_EvictionMoratoria/01_code") 

path <- '03_results/'
res_list <- list()

# 1. Main exposures files

files <- gtools::mixedsort(list.files(path, pattern = "exposures.rds"))
param_grid <- readRDS("02_data/00_auxiliary/grid_off_after_all_exposures.rds")

for (ff in files){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list[[index]] <- temp
}

# Convert to tibble 
df_res = tibble(res_list)

# Append the parameter grid to identify which model each row corresponds to
df_res_param <- df_res %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate
#! IMPORTANT: xgboost has folds = 5 for all of these. 
# when the model is not available is because it didn't have enough time to converge.

# 2. Create grid of contrasts 
main_contrasts_df = expand_grid(
  #! I think I have to specify here the learners 
  learners = c(list(c("SL.mean", "SL.glmnet", "SL.earth")), 
    list(c("SL.mean", "SL.glmnet", "SL.earth", "SL.xgboost"))),
       ref = c("observed"),
       counterfactual = c("jul-2020", "jan-2021", "jul-2021"), 
  exposure = c("lift_moratoria_obin", "lift_moratoria_obin_ceiling", "lift_moratoria_obin_floor")) %>%
  mutate(id = row_number())

contrast_list = list()

for (ii in 1:nrow(main_contrasts_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_df[ii,]
 
  tryCatch({ 
  main <- df_res_param[df_res_param$shift_label == row$counterfactual & 
                       identical(df_res_param$learners[[1]], row$learners[[1]]) & 
                         df_res_param$policy == row$exposure,
                         "res_list"][[1]]
  
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param[df_res_param$shift_label == row$ref &
                        identical(df_res_param$learners[[1]], row$learners[[1]]) &
                      df_res_param$policy == row$exposure,
                      "res_list"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_df <- bind_rows(contrast_list) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_df, by = "id")
#View(contrast_df)

saveRDS(contrast_df, file = "03_results/01_contrasts/main_contrasts.rds")


## 

# Age Group Results 
res_list_age <- list()

files_age <- gtools::mixedsort(list.files(path, pattern = "agegrp.rds"))
param_grid_age <- readRDS("02_data/00_auxiliary/grid_off_after_agegrp.rds")

for (ff in files_age){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_age[[index]] <- temp
}

# Convert to tibble 
df_res_age = tibble(res_list_age)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_age <- df_res_age %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_age, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate
#! IMPORTANT: xgboost has folds = 5 for all of these. 
# when the model is not available is because it didn't have enough time to converge.

# 2. Create grid of contrasts 
main_contrasts_age_df = expand_grid(
  #! I think I have to specify here the learners 
  agegrp = c("age1","age2","age3","age4"), 
       ref = c("observed"),
       counterfactual = c("jul-2020", "jan-2021", "jul-2021")) %>%
  mutate(id = row_number())

contrast_list_age = list()

for (ii in 1:nrow(main_contrasts_age_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_age_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_age[df_res_param_age$shift_label == row$counterfactual & 
                         df_res_param_age$subgroup == row$agegrp,
                         "res_list_age"][[1]]
  
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_age[df_res_param_age$shift_label == row$ref &
                       df_res_param_age$subgroup == row$agegrp,
                      "res_list_age"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_age[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_age[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_age_df <- bind_rows(contrast_list_age) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_age_df, by = "id")
View(contrast_age_df)



# XGBOOST 
res_list_xg <- list()
filesxg <- gtools::mixedsort(list.files(path, pattern = "exposures_xgboost.rds"))
param_grid_boost <- readRDS("02_data/00_auxiliary/grid_off_after_all_exposures_xgboost.rds")

for (ff in filesxg){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_xg[[index]] <- temp
}

# Convert to tibble 
df_res_xg = tibble(res_list_xg)

# Append the parameter grid to identify which model each row corresponds to
df_res_xg_param <- df_res_xg %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_boost, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate
# Note:
#    - when the model is not available is because it didn't have enough time to converge.

main_contrasts_boost_df = expand_grid(
  #! I think I have to specify here the learners 
  folds = c(1,5),
       ref = c("observed"),
       counterfactual = c("jul-2020", "jan-2021", "jul-2021"), 
  exposure = c("lift_moratoria_obin", "lift_moratoria_obin_ceiling", "lift_moratoria_obin_floor")) %>%
  mutate(id = row_number())

contrast_xg_list = list()

for (ii in 1:nrow(main_contrasts_boost_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_boost_df[ii,]
 
  tryCatch({ 
  main <- df_res_xg_param[df_res_xg_param$shift_label == row$counterfactual & 
                           df_res_xg_param$folds == row$folds & 
                         df_res_xg_param$policy == row$exposure,
                         "res_list_xg"][[1]]
  
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_xg_param[df_res_xg_param$shift_label == row$ref &
                           df_res_xg_param$folds == row$folds & 
                      df_res_xg_param$policy == row$exposure,
                      "res_list_xg"][[1]]
  
  }, error = function(e){
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- NULL
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_xg_list[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
  contrast_xg_list[[ii]] <<-  tibble(
    shift = NA, 
    ref = NA, theta = NA, std.error = NA, conf.low = NA,
    conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
  
  #contrast_xg_list[[ii]] <- temp$vals
}

contrast_df_xg <- bind_rows(contrast_xg_list) %>%  #! This is better but still fix that
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_boost_df, by = "id")
View(contrast_df_xg)

saveRDS(contrast_df_xg, file = "03_results/01_contrasts/main_contrasts_xgboost.rds")


### - Erase down maybe?>

# learners = earth, glmnet, mean 
# main exposure and main contraststhe indices correspond to the rows in the parameter grid
#lmtp_contrast(res_list[[3]], ref = res_list[[1]])
#lmtp_contrast(res_list[[5]], ref = res_list[[1]])
#lmtp_contrast(res_list[[7]], ref = res_list[[1]])
#
## ceiling exposure 
#lmtp_contrast(res_list[[11]], ref = res_list[[9]])
#lmtp_contrast(res_list[[13]], ref = res_list[[9]])
#lmtp_contrast(res_list[[15]], ref = res_list[[9]])
#
## floor exposure 
#lmtp_contrast(res_list[[19]], ref = res_list[[17]])
#lmtp_contrast(res_list[[21]], ref = res_list[[17]])
#lmtp_contrast(res_list[[23]], ref = res_list[[17]])
#
## learners = earth, glmnet, mean, xgboost
## main exposure and main contrasts the indices correspond to the rows in the parameter grid
#
#lmtp_contrast(res_list[[4]], ref = res_list[[2]])
#lmtp_contrast(res_list[[6]], ref = res_list[[2]])
#lmtp_contrast(res_list[[8]], ref = res_list[[2]])
#
## ceiling exposure 
#lmtp_contrast(res_list[[12]], ref = res_list[[9]])
#lmtp_contrast(res_list[[14]], ref = res_list[[9]])
#lmtp_contrast(res_list[[16]], ref = res_list[[9]])
#
## floor exposure 
#lmtp_contrast(res_list[[20]], ref = res_list[[18]])
#lmtp_contrast(res_list[[22]], ref = res_list[[18]])
#lmtp_contrast(res_list[[24]], ref = res_list[[18]])
#

## * all contrast 
contrast_list <- list()
ii <- 1

# This first number indicates the list of learners
# 1. glmnet, mean, earth
# 2. glmnet, mean, earth, xgboost

for (ref_num in seq(1:2) ) {
  for (cont_num in seq(1:3)) {
    
    # This second number indicates the counterfactual 
    # 1. "july-2020"
    # 2. "jan-2021"
    # 3. "july-2021"
    
    tryCatch({ 
      
    message("Comparing row num: ", ref_num + 2 *cont_num)
      
    temp <- lmtp_contrast(res_list[[ref_num + 2 *cont_num]], ref = res_list[[ref_num]])
      
    contrast_list[[ii]] <- tibble(temp$estimates, num_observations = length(res_list[[ref_num]]$id))
    
    contrast_list[[ii]] <- bind_cols(contrast_list[[ii]],
                                                 param_grid[ref_num,  c("df", "shift_label", "policy")])
    
    contrast_list[[ii]] <- bind_cols(contrast_list[[ii]],
                                                 param_grid[ref_num + 2 * cont_num,  c("df", "shift_label", "policy")])
    
    
    }, error = function(e){
        
    message("error with a contrast")
        
        contrast_list[[ii]] <-  tibble(
          shift = NULL, 
          ref = NULL, estimate= NULL, std.error = NULL, conf.low = NULL,
          conf.high = NULL, p.value = NULL, num_observations = NULL)
      }
    )
    ii <- ii + 1 
  }
}


path <- '03_results/'
res_list_sub <- list()

files_sub <- gtools::mixedsort(list.files(path, pattern = "subgroups.rds"))
param_grid_sub <- readRDS("02_data/00_auxiliary/grid_off_after_subgroups.rds")

for (ff in files_sub){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_sub[[index]] <- temp
}

df_sub = tibble(res_list_sub)

df_sub_param <-  df_sub %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_sub, by = c("id" = "job_id"))

# create a grid with the ref and comparisons i want, and then pass it to a function that selects the correct columns 

contrasts_df = expand_grid(subgroup = c("white", "male", "female", "asian", "aian", "nhpi", "black"), 
       ref = c("observed"),
       counterfactual = c("jul-2020", "jan-2021", "jul-2021")) %>%
  mutate(id = row_number())

contrast_list_subgroups = list()

for (ii in 1:nrow(contrasts_df)) {
  row <-  contrasts_df[ii,]
  
  main <-  df_sub_param[df_sub_param$shift_label == row$counterfactual & df_sub_param$subgroup == row$subgroup,
                         "res_list_sub"][[1]]
  
  ref <- df_sub_param[df_sub_param$shift_label == row$ref & df_sub_param$subgroup == row$subgroup,
                        "res_list_sub"][[1]]
  
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  
  contrast_list_subgroups[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
}

contrast_df_subgroups <- bind_rows(contrast_list_subgroups) %>%
  mutate(id = row_number()) %>% 
  left_join(contrasts_df, by = "id")

saveRDS(contrast_df_subgroups, file = "03_results/01_contrasts/subgroup_contrast.rds")

## Hispanic

res_list_hisp <- list()
files_hisp <- gtools::mixedsort(list.files(path, pattern = "hisp.rds"))
param_grid_hisp <- readRDS("02_data/00_auxiliary/grid_hisp.rds")

for (ff in files_hisp){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_hisp[[index]] <- temp
}

df_hisp = tibble(res_list_hisp)

df_hisp_param <-  df_hisp %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_hisp, by = c("id" = "job_id"))

# create a grid with the ref and comparisons i want, and then pass it to a function that selects the correct columns 

contrasts_hisp_df = expand_grid( 
       ref = c("observed"),
       counterfactual = c("jul-2020", "jan-2021", "jul-2021"), 
       subgroup = "hisp") %>%
  mutate(id = row_number())

contrast_list_hisp = list()

for (ii in 1:nrow(contrasts_hisp_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  contrasts_hisp_df[ii,]
  
  tryCatch({ 
    main <- df_hisp_param[df_hisp_param$shift_label == row$counterfactual & 
                                   df_hisp_param$subgroup == row$subgroup,
                                 "res_list_hisp"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
  tryCatch({
    ref <- df_hisp_param[df_hisp_param$shift_label == row$ref &
                                  df_hisp_param$subgroup == row$subgroup,
                                "res_list_hisp"][[1]]
    
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
    temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
    contrast_list_hisp[[ii]] <- tibble(temp$vals,  num_observations = length(main[[1]]$id))
    
  }, error = function(e){
    
    contrast_list_hisp[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_df_hisp <- bind_rows(contrast_list_hisp) %>%
  mutate(id = row_number()) %>% 
  left_join(contrasts_hisp_df, by = "id")

View(contrast_df_hisp)

saveRDS(contrast_df_hisp, file = "03_results/01_contrasts/hispanic_contrast.rds")


## extended subgroups (including age groups)

contrast_df_subgroups_all <-  contrast_df_subgroups %>%
  bind_rows(contrast_df_hisp, 
            contrast_age_df %>% rename(subgroup = agegrp))

saveRDS(contrast_df_subgroups_all, file = "03_results/01_contrasts/all_subgroup_contrast.rds")

#! For Reference --- Not using this code ----- |
#param_grid <- read_rds("data/grid_off_after.rds") %>%
#  dplyr::select(job_id, df, counterfactual, policy) 
#
#for (ref_num in seq(1:4) ) {
#  
#  for (cont_num in seq(1:6)) {
#    # This second number indicates the counterfactual 
#    # 1. "oct-2020"
#    # 2. "apr-2021"
#    # 3. "oct-2021"
#    # 4. "apr-2022"
#    # 5. "oct-2022"
#    # 6. "all_off"
#    
#    tryCatch({ 
#      
#      temp <-  lmtp_contrast(res_nonhomeless_list[[ref_num + 4 * cont_num]],
#                             ref = res_nonhomeless_list[[ref_num]])
#      nonhomeless_contrast_list[[ii]] <- temp$estimates
#      
#      nonhomeless_contrast_list[[ii]] <- bind_cols(nonhomeless_contrast_list[[ii]],
#                                                   param_grid_homeless[ref_num,  c("df", "counterfactual", "policy")])
#      
#      nonhomeless_contrast_list[[ii]] <- bind_cols(nonhomeless_contrast_list[[ii]],
#                                                   param_grid_homeless[ref_num + 4 * cont_num,  c("df", "counterfactual", "policy")])
#      
#    }, error = function(e){
#      
#      message("|--- Error with a contrast in row: ", ii)
#      nonhomeless_contrast_list[[ii]] <<-  tibble(
#        shift = NA, 
#        ref = NA, estimate= NA, std.error = NA, conf.low = NA,
#        conf.high = NA, p.value = NA)
#      
#      nonhomeless_contrast_list[[ii]] <<- bind_cols(nonhomeless_contrast_list[[ii]],
#                                                    param_grid_homeless[ref_num,  c("df", "counterfactual", "policy")])
#      
#      nonhomeless_contrast_list[[ii]] <<- bind_cols(nonhomeless_contrast_list[[ii]],
#                                                    param_grid_homeless[ref_num + 4 * cont_num,  c("df", "counterfactual", "policy")])
#      
#    }
#    )
#    
#    ii <- ii + 1 
#  }
#}
#
#
# Firearm Nonfirearm 

res_list_firearm <- list()
res_list_firearm_obs <- list()

files_firearm <- gtools::mixedsort(list.files(path, pattern = "firearm.rds"))
files_firearm_obs <- gtools::mixedsort(list.files(path, pattern = "nonfirearm_observed.rds"))
param_grid_firearm <- readRDS("02_data/00_auxiliary/grid_firearm_nonfirearm.rds")
param_grid_firearm_obs <- readRDS("02_data/00_auxiliary/grid_firearm_nonfirearm_observed.rds")

for (ff in files_firearm){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_firearm[[index]] <- temp
}

# load observed
for (ff in files_firearm_obs){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_firearm_obs[[index]] <- temp
}

# Convert to tibble 
df_res_firearm = tibble(res_list_firearm)
df_res_firearm_obs = tibble(res_list_firearm_obs) %>%
  rename(res_list_firearm = res_list_firearm_obs)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_firearm <- df_res_firearm %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_firearm, by = c("id" = "job_id"))

df_res_param_firearm_obs <- df_res_firearm_obs %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_firearm_obs, by = c("id" = "job_id"))

df_res_param_firearm <- bind_rows(df_res_param_firearm_obs,
                                  df_res_param_firearm)

# Create a grid of the contrasts I want to evaluate
#! IMPORTANT: xgboost has folds = 5 for all of these. 
# when the model is not available is because it didn't have enough time to converge.

# 2. Create grid of contrasts 
main_contrasts_firearm_df = expand_grid(
  #! I think I have to specify here the learners 
  outcome = c("mean_ipv_firearm_cty_rate", "mean_ipv_nonfirearm_cty_rate"),
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
  policy = c("lift_moratoria_obin", "lift_moratoria_obin_ceiling", "lift_moratoria_obin_floor")
  ) %>%
  mutate(id = row_number())

contrast_list_firearm = list()

for (ii in 1:nrow(main_contrasts_firearm_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_firearm_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_firearm[df_res_param_firearm$shift_label == row$counterfactual & 
                         df_res_param_firearm$policy == row$policy &
                         df_res_param_firearm$outcome == row$outcome,
                         "res_list_firearm"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_firearm[df_res_param_firearm$shift_label == row$ref &
                         df_res_param_firearm$policy == row$policy &
                       df_res_param_firearm$outcome == row$outcome,
                      "res_list_firearm"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_firearm[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_firearm[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_firearm_df <- bind_rows(contrast_list_firearm) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_firearm_df, by = "id")
View(contrast_firearm_df)

saveRDS(contrast_firearm_df, file = "03_results/01_contrasts/firearm_contrasts.rds")



## Bigger Counties

res_list_bc <- list()

files_bc <- gtools::mixedsort(list.files(path, pattern = "bigger_counties.rds"))
param_grid_bc <- readRDS("02_data/00_auxiliary/grid_bigger_counties.rds")

for (ff in files_bc){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_bc[[index]] <- temp
}

# Convert to tibble 
df_res_bc = tibble(res_list_bc)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_bc <- df_res_bc %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_bc, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate
#! IMPORTANT: xgboost has folds = 5 for all of these. 
# when the model is not available is because it didn't have enough time to converge.

# 2. Create grid of contrasts 
main_contrasts_bc_df = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
  subgroup = c("5000", "50_000")
  ) %>%
  mutate(id = row_number())

contrast_list_bc = list()

for (ii in 1:nrow(main_contrasts_bc_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_bc_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_bc[df_res_param_bc$shift_label == row$counterfactual & 
                         df_res_param_bc$subgroup == row$subgroup,
                         "res_list_bc"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_bc[df_res_param_bc$shift_label == row$ref &
                       df_res_param_bc$subgroup == row$subgroup,
                      "res_list_bc"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_bc[[ii]] <- tibble(temp$vals,  num_observations = length(main[[1]]$id))
  
  
  }, error = function(e){
    
    contrast_list_bc[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observatison = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_bc_df <- bind_rows(contrast_list_bc) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_bc_df, by = "id")
View(contrast_bc_df)

saveRDS(contrast_bc_df, file = "03_results/01_contrasts/bigger_counties_contrasts.rds")



### Broken Down by Moratoria Stages

res_list_stages <- list()

files_stages <- gtools::mixedsort(list.files(path, pattern = "moratoria_stages.rds"))
param_grid_stages <- readRDS("02_data/00_auxiliary/grid_moratoria_stages.rds")

for (ff in files_stages){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_stages[[index]] <- temp
}

# Convert to tibble 
df_res_stages = tibble(res_list_stages)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_stages <- df_res_stages %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_stages, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate
#! IMPORTANT: xgboost has folds = 5 for all of these. 
# when the model is not available is because it didn't have enough time to converge.

# 2. Create grid of contrasts 
main_contrasts_stages_df = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
  subgroup = c("stages1_2", "stages3_5")
  ) %>%
  mutate(id = row_number())

contrast_list_stages = list()

for (ii in 1:nrow(main_contrasts_stages_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_stages_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_stages[df_res_param_stages$shift_label == row$counterfactual & 
                         df_res_param_stages$subgroup == row$subgroup,
                         "res_list_stages"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_stages[df_res_param_stages$shift_label == row$ref &
                       df_res_param_stages$subgroup == row$subgroup,
                      "res_list_stages"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_stages[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_stages[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_stages_df <- bind_rows(contrast_list_stages) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_stages_df, by = "id")
View(contrast_stages_df)

saveRDS(contrast_stages_df, file = "03_results/01_contrasts/stages_contrasts.rds")



# City Level Analysis 

res_list_city <- list()

files_city <- gtools::mixedsort(list.files(path, pattern = "city_extended.rds"))
param_grid_city <- readRDS("02_data/00_auxiliary/grid_city_extended.rds")

for (ff in files_city){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_city[[index]] <- temp
}

# Convert to tibble 
df_res_city = tibble(res_list_city)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_city <- df_res_city %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_city, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate

# 2. Create grid of contrasts 
main_contrasts_city_df = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021")
  ) %>%
  mutate(id = row_number())

contrast_list_city = list()

for (ii in 1:nrow(main_contrasts_city_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_city_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_city[df_res_param_city$shift_label == row$counterfactual,
                         "res_list_city"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_city[df_res_param_city$shift_label == row$ref,
                      "res_list_city"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_city[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_city[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_city_df <- bind_rows(contrast_list_city) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_city_df, by = "id")
View(contrast_city_df)

saveRDS(contrast_city_df, file = "03_results/01_contrasts/city_extended_contrasts_v2.rds")


# City Firearm 
res_list_city_fire <- list()

files_city_fire <- gtools::mixedsort(list.files(path, pattern = "city_firearm.rds"))
param_grid_city_firearm <- readRDS("02_data/00_auxiliary/grid_city_firearm.rds")

for (ff in files_city_fire){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_city_fire[[index]] <- temp
}

# Convert to tibble 
df_res_city_firearm = tibble(res_list_city_fire)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_city_firearm <- df_res_city_firearm %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_city_firearm, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate

# 2. Create grid of contrasts 
main_contrasts_city_firearm_df = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
       outcome = c("mean_ipv_firearm_city_rate",  "mean_ipv_nonfirearm_city_rate")
  ) %>%
  mutate(id = row_number())

contrast_list_city_firearm = list()

for (ii in 1:nrow(main_contrasts_city_firearm_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_city_firearm_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_city_firearm[df_res_param_city_firearm$shift_label == row$counterfactual &
                                    df_res_param_city_firearm$outcome == row$outcome,
                         "res_list_city_fire"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_city_firearm[df_res_param_city_firearm$shift_label == row$ref &
                                    df_res_param_city_firearm$outcome == row$outcome,
                      "res_list_city_fire"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_city_firearm[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_city_firearm[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_city_firearm_df <- bind_rows(contrast_list_city_firearm) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_city_firearm_df, by = "id")

View(contrast_city_firearm_df)

saveRDS(contrast_city_firearm_df, file = "03_results/01_contrasts/city_firearm_contrasts.rds")

# City - stages
res_list_city_stages <- list()

files_city_stages <- gtools::mixedsort(list.files(path, pattern = "city_stages.rds"))
param_grid_city_stages <- readRDS("02_data/00_auxiliary/grid_city_stages.rds")

for (ff in files_city_stages){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_city_stages[[index]] <- temp
}

# Convert to tibble 
df_res_city_stages = tibble(res_list_city_stages)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_city_stages <- df_res_city_stages %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_city_stages, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate

# 2. Create grid of contrasts 
main_contrasts_city_stages_df = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
       subgroup = c("stages1_2",  "stages3_5")
  ) %>%
  mutate(id = row_number())

contrast_list_city_stages = list()

for (ii in 1:nrow(main_contrasts_city_stages_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_city_stages_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_city_stages[df_res_param_city_stages$shift_label == row$counterfactual &
                                    df_res_param_city_stages$subgroup == row$subgroup,
                         "res_list_city_stages"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_city_stages[df_res_param_city_stages$shift_label == row$ref &
                                    df_res_param_city_stages$subgroup == row$subgroup,
                      "res_list_city_stages"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_city_stages[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_city_stages[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_city_stages_df <- bind_rows(contrast_list_city_stages) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_city_stages_df, by = "id")

View(contrast_city_stages_df)

saveRDS(contrast_city_stages_df, file = "03_results/01_contrasts/city_stages_contrasts.rds")


# State Level - firearm and stages 

res_list_state_fs <- list()

files_state_fs <- gtools::mixedsort(list.files(path, pattern = "_states_stages_firearm"))
param_grid_state_fs <- readRDS("02_data/00_auxiliary/grid_states_stages_firearm.rds")

for (ff in files_state_fs){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_state_fs[[index]] <- temp
}

# Convert to tibble 
df_res_state_fs = tibble(res_list_state_fs)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_state_fs <- df_res_state_fs %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_state_fs, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate

# 2. Create grid of contrasts 
main_contrasts_state_fs_df = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
       subgroup = c("all_counties", "stages1_2", "stages3_5"), 
       outcome = c("mean_ipv_state_rate", "mean_ipv_firearm_state_rate", "mean_ipv_nonfirearm_state_rate")
  ) %>%
  mutate(subgroup = ifelse(grepl("firearm", outcome), "all_counties", subgroup)) %>%
  distinct(ref, counterfactual, subgroup, outcome) %>%
  mutate(id = row_number()) 

contrast_list_state_fs = list()

for (ii in 1:nrow(main_contrasts_state_fs_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_state_fs_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_state_fs[df_res_param_state_fs$shift_label == row$counterfactual &
                             df_res_param_state_fs$subgroup == row$subgroup &
                             df_res_param_state_fs$outcome == row$outcome,
                         "res_list_state_fs"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_state_fs[df_res_param_state_fs$shift_label == row$ref &
                             df_res_param_state_fs$subgroup == row$subgroup &
                             df_res_param_state_fs$outcome == row$outcome,
                      "res_list_state_fs"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_state_fs[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_state_fs[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_state_fs_df <- bind_rows(contrast_list_state_fs) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_state_fs_df, by = "id")
View(contrast_state_fs_df)

saveRDS(contrast_state_fs_df, file = "03_results/01_contrasts/state_fs_contrasts.rds")

## State level subgroups =====

res_list_state_sg <- list()

files_state_sg <- gtools::mixedsort(list.files(path, pattern = "state_subgroups_all_counties.rds"))
param_grid_state_sg <- readRDS("02_data/00_auxiliary/grid_state_subgroups_all_counties.rds")

for (ff in files_state_sg){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_state_sg[[index]] <- temp
}

# Convert to tibble 
df_res_state_sg = tibble(res_list_state_sg)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_state_sg <- df_res_state_sg %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_state_sg, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate

# 2. Create grid of contrasts 
main_contrasts_state_df_sg = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021"),
       subgroup = str_c(c("age1", "age2", "age3", "age4", "asian", "aian", "nhpi", "black", "female", "male", "hisp", "white"), "all_counties"),
  ) %>%
  mutate(id = row_number())

contrast_list_state_sg = list()

for (ii in 1:nrow(main_contrasts_state_df_sg)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_state_df_sg[ii,]
 
  tryCatch({ 
  main <- df_res_param_state_sg[df_res_param_state_sg$shift_label == row$counterfactual &
                             df_res_param_state_sg$subgroup == row$subgroup,
                         "res_list_state_sg"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_state_sg[df_res_param_state_sg$shift_label == row$ref &
                             df_res_param_state_sg$subgroup == row$subgroup,
                      "res_list_state_sg"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_state_sg[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_state_sg[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_state_df_sg <- bind_rows(contrast_list_state_sg) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_state_df_sg, by = "id") %>%
  mutate(subgroup = str_replace(subgroup, "all_counties",  ""))

View(contrast_state_df_sg)

saveRDS(contrast_state_df_sg, file = "03_results/01_contrasts/state_subgroup_contrasts.rds")



# Controlling CARES ACT

res_list_any <- list()

files_any <- gtools::mixedsort(list.files(path, pattern = "any_moratoria.rds"))
param_grid_any <- readRDS("02_data/00_auxiliary/grid_any_moratoria.rds")

for (ff in files_any){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_any[[index]] <- temp
}

# Convert to tibble 
df_res_any = tibble(res_list_any)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_any <- df_res_any %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_any, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate

# 2. Create grid of contrasts 
main_contrasts_any_df = expand_grid(
  ref = c("observed"),
  counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021")
) %>%
  mutate(id = row_number())

contrast_list_any = list()

for (ii in 1:nrow(main_contrasts_any_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_any_df[ii,]
  
  tryCatch({ 
    main <- df_res_param_any[df_res_param_any$shift_label == row$counterfactual,
                              "res_list_any"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
  tryCatch({
    ref <- df_res_param_any[df_res_param_any$shift_label == row$ref,
                             "res_list_any"][[1]]
    
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
    temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
    contrast_list_any[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
    
  }, error = function(e){
    
    contrast_list_any[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_any_df <- bind_rows(contrast_list_any) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_any_df, by = "id")
View(contrast_any_df)

saveRDS(contrast_any_df, file = "03_results/01_contrasts/any_moratoria_contrasts.rds")




# cares_act Moratoria

res_list_cares_act <- list()
files_cares_act <- gtools::mixedsort(list.files(path, pattern = "cares_act.rds"))
param_grid_cares_act <- readRDS("02_data/00_auxiliary/grid_cares_act.rds")

for (ff in files_cares_act){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_cares_act[[index]] <- temp
}
# Convert to tibble 
df_res_cares_act = tibble(res_list_cares_act)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_cares_act <- df_res_cares_act %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_cares_act, by = c("id" = "job_id"))
# Create a grid of the contrasts I want to evaluate
# 2. Create grid of contrasts 
main_contrasts_cares_act_df = expand_grid(
  ref = c("observed"),
  counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021")
) %>%
  mutate(id = row_number())

contrast_list_cares_act = list()
for (ii in 1:nrow(main_contrasts_cares_act_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_cares_act_df[ii,]
    tryCatch({ 
    main <- df_res_param_cares_act[df_res_param_cares_act$shift_label == row$counterfactual,
                              "res_list_cares_act"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
  tryCatch({
    ref <- df_res_param_cares_act[df_res_param_cares_act$shift_label == row$ref,
                             "res_list_cares_act"][[1]]
      }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
    temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
    contrast_list_cares_act[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
    
      }, error = function(e){
        contrast_list_cares_act[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}
contrast_cares_act_df <- bind_rows(contrast_list_cares_act) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_cares_act_df, by = "id")
View(contrast_cares_act_df)

saveRDS(contrast_cares_act_df, file = "03_results/01_contrasts/cares_act_contrasts.rds")




# States with Variation

res_list_st_with_variation <- list()
files_st_with_variation <- gtools::mixedsort(list.files(path, pattern = "st_with_variation.rds"))
param_grid_st_with_variation <- readRDS("02_data/00_auxiliary/grid_st_with_variation.rds")

for (ff in files_st_with_variation){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_st_with_variation[[index]] <- temp
}
# Convert to tibble 
df_res_st_with_variation = tibble(res_list_st_with_variation)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_st_with_variation <- df_res_st_with_variation %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_st_with_variation, by = c("id" = "job_id"))
# Create a grid of the contrasts I want to evaluate
# 2. Create grid of contrasts 
main_contrasts_st_with_variation_df = expand_grid(
  ref = c("observed"),
  counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021")
) %>%
  mutate(id = row_number())

contrast_list_st_with_variation = list()
for (ii in 1:nrow(main_contrasts_st_with_variation_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_st_with_variation_df[ii,]
    tryCatch({ 
    main <- df_res_param_st_with_variation[df_res_param_st_with_variation$shift_label == row$counterfactual,
                              "res_list_st_with_variation"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
  tryCatch({
    ref <- df_res_param_st_with_variation[df_res_param_st_with_variation$shift_label == row$ref,
                             "res_list_st_with_variation"][[1]]
      }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
    temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
    contrast_list_st_with_variation[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
    
      }, error = function(e){
        contrast_list_st_with_variation[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}
contrast_st_with_variation_df <- bind_rows(contrast_list_st_with_variation) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_st_with_variation_df, by = "id")
View(contrast_st_with_variation_df)

saveRDS(contrast_st_with_variation_df, file = "03_results/01_contrasts/st_with_variation_contrasts.rds")


# States with Moratorias longer than 2 months

res_list_exclude_two_months <- list()
files_exclude_two_months <- gtools::mixedsort(list.files(path, pattern = "exclude_two_months.rds"))
param_grid_exclude_two_months <- readRDS("02_data/00_auxiliary/grid_exclude_two_months.rds")

for (ff in files_exclude_two_months){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_exclude_two_months[[index]] <- temp
}
# Convert to tibble 
df_res_exclude_two_months = tibble(res_list_exclude_two_months)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_exclude_two_months <- df_res_exclude_two_months %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_exclude_two_months, by = c("id" = "job_id"))
# Create a grid of the contrasts I want to evaluate
# 2. Create grid of contrasts 
main_contrasts_exclude_two_months_df = expand_grid(
  ref = c("observed"),
  counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021")
) %>%
  mutate(id = row_number())

contrast_list_exclude_two_months = list()
for (ii in 1:nrow(main_contrasts_exclude_two_months_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_exclude_two_months_df[ii,]
    tryCatch({ 
    main <- df_res_param_exclude_two_months[df_res_param_exclude_two_months$shift_label == row$counterfactual,
                              "res_list_exclude_two_months"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
  tryCatch({
    ref <- df_res_param_exclude_two_months[df_res_param_exclude_two_months$shift_label == row$ref,
                             "res_list_exclude_two_months"][[1]]
      }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
    temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
    contrast_list_exclude_two_months[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
    
      }, error = function(e){
        contrast_list_exclude_two_months[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}
contrast_exclude_two_months_df <- bind_rows(contrast_list_exclude_two_months) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_exclude_two_months_df, by = "id")
View(contrast_exclude_two_months_df)

saveRDS(contrast_exclude_two_months_df, file = "03_results/01_contrasts/exclude_two_months_contrasts.rds")






# Controlling for States fixed effects
res_list_state_fe <- list()

files_state_fe <- gtools::mixedsort(list.files(path, pattern = "state_fe.rds"))
param_grid_state_fe <- readRDS("02_data/00_auxiliary/grid_state_fe.rds")

for (ff in files_state_fe){
  #* Use index from the files name to assign it to the list 
  index <- as.numeric(str_extract(ff, "task_(\\d{1,2})*", group = 1))
  temp <- readRDS(str_c(path, "/", ff))
  res_list_state_fe[[index]] <- temp
}

# Convert to tibble 
df_res_state_fe = tibble(res_list_state_fe)

# Append the parameter grid to identify which model each row corresponds to
df_res_param_state_fe <- df_res_state_fe %>% 
  mutate(id = row_number()) %>%
  left_join(param_grid_state_fe, by = c("id" = "job_id"))

# Create a grid of the contrasts I want to evaluate

# 2. Create grid of contrasts 
main_contrasts_state_fe_df = expand_grid(
       ref = c("observed"),
       counterfactual = c("may-2020", "jun-2020", "jul-2020", "jan-2021", "jul-2021")
  ) %>%
  mutate(id = row_number())

contrast_list_state_fe = list()

for (ii in 1:nrow(main_contrasts_state_fe_df)) {
  message("|----- CURRENTLY CHECKING ROW: ", ii , " ------|")
  row <-  main_contrasts_state_fe_df[ii,]
 
  tryCatch({ 
  main <- df_res_param_state_fe[df_res_param_state_fe$shift_label == row$counterfactual,
                         "res_list_state_fe"][[1]]
  }, error = function(e){
    message("Failed with this row ", ii, ". How? -> \n", e)
  }
  )
 tryCatch({
  ref <- df_res_param_state_fe[df_res_param_state_fe$shift_label == row$ref,
                      "res_list_state_fe"][[1]]
  
  }, error = function(e){
    ref <- NULL
    message("Failed this row ", ii, " How? -> \n", e)
  }
  )
  tryCatch({
  temp <- lmtp_contrast(main[[1]], ref = ref[[1]])
  contrast_list_state_fe[[ii]] <- tibble(temp$vals, num_observations = length(main[[1]]$id))
  
  }, error = function(e){
    
    contrast_list_state_fe[[ii]] <<- tibble(
      shift = NA,  ref = NA, std.error = NA, conf.low = NA,
      conf.high = NA, p.value = NA, num_observations = NA)
    message("Failed at estimating the contrast on this row",ii,". How? -> \n", e)
  }
  )
}

contrast_state_fe_df <- bind_rows(contrast_list_state_fe) %>%
  mutate(id = row_number()) %>% 
  left_join(main_contrasts_state_fe_df, by = "id")

View(contrast_state_fe_df)

saveRDS(contrast_state_fe_df, file = "03_results/01_contrasts/state_fe_contrasts.rds")

