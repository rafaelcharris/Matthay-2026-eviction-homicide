############################################################################################
# DATE CREATED: Jul 1st 2026
# PROJECT:      Eviction Moratoria
# PROGRAMMER:   Rafael Charris
# RESEARCHER:   Ellicott Matthay
# PURPOSE:      Run the grid of TMLE models on Big Purple for the city level analysis only
############################################################################################
rm(list = ls())

library(lmtp, lib.loc = "/gpfs/data/matthe01lab/01_EvictionMoratoria/01_code") 
library(progressr)
library(tidyverse)
library(SuperLearner)

options(warn = 1)
setwd("/gpfs/data/matthe01lab/01_EvictionMoratoria/")

# Get the data from the bash script
task_id <- as.integer(Sys.getenv("TASK_ID"))
grid_name <- Sys.getenv("GRID_NAME")
#grid_name <- "grid_off_after"

# ---------- helpers to construct time-varying variable lists ----------
#' Create wrapper around glmnet to use ridge and reduce the number of crossvalidation folds to 5
#' 
SL.ridge.5fold <- function(...){
  SL.glmnet(..., nfolds = 5, alpha = 0)  # reduce from default 10
}

#' Create the list of treatment covariates
#' @param first_period:
#' @param end_period:
#' @param statetimevarying: description
#' @param outcomestate: description
#' @param cdc_moratoria: description
build_state_trt_list <- function(first_period, end_period, statetimevarying, outcomestate, cdc_moratoria) {
  statevars <- lapply(first_period:(end_period - 1), function(i) {
    if (i < 9) {
      c(
        paste0("paste0(statetimevarying, rep('", i, "', length(statetimevarying)))"),
        paste0("paste0(outcomestate, '", i, "')")
      )
    } else if (i <= 19 & i >= 9) {
      c(
        paste0("paste0(statetimevarying, rep('", i, "', length(statetimevarying)))"),
        paste0("paste0(outcomestate, '", first_period:i, "')"),
        paste0("paste0(cdc_moratoria, ", i, ")")
      )
    } else {
      c(
        paste0("paste0(statetimevarying, rep('", i, "', length(statetimevarying)))"),
        paste0("paste0(outcomestate, '", first_period:i, "')")
      )
    }
  })

  trt_list <- lapply(statevars, function(x) {
    unlist(lapply(x, \(s) eval(parse(text = s))))
  })
  trt_list
}

#' Create list for the outcome model
#' @param first_period:
#' @param end_period:
#' @param ctytimevarying: description
#' @param outcomecty: description
#' @param cdc_moratoria: description
build_cty_outcome_list <- function(first_period, end_period, ctytimevarying, outcomecty, cdc_moratoria) {
  ctyvars <- lapply(first_period:(end_period - 1), function(i) {
    if (i < 9) {
      c(
        paste0("paste0(ctytimevarying, rep('", i, "', length(ctytimevarying)))"),
        paste0("paste0(outcomecty, '", i, "')")
      )
    } else if (i <= 19 & i >= 9) {
      c(
        paste0("paste0(ctytimevarying, rep('", i, "', length(ctytimevarying)))"),
        paste0("paste0(outcomecty, '", first_period:i, "')"),
        paste0("paste0(cdc_moratoria, ", i, ")")
      )
    } else {
      c(
        paste0("paste0(ctytimevarying, rep('", i, "', length(ctytimevarying)))"),
        paste0("paste0(outcomecty, '", first_period:i, "')")
      )
    }
  })

  outcome_list <- lapply(ctyvars, function(x) {
    unlist(lapply(x, \(s) eval(parse(text = s))))
  })
  outcome_list
}

#' Create the time varying list of covariates 
#' @param first_period:
#' @param end_period:
#' @param statetimevarying: description
#' @param ctytimevarying: description
#' @param outcomestate: description
#' @param outcomecty: description
#' @param cdc_moratoria: description
build_timevary_lists <- function(end_period, first_period, statetimevarying, outcomestate,
                                ctytimevarying, outcomecty, cdc_moratoria) {

  num_timepoints <- end_period - first_period

  trt_list      <- build_state_trt_list(first_period, end_period, statetimevarying, outcomestate, cdc_moratoria)
  outcome_list  <- build_cty_outcome_list(first_period, end_period, ctytimevarying, outcomecty, cdc_moratoria)
  cens_list     <- vector("list", length = num_timepoints) # all NULL

  list(
    trt     = trt_list,
    cens    = cens_list,
    outcome = outcome_list
  )
}

#' Check missing volumns 
#' @param allvars: list of treatment, outcome and exposure variables
#' @param df_full:
check_missing_columns <- function(allvars, df_full) {
  df_columns <- colnames(df_full)
  check_columns <- function(sublist, df_columns) {
    setdiff(unlist(sublist), df_columns)
  }
  lapply(allvars, check_columns, df_columns = df_columns)
}

#' Get the shifted dataset or label 
#' @param shift_label:
# @param policy: this is the original argument. I adapted it on July 21st 
#' @param subgroup:
get_shift_fun <- function(shift_label,  policy, subgroup = NULL) { # if running subgroups use subgroup as an argument, add policy instead
  if (shift_label == "always_lift1"){
    list(shift_fun = static_binary_on, df_shifted = NULL)
  } else if (shift_label == "always_lift0") {
    list(shift_fun = static_binary_off, df_shifted = NULL)
  } else if (shift_label == "observed") {
    list(shift_fun = NULL, df_shifted = NULL)
  } else{
  
    message("policy: ", policy)
    
    # If am not using any subgroups, then just return the normal shifted dataset
    if (is.null(subgroup)) {
   
    df_shifted <- readRDS(str_c('02_data/01_analytic/eviction_dataset_wide_off_after_', shift_label, "_", policy,  ".rds")) %>%
      mutate(county = as.numeric(county))
    } else if (subgroup %in% c("stages1_2", "stages3_5", "state_fe", "exclude_two_months")) {
      
    #!  I can add here all the other subgroups
      
      df_shifted <- readRDS(str_c('02_data/01_analytic/eviction_dataset_wide_off_after_', shift_label, "_", policy,  ".rds")) %>%
      mutate(county = as.numeric(county))
    
      stages1_2 <- c(
      2,  6 , 8, 9,  
      10,   12 , 15,  17,  18, 19, 21,   25,   26,  27,  30,  37, 32 ,  
      36, 33, 41, 42, 44, 45, 50,  53, 55  )
      
      # states with moratoria duration of 2 or less
      short_duration_states <- c(
        54, 55, 49, 48, 47,
        45, 31, 38, 30,
        28, 16, 19, 1)
      
     states_names <- c( 
       "West Virginia", "Wisconsin", "Utah", "Texas", "Tennessee",
        "South Carolina", "Nebraska", "North Dakota", "Montana",
        "Mississippi", "Idaho", "Iowa", "Alabama"
      )
      
      if (subgroup == "stages1_2"){
      df_shifted <- df_shifted %>%
        filter(state_fips %in% stages1_2) 
      }  
    
      if (subgroup == "stages3_5"){
      
      moratoria_states <- unique(df_shifted$state_fips)
      stages3_5 <- setdiff(moratoria_states, stages1_2)
      df_shifted <- df_shifted %>%
        filter(state_fips %in% stages3_5) 
      }  
    
      if (subgroup == "state_fe"){
      
        df_shifted <- fastDummies::dummy_cols(df_shifted, select_columns = 'state_fips') 
        
      }
     
     if (subgroup == "exclude_two_months"){
       
      df_shifted <- df_shifted %>%
        filter(!c(state_fips %in% short_duration_states))
      
     }
      
    } else if (subgroup %in% c("hisp")) {
      
      df_shifted <- readRDS(str_c('02_data/01_analytic/eviction_dataset_wide_off_after_', shift_label, "_", subgroup,  ".rds")) %>%
        mutate(county = as.numeric(county))
      
    } else 
      
      {
        # st_with_variation or 5000, 50.000
           df_shifted <- readRDS(str_c('02_data/01_analytic/eviction_dataset_wide_off_after_', shift_label, "_sens_", subgroup,  ".rds")) %>%
      mutate(county = as.numeric(county))
    
      }

    list(shift_fun = NULL, df_shifted = df_shifted)
  }
}

#' Run a single tmle run 
#' @param param_row
#' @param statebaseline
#' @param statetimevarying
#' @param ctybaseline
#' @param ctytimevarying
#' @param outcomestate
#' @param outcomecty
#' @param df_shifted
#' @param cdc_moratoria
#' @param shift_fun
#' @param use_progress
run_lmtp <- function(params_row,
                    statebaseline,
                    statetimevarying,
                    ctybaseline,
                    ctytimevarying,
                    outcomestate,
                    outcomecty,
                    df_shifted = NULL,
                    cdc_moratoria = NULL,
                    shift_fun = NULL,
                    use_progress = FALSE) {

  # params_row: one-row tibble / data.frame
  # expects at least columns: period, k, outcome, policy, shift_label, mtp
  df_full <- readRDS(str_c("02_data/01_analytic/", params_row$df)) %>%
      mutate(county = as.numeric(county))
  
  first_period     <- params_row$first_period
  end_period     <- params_row$end_period
  k          <- params_row$k
  folds      <- params_row$folds
  outcome    <- params_row$outcome   # e.g. "ipv_cty_rate"
  policy     <- params_row$policy    # e.g. "lift_moratoria_obin"
  shift_lab  <- params_row$shift_label
  mtp_val    <- params_row$mtp
  subgroup   <- params_row$subgroup
  learners   <- unlist(params_row$learners)

  message(
    str_c("\n|============ Running spec: period=", end_period,
          " | outcome=", outcome,
          " | policy=", policy,
          " | shift=", shift_lab,
          " | mtp=", mtp_val, " ===========|")
  )

  stages1_2 <- c(
    2,  6 , 8, 9,  
    10,   12 , 15,  17,  18, 19, 21,   25,   26,  27,  30,  37, 32 ,  
    36, 33, 41, 42, 44, 45, 50,  53, 55  )
  
  short_duration_states <- c(
    54, 55, 49, 48, 47,
    45, 31, 38, 30,
    28, 16, 19, 1)
  
  if (subgroup == "exclude_two_months"){
    
    df_full <- df_full %>%
      filter(!c(state_fips %in% short_duration_states))
  }
  
  if (subgroup == "stages1_2"){
   df_full <- df_full %>%
      filter(state_fips %in% stages1_2) 
  }  
  
  if (subgroup == "stages3_5"){
    
    moratoria_states <- unique(df_full$state_fips)
    stages3_5 <- setdiff(moratoria_states, stages1_2)
    df_full <- df_full %>%
      filter(state_fips %in% stages3_5) 
  } 

  # build exposure (trt) names: policy5 ... policy{period}
  trt <- names(df_full[, paste0(policy, (first_period + 1 ):end_period)])

  if (subgroup == "state_fe"){
    
    # Add State Fixed Effects
    df_full <- fastDummies::dummy_cols(df_full, select_columns = 'state_fips') 
    
    state_fe <- df_full %>%
      select(matches("state_fips")) %>%
      names()
    
    # originally the state fixed effects where at the outcome model, which are at the county level.
    mybaseline <- list(trt = c(statebaseline, state_fe),
                       cens = NULL, 
                       outcome = c(ctybaseline, state_fe)) 
    
  } else {
  # baseline lists
  mybaseline <- list(trt = statebaseline, cens = NULL, outcome = ctybaseline)

  }
  # time-varying structure
  allvars <- build_timevary_lists(
    first_period     = first_period,
    end_period       =  end_period,
    statetimevarying = statetimevarying,
    outcomestate     = outcomestate,
    ctytimevarying   = ctytimevarying,
    outcomecty       = outcomecty,
    cdc_moratoria    = cdc_moratoria
  )

  # optionally check missing cols
  message("\n|================= Missing columns check =================|")
  miss <- check_missing_columns(allvars, df_full)
  print(miss)

  # full outcome var (e.g. outcome="ipv_cty_rate", period=10 -> "ipv_cty_rate10")
  outcome_var <- paste0(outcome, end_period)

  message("\n|================= Running lmtp_tmle =================|")
  start_time <- Sys.time()
  message("Start time: ", start_time)

  res <- tryCatch({
    
    lmtp_tmle(
      data             = ungroup(df_full),
      trt              = trt,
      outcome          = outcome_var,
      baseline         = mybaseline,
      time_vary        = allvars,
      cens             = NULL,
      shift            = shift_fun,
      shifted          = df_shifted, 
      id               = "state_fips",
      outcome_type     = "continuous",
      learners_outcome = learners,
      learners_trt     = learners,
      folds            = folds,
      k                = k,
      mtp              = mtp_val
    )
  }, error = function(e) {
        message("=== ERROR REPORT ===")
  message("Error message: ", e$message)
  message("Task ID: ", task_id)
  message("Job params:")
  message("  policy:          ", policy)
  message("  counterfactual:  ", shift_lab)
  message("  outcome:         ", outcome)
  message("  end_period:      ", end_period)
  message("  folds:           ", folds)
  message("  k:               ", k)
  message("  mtp:             ", mtp_val)
  message("  learners:        ", learners)
  message("Full traceback:")
  # This is key on a server
  message(paste(capture.output(traceback()), collapse = "\n"))
  return(NULL)
    })


  end_time <- Sys.time()
  dif <- round(difftime(end_time, start_time, units = "mins"), 2)
  timestamp <- format(end_time, "%Y-%m-%d_%H:%M")

  if (!is.null(res)) {
    # attach covariate info
    res[['vals']]$state_baseline  <- statebaseline
    res[['vals']]$state_varying   <- statetimevarying
    res[['vals']]$county_baseline <- ctybaseline
    res[['vals']]$county_varying  <- ctytimevarying
    res[['vals']]$my_shift_fun  <- shift_fun

    # save file name
    fname <- str_c(
      "03_results/task_", task_id,"_grid_name",
      "_", params_row$grid_name, ".rds"
    )
    tryCatch({
      saveRDS(res, fname)
      message("Successfully saved results to ", fname, " (took ", dif, " mins)")
    }, error = function(e) {
      message("ERROR saving results (period ", end_period, ", shift=", shift_lab, "): ", e$message)
    })
  } else {
    message("Model failed; no results saved for period ", end_period, ", shift=", shift_lab)
  }

  list(
    result       =  res,
    time_min     = dif,
    timestamp    = timestamp,
    end_period   = end_period,
    first_period = first_period,
    outcome      = outcome,
    policy       = policy,
    shift        = shift_lab,
    mtp          = mtp_val,
    learner      = learners
  )
}


# assume df_full already read

# covariates (same as your script)
statebaseline <- c(#"rest_score19", 
		   "pct_demvotes_state2016",
		   'pct_overcrowding_state',
  'density_state', 'perc_female_state', 'perc_under18_state', 'perc_18_34_state',
  'perc_35_64_state', 'perc_65_over_state', 'perc_white_non_h_state',
  'perc_black_non_h_state','perc_hispanic_state', 'median_income_state', 'gini_state',
  'perc_poverty_state','per_pophealthins_state', 'perc_renterhu_state',
  'perc_bachelors_state','perc_high_school_grad_state', 'perc_ssi_state',
  'perc_hhpai_state','perc_construction_state', 'perc_management_state',
  'perc_sales_state', 'perc_service_state','pct_rent30to49_state',
  "pct_rent50plus_state")

statetimevarying <- c(
  'administered_dose1_pop_pct_state', 'medcomtranscat_state',
  'pv_rate_state', "covidratenew_state", "coviddeathratenew_state",
  "bar_open", "lausunemp_meanstate", "econimpactindex_median",
  "deliver_to_homes_offpremise", "government_response_index_average"
)

ctybaseline <- c("pct_demvotes_cty2016", "metro", 
  'pct_rent30to49_cty', 'pct_rent50plus_cty','density_cty', 'perc_female_cty',
  'perc_under18_cty', 'perc_18_34_cty', 'perc_35_64_cty', 'perc_65_over_cty',
  'perc_white_non_h_cty', 'perc_black_non_h_cty', 'perc_hispanic_cty',
  'median_income_cty', 'gini_cty', 'perc_poverty_cty', 'per_pophealthins_cty',
  'perc_renterhu_cty', 'perc_bachelors_cty', 'perc_high_school_grad_cty',
  'perc_ssi_cty','perc_hhpai_cty','perc_construction_cty', 'perc_management_cty',
  'perc_sales_cty','perc_service_cty', "pct_overcrowding_cty")

ctytimevarying <- c('covidratenew_cty', 'coviddeathratenew_cty','cares_act',
                    'medcomtranscat_cty', 'econimpactindex_cty', 'lau_unemprate',
                    "administered_dose1_pop_pct_cty")

outcomestate <- c("ipv_state_rate")
outcomecty   <- c("ipv_cty_rate")

cdc_moratoria <- c("cdc_moratoria")

# subgroup label (like your original)
subgroup <- "_"
if (subgroup == "_") subgroup <- "all"

# Load parameter grid

#grid_name = "grid_notopeif_alternative_exposure_coding"

param_grid <- readRDS(str_c("02_data/00_auxiliary/", grid_name, ".rds"))
param_grid$grid_name <- str_replace(grid_name,"grid_", "")

params_row <- param_grid %>% filter(job_id == task_id)
if (nrow(params_row) != 1) {
  stop("Invalid TASK_ID: ", task_id)
}

res <- get_shift_fun(params_row$shift_label, params_row$policy, params_row$subgroup)
sf <- res$shift_fun
df_shifted <- res$df_shifted

result <- run_lmtp(
  params_row     = params_row,
  statebaseline  = statebaseline,
  statetimevarying = statetimevarying,
  ctybaseline    = ctybaseline,
  ctytimevarying = ctytimevarying,
  outcomestate   = outcomestate,
  outcomecty     = outcomecty,
  df_shifted     = df_shifted,
  cdc_moratoria  = cdc_moratoria,
  shift_fun      = sf,
  use_progress   = FALSE
)
