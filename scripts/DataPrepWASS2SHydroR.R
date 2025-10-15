###################################################################################
######              Data Prep for WASS2SHydroR                          ###########
###################################################################################

## Netoyage
rm(list = ls())


# ==== PARAMETERS (participants only edit this block) ==========================
COUNTRY_CODE <- "MLI" # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PREDICTOR_VARS <-"PRCP" # "PRCP", "SST"  # choose among available folders under predictors/
SELECTED_MODELS <- NULL  # e.g., c("CanCM3","CCSM4") or NULL to use all available
# Where things live (relative to project root)
PATH_COUNTRIES   <- "D:/CCR_AOS/WASS2SHydroRTraining/static/was_contries.shp"   # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <- "D:/CCR_AOS/WASS2SHydroRTraining/static/subbassins.shp"     # shapefile with HYBAS_ID field
PATH_HISTORICAL  <- "D:/CCR_AOS/Wass2sHydro-Training/data/was_subbassins_seasonal_data.csv" # columns: DATE, HYBAS_ID, Q, prcp, evap
PATH_PREDICTORS  <- "D:/CCR_AOS/Wass2sHydro-Training_base/predictors/PRCP/Jun-Sep/PRCP"             # expect subfolders: PRCP/, SST/
# Optional: performance/speed knobs
N_CORES <- 4#max(1, parallel::detectCores() - 1)


#=========== Configuration files ===================================================
source("scripts/helpers_dp.R")
source("scripts/processing.R")

# 1) Select the user's country and find covered subbasins
ggplot2::ggplot()+
  geom_sf(data=subs_sel)+
  geom_sf(data = country, fill="orange", alpha=0.2)+
  theme_minimal()

# 2) Load historical data for selected subbasins

hist_df <- readr::read_csv(PATH_HISTORICAL, show_col_types = FALSE) %>%
  #mutate(DATE = as.Date(.data$DATE)) %>%
  filter(.data$HYBAS_ID %in% HYBAS_IDS) %>%
  arrange(.data$HYBAS_ID, .data$DATE)

# Sanity check
hist_df %>% group_by(HYBAS_ID) %>% summarise(n = n(), .groups = "drop") %>% head(10)

# 3) Catalog available predictor files (PRCP / SST)

pred_catalog <- catalog_predictors(base_dir = PATH_PREDICTORS,
                                   vars_keep = PREDICTOR_VARS) %>%
  dplyr::filter(init_year==2025)
unique(pred_catalog$model)

# Filter by trainee choices
SELECTED_MODELS <- c("CCSM4","CESM1","CanSIPSIC4","METEOFRANCE9","SEAS51")
pred_catalog <- pred_catalog %>% filter(var %in% PREDICTOR_VARS)
if (!is.null(SELECTED_MODELS)) {
  pred_catalog <- pred_catalog %>% filter(model %in% SELECTED_MODELS)
}


training_list <- extract_predictors_nested(hybas_ids = unique(hist_df$HYBAS_ID),
                                           models = SELECTED_MODELS,
                                           hist_df = hist_df,
                                           predictor = PREDICTOR_VARS,
                                           predictors_root = PATH_PREDICTORS,
                                           subbasins_sf = subs_sel,
                                           init_year = 2025)


## 5) Quick sanity checks on the output list
# Show first subbasin's head
training_list_clean <- training_list |>
  map(~ keep(.x, ~ is.data.frame(.x) && NROW(.x) > 0)) |>
  discard(~ length(.x) == 0)

first_key <- names(training_list)[1]
if (!is.null(first_key)) {
  training_list[[first_key]] %>% head()
}


## 6) Save the prepared list for modeling
dir.create("outputs", showWarnings = FALSE)
saveRDS(training_list_clean, file = file.path("outputs", paste0("training_list_", COUNTRY_CODE, ".rds")))
message("Saved: ", file.path("outputs", paste0("training_list_", COUNTRY_CODE, ".rds")))


