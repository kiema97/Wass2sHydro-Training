###################################################################################
######              Data Prep for WASS2SHydroR                          ###########
###################################################################################

## Netoyage
rm(list = ls())
# ==== PARAMETERS (participants only edit this block) ==========================
COUNTRY_CODE <- "CIV" # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PREDICTOR_VARS <-"SST" # "PRCP", "SST"  # choose among available folders under predictors/
SELECTED_MODELS <- NULL  # e.g., c("CanCM3","CCSM4") or NULL to use all available
# Where things live (relative to project root)
PATH_COUNTRIES   <- "static/was_contries.shp"  # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <-"D:/CCR_AOS/Wass2sHydro-Training_base/static/subbassins_ci.shp" #"static/subbassins.shp"     # shapefile with HYBAS_ID field
PATH_HISTORICAL  <-"D:/CCR_AOS/Wass2sHydro-Training_base/data/CIV/was_subbassins_seasonal_data_ci_v2.csv" #"data/was_subbassins_seasonal_data.csv" # columns: DATE, HYBAS_ID, Q, prcp, evap
PATH_PREDICTORS  <- "predictors"             # expect subfolders: PRCP/, SST/
PATH_OUTPUT <- "outputs"
update_github <- TRUE
FIELD_SEPERATOR <- ","
MISSING_VALUE_CODE <-  "-999"
HISTORICAL_DATA_ID_COL <- "HYBAS_ID"
FYEAR <- 2025
# Optional: performance/speed knobs
N_CORES <- 4#max(1, parallel::detectCores() - 1)


#=========== Configuration files ===================================================
source("scripts/helpers_dp_v3.R")
source("scripts/processing_v3.R")

# 1) Select the user's country and find covered subbasins
ggplot2::ggplot()+
  geom_sf(data=subs_sel)+
  geom_sf(data = country, fill="orange", alpha=0.2)+
  theme_minimal()+
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    pad_x = unit(-0.1, "cm"),
    pad_y = unit(0.1, "cm")
  )+ ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.3
  )

# 2) Load historical data for selected subbasins
hist_df <- read_historical_df_yearly(path = PATH_HISTORICAL,
                                     sep = FIELD_SEPERATOR,
                                     id_col ="HYBAS_ID",
                                     hybas_ids = HYBAS_IDS,
                                     missing_value_code =MISSING_VALUE_CODE,
                                     check_warn =  TRUE,fyear = FYEAR) %>%
  rename(DATE = YYYY) %>%
  distinct(HYBAS_ID, DATE, .keep_all = TRUE)

# Sanity check
hist_df %>% group_by(HYBAS_ID) %>% summarise(n = n(), .groups = "drop") %>% head(10)

# 3) Catalog available predictor files (PRCP / SST)

pred_catalog <- catalog_predictors(base_dir = PATH_PREDICTORS,
                                   vars_keep = PREDICTOR_VARS) %>%
  dplyr::filter(init_year==FYEAR)
unique(pred_catalog$model)

# Filter by trainee choices
SELECTED_MODELS <- c("CCSM4","CFSv2","CanSIPSIC4","METEOFRANCE9",
                     "SEAS51","SPSv3p5","SEAS51c")
pred_catalog <- pred_catalog %>% filter(var %in% PREDICTOR_VARS)
if (!is.null(SELECTED_MODELS)) {
  pred_catalog <- pred_catalog %>% filter(model %in% SELECTED_MODELS)
}
sum(unique(hist_df$HYBAS_ID)%in%unique(subs_sel$HYBAS_ID))

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
dir.create(PATH_OUTPUT, showWarnings = FALSE)
savePath <- file.path(PATH_OUTPUT, paste0(PREDICTOR_VARS,"_training_list_", COUNTRY_CODE, "_obs.rds"))
saveRDS(training_list_clean, file =savePath )
message("Saved: ",savePath)


