################################################################################
# Seasonal Hydrologic Forecasts with WASS2SHydroR (Statistical Method)
# Clean, documented, and beginner-friendly script
################################################################################

# ==============================================================================
# 1) CONFIGURATION (participants edit only this section)
# ==============================================================================

PATH_INPUTS <- "data/SST_WAS_TRAINING_DATA_v2.rds"
COUNTRY_CODE <- "GHA" # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PATH_COUNTRIES   <- "static/was_contries.shp"   # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <- "static/subbassins.shp"
PREDICTOR_VARS <-"SST"
pred_pattern_by_product <- "^(prcp|sst)"
PATH_OUTPUT <- "outputs_v2"
FINAL_FUSER <- "rf"
update_github <- TRUE
dir.create(PATH_OUTPUT, showWarnings = FALSE)
fyears <- c(2020,2025)
fyear <- 20260101

# ==============================================================================
# 2) LOAD PACKAGES AND DATA
# ==============================================================================
data_by_products <- readRDS(PATH_INPUTS)
workers <- min(length(data_by_products),max(future::availableCores()-2,1))
source("scripts/load_required_packages_frcst_v3.R")

# ==============================================================================
# 3) RUN STATISTICAL FORECASTS
# ==============================================================================
with_progress({
  p <- progressor(along = data_by_products)
  stats_results <- future_map(
    data_by_products,
    function(.x) {
      # incrémente la progression à chaque bassin terminé
      p(sprintf("Done: %s", .x$HYBAS_ID[1] %||% ""))
      WASS2SHydroR::wass2s_run_basins_stat(data_by_product = .x,
                                           hybas_id ="HYBAS_ID",
                                           pred_pattern_by_product  = pred_pattern_by_product,
                                           final_fuser = FINAL_FUSER,
                                           grid_levels = 20,
                                           topK = 1,
                                           quiet = FALSE,
                                           min_kge_model =-Inf,
                                           use_sub_fuser =FALSE,
                                           prediction_years =fyears,
                                           init_frac = 0.8,
                                           assess_frac = 0.2,
                                           cumulative = TRUE)
    },
    .options = furrr_options(seed = TRUE)
  )
})

plan(sequential)

# ==============================================================================
# 4) EXTRACT FUSED PREDICTIONS
# ==============================================================================

message("Extracting fused predictions ...")

fused_all <- imap(stats_results, ~ purrr::pluck(.x, 1, "fused_by_model", .default = NULL)) %>%
  discard(is.null) %>%
  imap_dfr(~ mutate(.x, HYBAS_ID = .y, .before = 1))


stats_results2 <- map(names(stats_results), function(id) {
  stats_results[[id]][[id]]
}) %>%
  set_names(names(stats_results))

stat_preds <- map2(stats_results2,names(stats_results2), function(.x,.y){
  fused_by_model <- .x$fused_by_model

  if(!all(c("YYYY","Q" ,"pred_final") %in% names(fused_by_model))){
    fused_by_model <- data.frame()
    return(fused_by_model)
  }
  fused_by_model <- fused_by_model %>%
    dplyr::select("YYYY","Q" ,"pred_final") %>%
    rename(pred = pred_final) %>%
    mutate(HYBAS_ID = .y)
}) %>% bind_rows()

hybas_ids <- unique(stat_preds$HYBAS_ID)

# ==============================================================================
# 5) COMPUTE PROBABILITIES
# ==============================================================================

message("Computing class probabilities ...")
probabilities <- map(hybas_ids,~{
  stat_pred <- stat_preds %>%
    dplyr::filter(HYBAS_ID == .x)
  rr <- c(stat_pred$Q-stat_pred$pred)^2

  error_sd <- sd(c(stat_pred$Q-stat_pred$pred),na.rm = TRUE)
  error_rmse <- sqrt(mean(rr,na.rm = TRUE))

  proba <- WASS2SHydroR::wass2s_class_from_forecast(df = stat_pred,
                                                    q_hist = stat_pred$Q,
                                                    sigma =error_sd )
  return(proba)
}) %>% bind_rows()

yprobas <- probabilities %>%
  dplyr::filter(YYYY == fyear) %>%
  mutate(HYBAS_ID = as.factor(HYBAS_ID))

# ==============================================================================
# 6) SAVE NUMERIC OUTPUTS
# ==============================================================================
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

file_rds <- file.path(
  PATH_OUTPUT,
  paste0(PREDICTOR_VARS, "_seasonal_forecast_stat_", FINAL_FUSER, "_", timestamp, ".rds")
)

saveRDS(stats_results, file_rds)

file_csv <- file.path(
  PATH_OUTPUT,
  paste0(COUNTRY_CODE, "_", PREDICTOR_VARS, "_statistic_probabilities_", FINAL_FUSER, "_", timestamp, ".csv")
)

write.csv(probabilities, file_csv, row.names = FALSE)


file_fused <- file.path(PATH_OUTPUT,paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_fused_stat_results_", FINAL_FUSER, "_",timestamp,".csv"))
write.csv(fused_all, file_fused, row.names = FALSE)

message("Numeric outputs saved.")

# ==============================================================================
# 7) BUILD MAPS
# ==============================================================================

message("Building probability map ...")
proba_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                            data = yprobas,basin_col = "HYBAS_ID" ) + annotation_north_arrow(
  location = "tr",
  which_north = "true",
  style = north_arrow_fancy_orienteering,
  height = unit(1.2, "cm"),
  width = unit(1.2, "cm"),
  pad_x = unit(-0.1, "cm"),
  pad_y = unit(0.1, "cm")
)+ annotation_scale(
  location = "br",
  width_hint = 0.3
)+
  scale_fill_gradient(
    low = "#deebf7", high = "#08519c",
    name = "Probability",
    limits = c(0, 1)
  )

print(proba_plot)


message("Building class map ...")
class_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                            data = yprobas,
                                            basin_col = "HYBAS_ID",
                                            type = "class") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5) )+
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    pad_x = unit(-0.1, "cm"),
    pad_y = unit(0.1, "cm")
  )+ annotation_scale(
    location = "br",
    width_hint = 0.3
  )

print(class_plot)

# ==============================================================================
# 8) SAVE MAPS
# ==============================================================================
filename_proba <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_stat_probas_", FINAL_FUSER, "_", timestamp, ".png")
ggsave(filename = filename_proba,
       plot = proba_plot,
       path = PATH_OUTPUT,
       width = 9.5,
       height = 6.5,
       dpi = 600,
       bg = "white")


filename_class <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_stat_class_", FINAL_FUSER, "_", timestamp, ".png")
ggsave(filename = filename_class,
         plot = class_plot,
         path = PATH_OUTPUT,
         width = 9.5,
         height = 6.5,
         dpi = 600,
         bg = "white")


message("Done. Outputs saved to: ", normalizePath(PATH_OUTPUT, winslash = "/"))
