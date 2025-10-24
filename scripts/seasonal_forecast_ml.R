################################################################################
# Seasonal Hydrologic Forecasts with WASS2SHydroR (AI Method)
# Clean, documented, and beginner-friendly script
################################################################################
# ---- Dependencies ----------------------------------------------------------
rm(list = ls())
# ==== PARAMETERS (participants only edit this block) ==========================
PATH_INPUTS <- "D:/CCR_AOS/Wass2sHydro-Training_base/outputs/PRCP_training_list_CIV_obs.rds"
data_by_products <- readRDS(PATH_INPUTS)
COUNTRY_CODE <- "CIV" # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PATH_COUNTRIES   <- "static/was_contries.shp"   # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <- "static/subbassins.shp"
PREDICTOR_VARS <-"PRCP"
PATH_OUTPUT <- "outputs"
MODELS <- c("mlp","mars","rf")
FINAL_FUSER <- "xgb"
update_github <- TRUE
dir.create(PATH_OUTPUT, showWarnings = FALSE)
fyear <- 2025
source("scripts/load_required_packages_frcst_v3.R")
#-------- 2) Run ML forecasts for each product group------------------------------
message("Running ML forecasts (per product) ...")

ml_results <- map(data_by_products, function(.x){
  pred_pattern_by_product <- as.vector(rep("^sst_",length(.x)))
  names(pred_pattern_by_product) <- names(.x)
  pred_pattern_by_product <- paste0("^", tolower(PREDICTOR_VARS),"_")
  wass2s_run_basins_ml(data_by_product = .x,
                       hybas_id = "HYBAS_ID",
                       pred_pattern_by_product =pred_pattern_by_product,
                       models = tolower(MODELS) ,
                       topK = 3,
                       min_kge_model =0 ,
                       grid_levels = 5,
                       prediction_years =c(fyear,fyear),
                       verbose_tune = FALSE,,
                       final_fuser = tolower(FINAL_FUSER),parallel = FALSE,workers = 4)
})

# fused_all <- map_dfr(names(ml_results), \(id) {
#   x <- ml_results[[id]]
#   if (!is.null(x[[1]]$fused_by_model)) {
#     x[[1]]$fused_by_model %>%
#       mutate(HYBAS_ID = id, .before = 1)
#   }
# })

fused_all <- imap(ml_results, ~ purrr::pluck(.x, 1, "fused_by_model", .default = NULL)) %>%
  discard(is.null) %>%
  imap_dfr(~ mutate(.x, HYBAS_ID = .y, .before = 1))


{
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_path <- file.path(PATH_OUTPUT,paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_seasonal_forecast_ml_", FINAL_FUSER, "_",timestamp,".rds"))
  file_path2 <- file.path(PATH_OUTPUT,paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_fused_ml_results_", FINAL_FUSER, "_",timestamp,".csv"))

  saveRDS(object =ml_results ,file = file_path )
  write.csv(fused_all, file_path2, row.names = FALSE)
  message("File saved: ", file_path)
}

#----------------- 3) Extract fused predictions per basin into a single tall tibble--------------------
message("Extracting fused predictions ...")
ml_results2 <- map(names(ml_results), function(id) {
  ml_results[[id]][[id]]
}) %>%
  set_names(names(ml_results))

stat_preds <- map2(ml_results2,names(ml_results2), function(.x,.y){
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

#--------------------- 4) Compute class probabilities per basin/year-----------------------------------
message("Computing class probabilities ...")
probabilities <- map(hybas_ids,~{
  ml_pred <- stat_preds %>%
    dplyr::filter(HYBAS_ID == .x)
  rr <- c(ml_pred$Q-ml_pred$pred)^2

  error_sd <- sd(c(ml_pred$Q-ml_pred$pred),na.rm = TRUE)
  error_rmse <- sqrt(mean(rr,na.rm = TRUE))

  proba <- WASS2SHydroR::wass2s_class_from_forecast(df = ml_pred,
                                                    q_hist = ml_pred$Q,
                                                    sigma =error_sd )
  return(proba)
}) %>% bind_rows()


yprobas <- probabilities %>%
  dplyr::filter(YYYY == fyear) %>%
  mutate(HYBAS_ID = as.factor(HYBAS_ID))

# ---- 5) Probability map -------------------------------------------------------
message("Building probability map ...")
proba_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                            data = yprobas,basin_col = "HYBAS_ID" )


print(proba_plot)

proba_plot <- proba_plot + ggspatial::annotation_north_arrow(
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
)+
  scale_fill_gradient(
    low = "#deebf7", high = "#08519c",
    name = "Probability",
    limits = c(0, 1)
  )

print(proba_plot)
## Sauvegarder le graphique

{
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_ml_probas_", FINAL_FUSER, "_", timestamp, ".png")
  ggsave(filename = filename,
         plot = proba_plot,
         path = PATH_OUTPUT,
         width = 9.5,
         height = 6.5,
         dpi = 600,
         bg = "white")
  message("Probabilities Map saved: ", filename)
}

# ---- 6) Class map (above/normal/below) ---------------------------------------
message("Building class map ...")
class_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                            data = yprobas,
                                            basin_col = "HYBAS_ID",
                                            type = "class" )
print(class_plot)

class_plot <- class_plot +
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

## Sauvegarder le graphique

{
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_ml_class_", FINAL_FUSER, "_", timestamp, ".png")
  ggsave(filename = filename,
         plot = class_plot,
         path = PATH_OUTPUT,
         width = 9.5,
         height = 6.5,
         dpi = 600,
         bg = "white")
  message("Probabilities Map saved: ", filename)
}

message("Done. Outputs saved to: ", normalizePath(PATH_OUTPUT, winslash = "/"))
