################################################################################
# Seasonal Hydrologic Forecasts with WASS2SHydroR (AI Method)
# Clean, documented, and beginner-friendly script
################################################################################
# ---- Dependencies ----------------------------------------------------------
rm(list = ls())
# ==== PARAMETERS (participants only edit this block) ==========================
PATH_ML_INPUTS <- "outputs/BFA_SST_seasonal_forecast_ml_xgb_20251023_011940.rds"
PATH_STAT_INPUTS <- "outputs/BFA_SST_seasonal_forecast_ml_xgb_20251023_011940.rds"
COUNTRY_CODE <- "CIV" # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PATH_COUNTRIES   <- "static/was_contries.shp"   # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <- "static/subbassins.shp"
PREDICTOR_VARS <-"PRCP"
PATH_OUTPUT <- "outputs"
FINAL_FUSER <- "rf"
update_github <- TRUE
fyear <- 2025
source("scripts/load_requirement.R")
source("scripts/fused_data_processing.R")

#--------------------- 4) Compute class probabilities per basin/year-----------------------------------
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
  filename <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_FINAL_HYDROLOGICAL_FORECAST_", toupper(FINAL_FUSER), "_", timestamp, ".png")
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
  filename <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_FINAL_HYDROLOGICAL_FORECAST_", toupper(FINAL_FUSER), "_", timestamp, ".png")
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

