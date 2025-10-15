################################################################################
# Seasonal Hydrologic Forecasts with WASS2SHydroR (Statistical Method)
# Clean, documented, and beginner-friendly script
################################################################################
# ---- Dependencies ----------------------------------------------------------
safeload <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Missing packages: ", paste(miss, collapse = ", "), call. = FALSE)
}
safeload(c("WASS2SHydroR", "sf", "dplyr", "purrr", "ggplot2", "ggspatial", "readr", "stringr", "tibble"))


# ==== PARAMETERS (participants only edit this block) ==========================
PATH_INPUTS <- "outputs/training_list_CIV.rds"
data_by_products <- readRDS(PATH_INPUTS)
COUNTRY_CODE <- "CIV" # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PATH_COUNTRIES   <- "D:/CCR_AOS/WASS2SHydroRTraining/static/was_contries.shp"   # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <- "D:/CCR_AOS/WASS2SHydroRTraining/static/subbassins.shp"
PATH_OUTPUT <- "outputs"
dir.create(PATH_OUTPUT, showWarnings = FALSE)
fyear <- 2025

#------------------- 1) Clip subbasins by country polygon-----------------------------------

# Read shapefiles
a_countries <- sf::st_read(PATH_COUNTRIES, quiet = TRUE) %>%
  sf::st_make_valid()
a_subs      <- sf::st_read(PATH_SUBBASINS, quiet = TRUE) %>%
  sf::st_make_valid()

# Ensure same CRS
if (sf::st_crs(a_countries) != sf::st_crs(a_subs)) {
  a_subs <- sf::st_transform(a_subs, sf::st_crs(a_countries))
}

# Filter country
country <- a_countries %>% filter(.data$GMI_CNTRY == COUNTRY_CODE)
if (nrow(country) == 0) stop("No country with GMI_CNTRY == ", COUNTRY_CODE)

# Intersections: subbasins partially or fully covered by the country polygon
inter_idx <- sf::st_intersects(a_subs, country, sparse = TRUE)
sel <- lengths(inter_idx) > 0
subs_sel <- a_subs[sel, ]

sf_basins <- sf::st_intersection(a_subs, country)%>%
  mutate(HYBAS_ID = as.factor(HYBAS_ID))


#-------- 2) Run statistical forecasts for each product group------------------------------
message("Running statistical forecasts (per product) ...")
stats_results <- map2(data_by_products, names(data_by_products), function(.x,.y){
  pred_pattern_by_product <- as.vector(rep("^prcp_",length(.x)))
  names(pred_pattern_by_product) <- names(.x)
  frcst <- WASS2SHydroR::wass2s_run_basins_stat(data_by_product = .x,
                                                hybas_id ="HYBAS_ID",
                                                pred_pattern_by_product  = pred_pattern_by_product,
                                                final_fuser = "rf",
                                                grid_levels = 10,  min_kge_model =-1 )
  names(frcst) <- .y
  return(frcst)
})

saveRDS(object =stats_results ,file = file.path(PATH_OUTPUT,"seasonal_forecast_stat.rds"))
#----------------- 3) Extract fused predictions per basin into a single tall tibble--------------------
message("Extracting fused predictions ...")
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

#--------------------- 4) Compute class probabilities per basin/year-----------------------------------
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

# ---- 5) Probability map -------------------------------------------------------
message("Building probability map ...")
proba_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                            data = yprobas,basin_col = "HYBAS_ID" )


print(proba_plot)

proba_plot <- proba_plot + annotation_north_arrow(
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
## Sauvegarder le graphique
ggsave(filename = paste0(COUNTRY_CODE,"_",fyear,"_stat_probas.png"),
       plot = proba_plot,
       path = PATH_OUTPUT,
       width = 9.5,
       height = 6.5, dpi = 600,
       bg = "white")


# ---- 6) Class map (above/normal/below) ---------------------------------------
message("Building class map ...")
class_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                            data = yprobas,
                                            basin_col = "HYBAS_ID",
                                            type = "class",
                                            colors =palette_colors )
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
ggsave(filename = paste0(COUNTRY_CODE,"_",fyear,"_stat_class.png"),
       plot = class_plot,
       path = PATH_OUTPUT,
       width = 9.5,
       height = 6.5,
       dpi = 600,
       bg = "white")
message("Done. Outputs saved to: ", normalizePath(PATH_OUTPUT, winslash = "/"))
