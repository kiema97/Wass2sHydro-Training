#------------------- 1) Clip subbasins by country polygon-----------------------------------
dir.create(PATH_OUTPUT, showWarnings = FALSE)
dir.create(file.path(PATH_OUTPUT,SHP_OUTPUT),showWarnings = FALSE,recursive = TRUE)

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
# country <- a_countries %>% filter(.data$GMI_CNTRY == COUNTRY_CODE)
# if (nrow(country) == 0) stop("No country with GMI_CNTRY == ", COUNTRY_CODE)
#

country <- a_countries
if(!is.null(COUNTRY_CODE)){
  country <- country %>% filter(.data$GMI_CNTRY == COUNTRY_CODE)
  if (nrow(country) == 0) stop("No country with GMI_CNTRY == ", COUNTRY_CODE)
}

# Intersections: subbasins partially or fully covered by the country polygon
inter_idx <- sf::st_intersects(a_subs, country, sparse = TRUE)
sel <- lengths(inter_idx) > 0
subs_sel <- a_subs[sel, ]

sf_basins <- sf::st_intersection(a_subs, country)%>%
  mutate(HYBAS_ID = as.factor(HYBAS_ID))




# Read shapefiles
a_rivers      <-safe_read_sf(PATH_RIVERS)
a_masque <- safe_read_sf(PATH_MASQUE)
a_outlets <- safe_read_sf(PATH_OUTLETS)
a_was <- safe_read_sf(PATH_WAS)
# Ensure same CRS
if (sf::st_crs(a_rivers) != sf::st_crs(a_subs)){
  a_rivers <- sf::st_transform(a_rivers, sf::st_crs(a_subs))
}

if (sf::st_crs(a_rivers) != sf::st_crs(a_masque)){
  a_masque <- sf::st_transform(a_masque, sf::st_crs(a_rivers))
}


if (sf::st_crs(a_rivers) != sf::st_crs(a_outlets)){
  a_outlets <- sf::st_transform(a_outlets, sf::st_crs(a_rivers))
}

sf_basins <- sf::st_intersection(a_subs, country)%>%
  mutate(HYBAS_ID = as.factor(HYBAS_ID))

subs_union <- a_subs %>%
  st_make_valid() %>%
  st_union() %>%
  st_as_sf()

sf_rivers_ <- sf::st_intersection(a_rivers, country)
sf_rivers <- sf::st_intersection(sf_rivers_, subs_union)

sf_masque <- sf::st_intersection(a_masque,country)
sf_outlets_ <- sf::st_intersection(a_outlets,country)
sf_outlets <- sf::st_intersection(sf_outlets_,subs_union)
if(!is.null(sf_masque)){
  sf_basins <- sf::st_intersection(sf_basins,sf_masque)
  sf_rivers <- sf::st_intersection(sf_rivers,sf_masque)
}

#------------------- 2)Forecast data processing -----------------------------------


ml_results  <- readRDS(PATH_ML_INPUTS)
stat_results <- readRDS(PATH_STAT_INPUTS)

# Helper
extract_fused <- function(x_list, hybas_id, out_col) {
  if (!hybas_id %in% names(x_list)) return(NULL)
  node <- x_list[[hybas_id]]
  if (is.null(node) || length(node) < 1 || is.null(node[[1]]$fused_by_model)) return(NULL)

  tbl <- node[[1]]$fused_by_model
  req_cols <- c("YYYY", "Q", "pred_final")
  if (!all(req_cols %in% names(tbl))) return(NULL)

  tbl %>%
    select(all_of(req_cols)) %>%
    rename(!!out_col := pred_final) %>%
    mutate(
      HYBAS_ID = hybas_id,
      YYYY = as.integer(YYYY)
    ) %>%
    relocate(HYBAS_ID, YYYY, Q)
}

HYBAS_IDs <- intersect(names(ml_results), names(stat_results))

# Build pieces (data.frame or NULL), drop NULLs, then bind
pieces <- map(HYBAS_IDs, function(id) {
  ai <- extract_fused(ml_results,  id, "METHOD_AI")
  st <- extract_fused(stat_results, id, "METHOD_STAT")

  # nothing available for this ID
  if (is.null(ai) && is.null(st)) return(NULL)

  # if only one side exists, return it directly
  if (is.null(ai)) return(st)
  if (is.null(st)) return(ai)

  # both exist -> join on full key
  inner_join(ai, st, by = c("HYBAS_ID", "YYYY", "Q"))
})

fused_data <- pieces %>%
  compact() %>%         # drop NULLs safely
  bind_rows() %>%
  arrange(HYBAS_ID, YYYY, Q)

message("Consolidating forecast from statistical et AI approach ...")

pred_cols_ref <- c("METHOD_AI", "METHOD_STAT")

consolidated_frcsts <- map_dfr(unique(fused_data$HYBAS_ID), function(.x) {

  df_basin_product <- fused_data %>%
    filter(HYBAS_ID == .x)
  # Keep only predictors that exist and have sd > 0 (and not all NA)
  predictors <- pred_cols_ref[pred_cols_ref %in% names(df_basin_product)]

  predictors <- predictors[
    vapply(predictors, function(p) {
      x <- df_basin_product[[p]]
      any(is.finite(x)) && stats::sd(x, na.rm = TRUE) > 0
    }, logical(1))
  ]

  # If one predictor is invalid/missing, use only the remaining one (no fusion)
  if (length(predictors) == 0) {
    return(tibble())  # or return rows with NA predictions if you prefer
  }

  fused_frcst <- wass2s_tune_pred_ml(
    df_basin_product = df_basin_product,
    predictors = predictors,      # <- 1 predictor => no fusion, 2 => fusion
    target = "Q",
    date_col = "YYYY",
    prediction_years = c(fyear, fyear),
    model = tolower(FINAL_FUSER)
  )

  fused_frcst$preds %>%
    right_join(
      df_basin_product %>% select(HYBAS_ID, YYYY),
      by = "YYYY"
    ) %>%
    select(HYBAS_ID, YYYY, Q, everything())
})

# consolidated_frcsts <- map(unique(fused_data$HYBAS_ID),function(.x){
#   df_basin_product <- fused_data %>%
#     dplyr::filter(HYBAS_ID==.x)
#   predictors <- intersect(colnames(df_basin_product),c("METHOD_AI" ,  "METHOD_STAT"))
#   fused_frcst <- wass2s_tune_pred_ml(df_basin_product = df_basin_product,
#                                      predictors = predictors,
#                                      target ="Q" ,
#                                      date_col = "YYYY",
#                                      prediction_years = c(fyear,fyear),
#                                      model = tolower(FINAL_FUSER))
#
#   final_frcst <- fused_frcst$preds %>%
#     dplyr::right_join(df_basin_product %>%
#                         dplyr::select(all_of(c("HYBAS_ID", "YYYY", "Q"))) ,
#                       by = "YYYY") %>%
#     dplyr::select(HYBAS_ID,YYYY,Q, everything())
#
# }) %>% bind_rows()


performances_metrics <- consolidated_frcsts %>%
  group_by(HYBAS_ID) %>%
  summarise(KGE = wass2s_kge(Q,pred),
            RMSE = wass2s_rmse(Q,pred),
            NSE = wass2s_nse(Q,pred),
            MAE = wass2s_mae(Q,pred),
            CORR =  wass2s_corr(Q,pred))
{
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_path_consolidated_frcsts <- file.path(PATH_OUTPUT,paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_seasonal_forecast_consolidated_", FINAL_FUSER, "_",timestamp,".csv"))
  file_path_performance <- file.path(PATH_OUTPUT,paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_seasonal_forecast_performances_", FINAL_FUSER, "_",timestamp,".csv"))

  write.table(x = performances_metrics,
              file =file_path_performance ,append =FALSE ,quote = FALSE,sep ="," ,row.names = FALSE)
  write.table(x = consolidated_frcsts,
              file = file_path_consolidated_frcsts,append =FALSE ,quote = FALSE,sep ="," ,row.names = FALSE)

  #saveRDS(object =consolidated_frcsts ,file = file_path )
  message("Consolidated forecast saved into : ", file_path_consolidated_frcsts)
  message("Forecast performances saved into : ", file_path_performance)
}



hybas_ids <- unique(consolidated_frcsts$HYBAS_ID)
message("Computing class probabilities ...")
probabilities <- map(hybas_ids,~{
  consolidated_frcst <- consolidated_frcsts %>%
    dplyr::filter(HYBAS_ID == .x)
  rr <- c(consolidated_frcst$Q-consolidated_frcst$pred)^2

  error_sd <- sd(c(consolidated_frcst$Q-consolidated_frcst$pred),na.rm = TRUE)
  error_rmse <- sqrt(mean(rr,na.rm = TRUE))

  proba <- WASS2SHydroR::wass2s_class_from_forecast(df = consolidated_frcst,
                                                    q_hist = consolidated_frcst$Q,
                                                    sigma =error_sd)
  return(proba)
}) %>% bind_rows()

{
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_path_proba <- file.path(PATH_OUTPUT,paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_seasonal_forecast_consolidated_probabilities_", FINAL_FUSER, "_",timestamp,".csv"))
  write.table(x = probabilities,file =file_path_proba ,append =FALSE ,quote = FALSE,sep ="," ,row.names = FALSE)


  message("Consolidated forecast probabilities saved into : ", file_path_proba)
}
