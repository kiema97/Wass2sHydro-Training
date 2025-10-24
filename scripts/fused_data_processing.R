#------------------- 1) Clip subbasins by country polygon-----------------------------------
dir.create(PATH_OUTPUT, showWarnings = FALSE)
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
      YYYY = as.integer(YYYY),
      Q    = suppressWarnings(as.integer(Q))  # assure consistent type
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
consolidated_frcsts <- map(unique(fused_data$HYBAS_ID),function(.x){
  df_basin_product <- fused_data %>%
    dplyr::filter(HYBAS_ID==.x)
  predictors <- intersect(colnames(df_basin_product),c("METHOD_AI" ,  "METHOD_STAT"))
  fused_frcst <- wass2s_tune_pred_ml(df_basin_product = df_basin_product,
                                     predictors = predictors,
                                     target ="Q" ,
                                     date_col = "YYYY",
                                     prediction_years = c(fyear,fyear),
                                     model = tolower(FINAL_FUSER))

  final_frcst <- fused_frcst$preds %>%
    dplyr::right_join(df_basin_product %>%
                        dplyr::select(all_of(c("HYBAS_ID", "YYYY", "Q"))) ,
                      by = "YYYY") %>%
    dplyr::select(HYBAS_ID,YYYY,Q, everything())

}) %>% bind_rows()



{
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file_path <- file.path(PATH_OUTPUT,paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_seasonal_forecast_consolidated_", FINAL_FUSER, "_",timestamp,".rds"))
  saveRDS(object =consolidated_frcsts ,file = file_path )
  message("Consolidated forecast saved into : ", file_path)
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

