# ==== Libraries ==============================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tidyr)
  library(sf)
  library(lubridate)
})

# WASS2SHydroR is expected to be installed; falls back gracefully if missing
has_wass2s <- requireNamespace("WASS2SHydroR", quietly = TRUE)
if (!has_wass2s) {
  message("NOTE: Package 'WASS2SHydroR' not found. You can still run most steps;\n",
          "the function 'wass2s_prepare_data()' will be called only if available.")
}

# Helper: safe parallel plan (base R's parallel via mclapply on Unix; fall back on lapply on Windows)
.parallel_map <- function(X, FUN, ...){
  if (.Platform$OS.type == "windows") {
    # Simple fallback for Windows notebooks to avoid cluster overhead for trainees
    lapply(X, FUN, ...)
  } else {
    parallel::mclapply(X, FUN, mc.cores = N_CORES, ...)
  }
}

# Walk predictor folders and parse filenames like: CanCM3.PRCP.nc, CCSM4.PRCP_f2025.nc, CCSM4.SST.nc
catalog_predictors <- function(base_dir = PATH_PREDICTORS){
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  tibble(dir = dirs) %>%
    mutate(var = basename(dir)) %>%
    filter(var %in% c("PRCP","SST")) %>%
    mutate(files = map(dir, ~list.files(.x, pattern = "\\.nc$", full.names = TRUE))) %>%
    tidyr::unnest(files) %>%
    mutate(
      file = files,
      fname = basename(file),
      # Patterns: Model.VAR.nc OR Model.VAR_fYYYY.nc
      model = str_replace(fname, "^([^.]+)\\..*$", "\\1"),
      var_detect = toupper(str_replace(fname, "^[^.]+\\.([A-Za-z]+).*$", "\\1")),
      init_year = ifelse(str_detect(fname, "_f(\\\
\\d{4})"), as.integer(str_replace(fname, ".*_f(\\\
\\d{4}).*", "\\1")), NA_integer_),
      var = ifelse(var_detect %in% c("PRCP","SST"), var_detect, var)
    ) %>%
    select(model, var, init_year, file)
}


extract_predictors_nested <- function(hybas_ids,
                                      models,
                                      hist_df = NULL,
                                      predictor = c("PRCP","SST"),
                                      predictors_root = "predictors",
                                      subbasins_sf = NULL,      # sf avec colonne HYBAS_ID (optionnel)
                                      bbox_list = NULL,         # nommée par HYBAS_ID: list("123"=c(xmin,ymin,xmax,ymax))
                                      init_year = NULL,         # ex. 2025 pour viser *_f2025.nc
                                      on_empty = c("warn","stop","quiet")) {
  on_empty <- match.arg(on_empty)
  
  # --- helpers concis ---------------------------------------------------------
  .bbox_for <- function(hid) {
    if (!is.null(bbox_list) && !is.null(bbox_list[[as.character(hid)]])) {
      bb <- bbox_list[[as.character(hid)]]
      stopifnot(is.numeric(bb), length(bb) == 4); return(as.numeric(bb))
    }
    if (!is.null(subbasins_sf)) {
      g <- subbasins_sf[subbasins_sf$HYBAS_ID == hid, , drop = FALSE]
      if (nrow(g)) {
        bb <- sf::st_bbox(sf::st_geometry(g))
        return(as.numeric(c( bb["ymax"],bb["xmin"], bb["ymin"], bb["xmax"])))
      }
    }
    NULL
  }
  
  .pattern <- function(model, var) {
    m <- stringr::str_replace_all(model, "\\.", "\\\\.")
    v <- stringr::str_replace_all(var,   "\\.", "\\\\.")
    paste0(sprintf("^%s\\.%s\\.nc$", m, v), "|", sprintf("^%s\\.%s.*_f%d\\.nc$", m, v, init_year))
  }
  
  .nc_dir <- function(var) {
    d <- file.path(predictors_root, toupper(var))
    if (dir.exists(d)) d else predictors_root
  }

  .extract_one <- function(hid, model, var) {
    dirv <- .nc_dir(var)
    pat  <- .pattern(model, toupper(var))

    fvec <- sort(list.files(dirv, pat, full.names = TRUE))
    if (!length(fvec)) {
      msg <- sprintf("[HYBAS %s][%s.%s] no files in %s", hid, model, var, dirv)
      if (on_empty == "stop") stop(msg) else if (on_empty == "warn") warning(msg)
      return(NULL)
    }
    bb <- .bbox_for(hid)
    df <- purrr::map(fvec, ~{
        WASS2SHydroR::wass2s_prepare_data(
        x = .x, bbox = bb, spatial_reduce = "none",
        cell_layout = "wide", cell_prefix = tolower(var),verbose = FALSE) %>% 
        mutate(lubridate::year(as.Date(DATE)))
      
    }) |>
      dplyr::bind_rows()
    if ("DATE" %in% names(df)) {
      df$DATE <- lubridate::year(as.Date(df$DATE))
      df <- df %>% 
        rename(YYYY = DATE) 
      df <- dplyr::distinct(dplyr::arrange(df, YYYY))
    }
    df
  }
  
  # --- cœur : liste imbriquée HYBAS_ID -> MODEL -> DF -------------------------
  out <- vector("list", length(hybas_ids)); names(out) <- as.character(hybas_ids)
  for (hid in hybas_ids) {
      message(paste0("Processing bassin : ", hid))
    per_model <- vector("list", length(models)); names(per_model) <- models
    for (m in models) {
      # concatène PRCP/SST par DATE pour ce modèle
      pieces <- lapply(predictor, function(v) .extract_one(hid, m, v))
      pieces <- pieces[!vapply(pieces, is.null, logical(1))]
      if (length(pieces)) {
        per_model[[m]] <- Reduce(function(x,y) dplyr::full_join(x,y, by="YYYY"), pieces) |>
          dplyr::arrange(YYYY)
      }
    }
    per_model <- per_model[!vapply(per_model, is.null, logical(1))]
    if (length(per_model)) out[[as.character(hid)]] <- per_model
  }
  # retire HYBAS_ID vides
  out[vapply(out, function(x) length(x) > 0, logical(1))]
}