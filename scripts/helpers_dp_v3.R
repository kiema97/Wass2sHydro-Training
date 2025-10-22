# ==== Libraries ==============================================================
# ------------------------------------------------------------------------------
# safeload(): Install and load required R packages (CRAN + GitHub)
#
# Purpose:
#   Ensures that a list of required packages is installed and loaded.
#   - Installs missing CRAN packages from the configured repo.
#   - Installs/updates a GitHub package (WASS2SHydroR) via 'remotes'.
#
# Arguments:
#   pkgs          Character vector of package names to ensure are available.
#   update_github Logical. If TRUE, force re-installation of WASS2SHydroR
#                 from GitHub even if it is already installed. Default: FALSE.
#
# Returns:
#   (Invisibly) the result of loading all requested packages.
#
# Notes:
#   - Uses 'remotes::install_github()' which is lighter than 'devtools'.
#   - Respects GITHUB_PAT if set (recommended for rate limits).
#   - Installs dependencies and parallelizes CRAN compilation when possible.
# ------------------------------------------------------------------------------

required_pkgs <- c(
  "WASS2SHydroR","lubridate","ncdf4","stars",
  "sf","tidyr","dplyr","purrr","ggplot2",
  "ggspatial","readr","stringr","tibble"
)

safeload <- function(pkgs, update_github = FALSE) {
  # Helper: check if a namespace is available without attaching it
  is_installed <- function(x) {
    requireNamespace(x, quietly = TRUE)
  }

  # Report missing packages (before any install attempts)
  missing_pkgs <- pkgs[!vapply(pkgs, is_installed, logical(1))]
  if (length(missing_pkgs)) {
    message("Missing packages detected: ", paste(missing_pkgs, collapse = ", "))
  }

  # Ensure a stable CRAN mirror (optional but recommended for reproducibility)
  if (is.null(getOption("repos")) || isTRUE(getOption("repos")["CRAN"] == "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  # Ensure 'remotes' is available for GitHub installations
  if (!is_installed("remotes")) {
    install.packages("remotes", dependencies = TRUE)
  }

  # Install or update the GitHub package if requested or missing
  if (update_github || "WASS2SHydroR" %in% missing_pkgs) {

    tryCatch({
      detach("package:WASS2SHydroR", unload = TRUE)
    }, error = function(e){

    })

    remotes::install_github(
      "kiema97/AGRHYMET-WASS2SHydroR",
      build_vignettes = FALSE,
      upgrade = "never",
      auth_token = NULL,
      dependencies = TRUE,
      quiet = TRUE
    )
  }

  # Remove WASS2SHydroR from the missing list (if it was there)
  missing_pkgs <- setdiff(missing_pkgs, "WASS2SHydroR")

  # Install remaining CRAN packages if needed
  if (length(missing_pkgs) > 0) {
    install.packages(
      missing_pkgs,
      dependencies = TRUE,
      Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L)
    )
  }

  # Silently attach all requested packages
  invisible(lapply(
    pkgs,
    function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))
  ))
}

# Execute once to ensure your working session has everything loaded
safeload(required_pkgs,update_github = update_github)


# suppressPackageStartupMessages({
#   library(dplyr)
#   library(readr)
#   library(stringr)
#   library(purrr)
#   library(tidyr)
#   library(sf)
#   library(lubridate)
#   library(leaflet)
#   library(ggplot2)
#   library(ncdf4)
# })


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
      init_year = ifelse(str_detect(fname, "_f\\d{4}"), as.integer(str_replace(fname, ".*_f(\\d{4}).*", "\\1")), NA_integer_),
      var = ifelse(var_detect %in% c("PRCP","SST"), var_detect, var)
    ) %>%
    select(model, var, init_year, file)
}

catalog_predictors <- function(
    base_dir = PATH_PREDICTORS,
    vars_keep = c("PRCP", "SST"),
    file_regex = "\\.nc(\\.(gz|zip))?$"  # accepte .nc, .nc.gz, .nc.zip
) {
  # --- Dépendances (pas de library() pour rester robuste en package)
  stopifnot(requireNamespace("tibble",  quietly = TRUE))
  stopifnot(requireNamespace("dplyr",   quietly = TRUE))
  stopifnot(requireNamespace("purrr",   quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("tidyr",   quietly = TRUE))

  # --- Vérifs de base
  if (is.null(base_dir) || !dir.exists(base_dir)) {
    stop("`base_dir` introuvable : ", base_dir)
  }

  # --- Lister les sous-dossiers (1er niveau)
  dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0L) {
    return(tibble::tibble(model = character(), var = character(),
                          init_year = integer(), file = character()))
  }

  tibble::tibble(dir = dirs) %>%
    dplyr::mutate(var_dir = basename(dir)) %>%                                  # nom du répertoire = var annoncée
    dplyr::filter(var_dir %in% vars_keep) %>%                                   # ne garder que PRCP/SST (paramétrable)
    dplyr::mutate(files = purrr::map(dir, ~ list.files(.x, pattern = file_regex, full.names = TRUE))) %>%
    tidyr::unnest(files, keep_empty = FALSE) %>%                                 # drop lignes vides
    dplyr::mutate(
      file  = files,
      fname = basename(file),

      # Modèle = tout avant le premier point (ex: "CanSIPSIC4" de "CanSIPSIC4.PRCP_f2025.nc")
      model = stringr::str_replace(fname, "^([^.]+)\\..*$", "\\1"),

      # Variable détectée depuis le nom (entre 1er point et suivant/underscore) → upper
      var_detect = stringr::str_to_upper(
        stringr::str_replace(fname, "^[^.]+[._]([A-Za-z]+).*$", "\\1")
      ),

      # Année d'initialisation si motif "_fYYYY" présent
      init_year = dplyr::if_else(
        stringr::str_detect(fname, "_f\\d{4}"),
        readr::parse_integer(stringr::str_extract(fname, "(?<=_f)\\d{4}")),
        NA_integer_
      ),

      # Choix final de la var : détection valide sinon le nom du dossier
      var = dplyr::if_else(var_detect %in% vars_keep, var_detect, var_dir)
    ) %>%
    dplyr::select(model, var, init_year, file) %>%
    dplyr::distinct() %>%
    dplyr::arrange(var, model, init_year, file)
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

    if(!is.null(hist_df)){
      hid_hist_df <- hist_df %>%
        dplyr::filter(HYBAS_ID == hid) %>%
        dplyr::select(HYBAS_ID,DATE,Q)
    }

    fvec <- sort(list.files(dirv, pat, full.names = TRUE))
    if (!length(fvec)) {
      msg <- sprintf("[HYBAS %s][%s.%s] no files in %s", hid, model, var, dirv)
      if (on_empty == "stop") stop(msg) else if (on_empty == "warn") warning(msg)
      return(NULL)
    }
    bb <- if(PREDICTOR_VARS == "PRCP") .bbox_for(hid) else NULL
    df <- purrr::map(fvec, ~{
        predictors_data <- tryCatch({
          WASS2SHydroR::wass2s_prepare_data(
            x = .x, bbox = bb, spatial_reduce = "none",
            cell_layout = "wide", cell_prefix = tolower(var), verbose = FALSE
          ) |>
            dplyr::mutate(
              DATE     = lubridate::year(as.Date(.data$DATE)),  # année (entier)
              HYBAS_ID = hid
            ) |>
            dplyr::select(HYBAS_ID, DATE, dplyr::everything())
        }, error = function(e) {
          message(paste0("Product ", basename(.x), " not avaible for bassin : ", hid, "\n", e))
          tibble::tibble(HYBAS_ID = integer(), DATE = integer())
        })

        if(is.null(hist_df)){
          hid_hist_df <- data.frame(HYBAS_ID =hid,
                                    DATE =predictors_data$DATE,
                                    Q = NA_integer_)
        }

        hid_hist_df %>% inner_join(predictors_data %>% mutate(HYBAS_ID = as.factor(HYBAS_ID)), by = c("DATE","HYBAS_ID")) %>%
        rename(YYYY = DATE) %>%
        dplyr::select(HYBAS_ID,YYYY,Q, everything())

    }) |>
      dplyr::bind_rows()
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


#' Read and clean historical dataframe (HYBAS_ID, YYYY, ...)
#'
#' @param path Character. Path to the historical file.
#' @param sep Character. Field separator (e.g., "," or "\t").
#' @param id_col Character. Column name in the file that contains basin IDs (will be renamed to "HYBAS_ID").
#' @param hybas_ids Character or numeric vector. HYBAS_IDs to keep. If NULL, keep all.
#' @param missing_value_code Character/numeric or NULL. Extra missing code in file (e.g., -999). If NULL, not added.
#' @param extra_na Character vector of additional NA strings. Default: c("NA", "", "NaN")
#' @param check_warn Logical. If TRUE, warns about HYBAS_IDs not found.
#'
#' @return A tibble with columns HYBAS_ID, YYYY, and other variables, sorted by HYBAS_ID then YYYY.
#' @examples
#' # hist_df <- read_historical_df_yearly("data/hist.csv", sep = ",",
#' #                                      id_col = "BASIN_ID",
#' #                                      hybas_ids = c("1090100154","1090100155"),
#' #                                      missing_value_code = -999)
read_historical_df_yearly <- function(path,
                                      sep,
                                      id_col,
                                      hybas_ids = NULL,
                                      missing_value_code = NULL,
                                      extra_na = c("NA", "", "NaN"),
                                      check_warn = TRUE) {
  stopifnot(file.exists(path))
  stopifnot(is.character(sep), length(sep) == 1L)
  stopifnot(is.character(id_col), length(id_col) == 1L)

  # Build NA set
  na_strings <- unique(c(
    extra_na,
    if (!is.null(missing_value_code)) as.character(missing_value_code) else NULL
  ))

  # Read file
  df_raw <- utils::read.table(
    file = path,
    header = TRUE,
    sep = sep,
    na.strings = na_strings,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (!(id_col %in% names(df_raw))) {
    stop(sprintf("Column '%s' not found in file: %s", id_col, path))
  }
  if (!("DATE" %in% names(df_raw))) {
    stop("Column 'DATE' not found in file. It must contain the years (YYYY).")
  }

  # Rename ID col and convert DATE -> YYYY
  library(dplyr)
  df <- df_raw %>%
    rename(HYBAS_ID = dplyr::all_of(id_col)) %>%
    mutate(
      HYBAS_ID = as.factor(HYBAS_ID),
      DATE = as.integer(DATE)
    )

  # Filter and arrange
  if (!is.null(hybas_ids)) {
    df <- dplyr::filter(df, .data$HYBAS_ID %in% hybas_ids)
    if (check_warn) {
      missing_ids <- setdiff(hybas_ids, unique(df$HYBAS_ID))
      if (length(missing_ids)) {
        warning("HYBAS_IDs not found in file: ", paste(missing_ids, collapse = ", "))
      }
    }
  }

  df <- dplyr::arrange(df, .data$HYBAS_ID, .data$DATE)

  df
}






#' Read yearly historical data and optionally add a Forecast YEAR row per basin
#'
#' @param path Character. Path to the historical file.
#' @param sep Character. Field separator (e.g., "," or "\t").
#' @param id_col Character. Name of the basin ID column in the file (will be renamed to "HYBAS_ID").
#' @param hybas_ids Vector or NULL. Subset of HYBAS_IDs to keep. If NULL, keep all.
#' @param missing_value_code Character/numeric or NULL. Extra missing code (e.g., -999).
#' @param extra_na Character vector. Additional NA strings. Default c("NA","", "NaN").
#' @param check_warn Logical. Warn about missing HYBAS_IDs. Default TRUE.
#' @param fyear Integer or NULL. If set (e.g., 2025), ensure one row per basin for YYYY = fyear
#'             with Q = NA and other columns copied from the latest year of that basin.
#'
#' @return Tibble with columns HYBAS_ID, YYYY (integer), Q (may include NA), and any other columns.
read_historical_df_yearly <- function(path,
                                      sep,
                                      id_col,
                                      hybas_ids = NULL,
                                      missing_value_code = NULL,
                                      extra_na = c("NA", "", "NaN"),
                                      check_warn = TRUE,
                                      fyear = NULL) {
  stopifnot(file.exists(path))
  stopifnot(is.character(sep), length(sep) == 1L)
  stopifnot(is.character(id_col), length(id_col) == 1L)

  na_strings <- unique(c(extra_na,
                         if (!is.null(missing_value_code)) as.character(missing_value_code) else NULL))

  df_raw <- utils::read.table(
    file = path, header = TRUE, sep = sep,
    na.strings = na_strings, check.names = FALSE, stringsAsFactors = FALSE
  )

  if (!(id_col %in% names(df_raw))) {
    stop(sprintf("Column '%s' not found in file: %s", id_col, path))
  }
  if (!("DATE" %in% names(df_raw))) {
    stop("Column 'DATE' not found in file. It must contain years (YYYY).")
  }

  # Standardize columns
  library(dplyr)
  df <- df_raw %>%
    rename(HYBAS_ID = dplyr::all_of(id_col)) %>%
    rename(YYYY = DATE) %>%
    mutate(HYBAS_ID = as.factor(HYBAS_ID),
           YYYY = as.integer(YYYY))

  # Keep only requested basins
  if (!is.null(hybas_ids)) {
    df <- dplyr::filter(df, .data$HYBAS_ID %in% hybas_ids)
    if (check_warn) {
      missing_ids <- setdiff(hybas_ids, unique(df$HYBAS_ID))
      if (length(missing_ids)) {
        warning("HYBAS_IDs not found in file: ", paste(missing_ids, collapse = ", "))
      }
    }
  }

  # Optionally add forecast year row per basin
  if (!is.null(fyear)) {
    fyear <- as.integer(fyear)
    # Ensure Q exists; if absent, create it (all NA) so we can set it below.
    if (!("Q" %in% names(df))) df$Q <- NA_real_

    df <- df %>%
      arrange(.data$HYBAS_ID, .data$YYYY) %>%
      group_by(.data$HYBAS_ID) %>%
      group_modify(~{
        g <- .x
        if (any(g$YYYY == fyear, na.rm = TRUE)) return(g)  # already present

        # Take the latest row of this basin as template for non-year columns
        tmpl <- dplyr::slice_tail(g, n = 1)

        # Set forecast year and Q = NA; keep other columns as in tmpl
        tmpl$YYYY <- fyear
        tmpl$Q <- NA_real_

        dplyr::bind_rows(g, tmpl)
      }) %>%
      ungroup() %>%
      arrange(.data$HYBAS_ID, .data$YYYY)
  } else {
    df <- df %>% arrange(.data$HYBAS_ID, .data$YYYY)
  }

  df
}
