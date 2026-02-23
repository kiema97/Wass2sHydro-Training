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
options(repos = c(CRAN = "https://cloud.r-project.org"))
required_pkgs <- c(
  "WASS2SHydroR","sf","tidyr","dplyr","purrr","ggplot2",
  "ggspatial","readr","stringr","tibble","xgboost","glmnet",
  "kknn","earth","Cubist","nnet","ranger","furrr","future","progressr"
)

safeload <- function(pkgs, update_github = TRUE) {
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
      quiet = FALSE,ref = "staging"
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
safeload(required_pkgs)

#------------------- 1) Clip subbasins by country polygon-----------------------------------

# Read shapefiles
a_countries <- sf::st_read(PATH_COUNTRIES, quiet = TRUE) %>%
  sf::st_make_valid()
a_subs      <- sf::st_read(PATH_SUBBASINS, quiet = TRUE) %>%
  sf::st_make_valid()

a_rivers      <- sf::st_read(PATH_RIVERS, quiet = TRUE) %>%
  sf::st_make_valid()

# Ensure same CRS
if (sf::st_crs(a_countries) != sf::st_crs(a_subs)) {
  a_subs <- sf::st_transform(a_subs, sf::st_crs(a_countries))
}

if (sf::st_crs(a_rivers) != sf::st_crs(a_subs)){
  a_rivers <- sf::st_transform(a_rivers, sf::st_crs(a_subs))
}

# Filter country
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

subs_union <- a_subs %>%
  st_make_valid() %>%
  st_union() %>%
  st_as_sf()

sf_rivers <- sf::st_intersection(a_rivers, subs_union)

merge_prcp_sst_lists <- function(
    prcp,
    sst,
    id_cols = c("HYBAS_ID", "YYYY","Q"),
    prcp_prefix = "prcp_",
    sst_prefix  = "sst_"
) {
  # ---- validate basic structure ----
  if (!is.list(prcp) || !is.list(sst)) {
    stop("`prcp` and `sst` must be lists.", call. = FALSE)
  }

  basins <- union(names(prcp), names(sst))

  out <- map(set_names(basins), function(b) {

    prcp_b <- prcp[[b]]
    sst_b  <- sst[[b]]

    # if one basin is missing entirely in one list, just return the other
    if (is.null(prcp_b) && is.null(sst_b)) return(NULL)
    if (is.null(prcp_b)) return(sst_b)
    if (is.null(sst_b))  return(prcp_b)

    models <- union(names(prcp_b), names(sst_b))

    map(set_names(models), function(m) {
      p <- prcp_b[[m]]
      s <- sst_b[[m]]

      if (is.null(p) && is.null(s)) return(NULL)
      if (is.null(p)) return(s)
      if (is.null(s)) return(p)

      # Keep only expected blocks to avoid duplicating id cols
      p_x <- p %>% select(any_of(id_cols), starts_with(prcp_prefix))
      s_x <- s %>% select(any_of(id_cols), starts_with(sst_prefix))

      # Join (keeps the PRCP rows; if you prefer strict intersection, use inner_join)
      merged <- p_x %>%
        left_join(s_x, by = id_cols)

      # Optional: ensure id_cols exist even if factors/names vary
      merged
    })
  })

  # Drop NULL basins (if any) and keep names clean
  compact(out)
}

safe_readRDS <- function(path) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  readRDS(path)
}

prcp_data_by_products <- safe_readRDS(PRCP_PATH_INPUTS)
sst_data_by_products  <- safe_readRDS(SST_PATH_INPUTS)

if (is.null(prcp_data_by_products) && is.null(sst_data_by_products)) {
  stop("You must provide at least one input: PRCP_PATH_INPUTS or SST_PATH_INPUTS.", call. = FALSE)
}

data_by_products <- if (is.null(prcp_data_by_products)){
  sst_data_by_products
}else if(is.null(sst_data_by_products)) {prcp_data_by_products
} else{
  merge_prcp_sst_lists(prcp_data_by_products, sst_data_by_products)
  PREDICTOR_VARS <-"PRCP-SST"
}




# 1) Choose number of workers (safe defaults)
# ------------------------------------------------------------------------------
if (!isTRUE(RUN_IN_PARALLEL)) {
  workers <- 1L
} else if (!is.null(WORKERS)) {
  workers <- as.integer(WORKERS)
  if (is.na(workers) || workers < 1L) {
    stop("WORKERS must be a positive integer.", call. = FALSE)
  }
} else {
  workers <- min(length(data_by_products), max(future::availableCores() - 2L, 1L))
}


# ------------------------------------------------------------------------------
# 2) Parallel plan
# - If not parallel, use sequential to avoid any multisession overhead
# ------------------------------------------------------------------------------
if (workers <= 1L) {
  future::plan(future::sequential)
} else {
  future::plan(future::multisession, workers = workers)
}

message("Workers used: ", workers)
# ------------------------------------------------------------------------------
# 4) Progress handlers
# ------------------------------------------------------------------------------
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")  # or "cli" if you prefer

# ==============================================================================
# 1bis) WORKSPACE SETUP (auto-create operational folders)
# ==============================================================================

WASS2S_ROOT_NAME <- "WASS2S_Operational_Runs"

# If NULL -> create in current working directory
#WASS2S_ROOT_PARENT <- NULL  # e.g. "D:/CCR_AOS/Operational" or NULL

PREDICTOR_SET <- PREDICTOR_VARS
RUN_ID <- sprintf("issue_%s", fyear)

init_wass2s_workspace <- function(
    root_parent = NULL,
    root_name = "WASS2S_Operational_Runs",
    approach,
    predictor_set,
    run_id,
    subdirs = c("figures",
                "tables",
                "logs",
                "exports","metrics",
                "models", "consolidations"),
    create = TRUE
) {
  # ---- sanitize strings for folder names ----
  approach <- tolower(approach)
  predictor_set <- gsub("[^A-Za-z0-9_]+", "_", toupper(predictor_set))
  run_id <- gsub("[^A-Za-z0-9_]+", "_", run_id)

  # ---- root location ----
  root <- if (is.null(root_parent)) {
    file.path(getwd(), root_name)
  } else {
    file.path(root_parent, root_name)
  }

  # ---- main run directory ----
  run_dir <- file.path(root, approach, predictor_set,  run_id)

  # ---- create directories ----
  if (isTRUE(create)) {
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    for (d in subdirs) dir.create(file.path(run_dir, d), recursive = TRUE, showWarnings = FALSE)
  }

  # return paths
  out <- list(
    root = root,
    run_dir = run_dir
  )
  for (d in subdirs) out[[d]] <- file.path(run_dir, d)

  out
}

ws <- init_wass2s_workspace(
  root_parent = WASS2S_ROOT_PARENT,
  root_name = WASS2S_ROOT_NAME,
  approach = APPROACH,
  predictor_set = PREDICTOR_SET,
  run_id = RUN_ID
)

message("WASS2S workspace: ", ws$run_dir)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
PATH_OUTPUT_ <- if(is.null(WASS2S_ROOT_PARENT)) "WASS2S_Operational_Runs" else WASS2S_ROOT_PARENT
PATH_OUTPUT <- file.path(PATH_OUTPUT_,tolower(APPROACH),PREDICTOR_VARS,RUN_ID)


get_basin_payload <- function(basin_obj, basin_id) {
  # Try the common nested structure first
  x <- basin_obj[[basin_id]]
  if (!is.null(x)) return(x)

  # If already "flat" (basin_obj itself contains leaderboards)
  basin_obj
}

meta <- tibble(
  approach = APPROACH,
  predictors = PREDICTOR_VARS,
  final_fuser = FINAL_FUSER
)
extract_leaderboards_long <- function(rds_obj, meta) {

  # The outer object may already be a list of basins,
  # or a list with a basins list inside. We assume named list by HYBAS_ID.
  basin_ids <- names(rds_obj)
  if (is.null(basin_ids) || length(basin_ids) == 0) {
    warning("RDS object has no names(); cannot infer basin IDs for file: ", meta$file)
    return(tibble())
  }

  imap_dfr(rds_obj, function(basin_obj, basin_id) {

    payload <- get_basin_payload(basin_obj, basin_id)

    lbs <- payload$leaderboards
    if (is.null(lbs) || length(lbs) == 0) return(tibble())

    imap_dfr(lbs, function(lb_df, model_name) {
      if (is.null(lb_df) || !is.data.frame(lb_df) || nrow(lb_df) == 0) return(tibble())

      # Expected columns: product, kge, weight
      out <- as_tibble(lb_df)

      # Defensive: ensure columns exist / standardize names
      nms <- names(out)
      if (!("product" %in% nms)) out$product <- NA_character_
      if (!("kge" %in% nms))     out$kge     <- NA_real_
      if (!("weight" %in% nms))  out$weight  <- NA_real_

      out %>%
        transmute(
          HYBAS_ID = as.character(basin_id),
          model = as.character(model_name),
          product = as.character(product),
          kge = as.numeric(kge),
          weight = as.numeric(weight)
        ) %>%
        mutate(
          predictors = meta$predictors,
          approach = meta$approach,
          final_fuser = meta$final_fuser,
          .before = 1
        )
    })
  })
}
