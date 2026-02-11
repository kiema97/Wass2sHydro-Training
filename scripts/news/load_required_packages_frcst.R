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

# 1) Plan de parallélisme
plan(multisession, workers = workers)   # ajuste selon ta machine

# 2) Pattern calculé une seule fois
pred_pattern_by_product <- paste0("^", tolower(PREDICTOR_VARS), "_")

# 3) Activer la progression
handlers(global = TRUE)
handlers("txtprogressbar")  # ou "cli" si tu préfères


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
