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
  "WASS2SHydroR","sf","tidyr","dplyr","purrr","ggplot2",
  "ggspatial","readr","stringr","tibble","xgboost","glmnet",
  "kknn","earth","Cubist","nnet"
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
safeload(required_pkgs)


