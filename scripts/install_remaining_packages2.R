# Temporarily disable renv
Sys.unsetenv("RENV_PROJECT")
Sys.unsetenv("RENV_PATHS_ROOT")
.libPaths('C:/Users/ARSENE/.local/share/mamba/envs/wass2s-hydro-3/Lib/R/library')
# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Function to check if package is installed
is_package_installed <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# Install Matrix first if needed
if (!is_package_installed("Matrix")) {
  message("Installing Matrix...")
  install.packages("Matrix", quiet = FALSE)
} else {
  message("✓ Matrix is already installed")
}

# Packages to install
packages <- c(
  "terra",
  "sf",
  "stars",
  "units",
  "tune",
  "rules",
  "tidymodels",
  "ecmwfr",
  "furrr",
  "future",
  "ggspatial",
  "viridis",
  "knitr",
  "rmarkdown",
  "stringi",
  "ranger",
  "xgboost",
  "testthat",
  "tidyselect",
  "hydrostats",
  "zoo",
  "xts"
)

# Filter out already installed packages
packages_to_install <- packages[!sapply(packages, is_package_installed)]

if (length(packages_to_install) > 0) {
  message("Packages to install: ", paste(packages_to_install, collapse = ", "))

  # Install packages in batches to avoid timeouts
  install_batch <- function(pkgs, batch_size = 5) {
    for(i in seq(1, length(pkgs), batch_size)) {
      batch <- pkgs[i:min(i + batch_size - 1, length(pkgs))]
      message("Installing batch: ", paste(batch, collapse = ", "))
      tryCatch({
        install.packages(batch, dependencies = TRUE, quiet = FALSE)
      }, error = function(e) {
        message("Error with batch: ", e$message)
      })
      # Small delay between batches
      Sys.sleep(2)
    }
  }

  # Install only needed packages
  install_batch(packages_to_install)
} else {
  message("✓ All packages are already installed")
}

# Test critical packages
message("Testing critical packages...")
critical_pkgs <- c("terra", "sf", "dplyr", "tidymodels")
for(pkg in critical_pkgs) {
  if(is_package_installed(pkg)) {
    message("✓ ", pkg, " installed successfully")
  } else {
    message("✗ ", pkg, " failed to install")
  }
}

# Install GitHub package only if not already installed
if (!is_package_installed("AGRHYMETWASS2SHydroR")) {
  message("Installing AGRHYMET-WASS2SHydroR from GitHub...")
  tryCatch({
    devtools::install_github(
      "kiema97/AGRHYMET-WASS2SHydroR",
      build_vignettes = FALSE,
      upgrade = "never",
      auth_token = NULL
    )
    message("✓ AGRHYMET-WASS2SHydroR installed successfully")
  }, error = function(e) {
    message("✗ Failed to install AGRHYMET-WASS2SHydroR: ", e$message)
  })
} else {
  message("✓ AGRHYMET-WASS2SHydroR is already installed")
}

# Final verification
message("\n=== Installation Summary ===")
all_packages <- c(packages, "Matrix", "AGRHYMETWASS2SHydroR")
for(pkg in all_packages) {
  if(is_package_installed(pkg)) {
    message("✓ ", pkg)
  } else {
    message("✗ ", pkg)
  }
}
