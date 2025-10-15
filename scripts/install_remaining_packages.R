# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install Matrix first
if (!require(Matrix)) {
  install.packages("Matrix")
}
# Packages to install from CRAN
cran_packages <- c(
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

# Install from CRAN
install.packages(cran_packages, dependencies = TRUE)

# Verify key spatial packages
if (!require(terra)) {
  message("Installing terra from RSPM...")
  install.packages("terra", repos = "https://rspatial.r-universe.dev")
}

if (!require(sf)) {
  message("Installing sf...")
  install.packages("sf", configure.args = "--with-proj-lib=/usr/local/lib/")
}

# Test critical packages
message("Testing package loading...")
test_packages <- c("dplyr", "sf", "terra", "tidymodels")
for (pkg in test_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    message("✓ ", pkg, " installed successfully")
  } else {
    message("✗ ", pkg, " failed to install")
  }
}
