# install.R — bootstrap pour formation WASS2SHydroR

# 1) Installer renv si nécessaire
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# 2) Restaurer les dépendances si un renv.lock est fourni
if (file.exists("renv.lock")) {
  message("Restoring environment from renv.lock...")
  renv::restore()
}

# 3) Installer devtools et IRkernel si besoin
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("IRkernel", quietly = TRUE)) install.packages("IRkernel")

# 4) Installer WASS2SHydroR depuis GitHub (pense à taguer une version stable)
if (!requireNamespace("WASS2SHydroR", quietly = TRUE)) {
  devtools::install_github("kiema97/AGRHYMET-WASS2SHydroR@v0.1.0")
}

# 5) Enregistrer un kernel Jupyter spécifique à la formation
IRkernel::installspec(
  name = "R-WASS2SHydroR",
  displayname = "R (WASS2SHydroR)",
  user = TRUE
)

message("✅ Environnement prêt. Lancez Jupyter et choisissez le kernel: R (WASS2SHydroR)")
