#!/usr/bin/env Rscript
# Script d'installation du noyau R pour Jupyter
# À exécuter dans RStudio

cat("=== Installation du noyau R pour Jupyter ===\n")

# Vérifier si les packages sont installés
required_packages <- c("devtools", "rzmq", "repr", "IRkernel", "IRdisplay")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installation de", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
  }
}

cat("Configuration du noyau...\n")
tryCatch({
  IRkernel::installspec()
  cat("Noyau R installé avec succès!\n")
}, error = function(e) {
  cat("Erreur lors de l'installation:", e$message, "\n")
  cat("Tentative alternative...\n")
  IRkernel::installspec(user = TRUE)
})

# Vérification finale
cat("Vérification...\n")
system("jupyter kernelspec list")
