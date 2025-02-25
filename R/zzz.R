

#' onload
#'
#' @param libname
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onLoad <- function(libname, pkgname) {

  shiny::addResourcePath(
    prefix = "logo",
    directoryPath = system.file(
      "logo",
      package = "calipR"
    )
  )

   # 📌 Déterminer si on est en cours d'installation
  is_install <- grepl("file\\d+", getwd())

  # 📂 Définir le chemin du package
  if (is_install) {
    package_path <- getwd()  # Pendant l'installation
    install_script <- file.path(package_path, "inst", "install.R")  # install.R encore dans inst/
    first_run_flag <- file.path(package_path, "inst", ".calipR_first_run")  # Flag dans inst/ pendant install
  } else {
    package_path <- system.file(package = pkgname)  # Après installation
    install_script <- file.path(package_path, "install.R")  # install.R est maintenant à la racine
    first_run_flag <- file.path(package_path, ".calipR_first_run")  # Flag à la racine après install
  }

  # 🛑 Vérifier si l'installation vient d'avoir lieu
  if (!file.exists(first_run_flag)) {
    # 📦 Marquer cette session comme première exécution après installation
    writeLines("first run", first_run_flag)

    # 🚀 Exécuter `install.R` si trouvé
    if (file.exists(install_script)) {
      message("🚀 Exécution de install.R après installation...")
      source(install_script)
    } else {
      warning("⚠️ install.R introuvable après installation.")
    }
  } else {
    message("✅ calipR a déjà été configuré, `install.R` ne sera pas ré-exécuté.")
  }

  # ✅ Afficher un message de confirmation
  message("📦 calipR chargé. Python devrait être configuré correctement.")
}
