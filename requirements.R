required_packages <- c("dplyr", "ggplot2", "tidyr", "renv", "maptiles", "sf", "osmdata", "tidyterra")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}
