#' Road Paper cloud compiling results
#'
#' @description
#' Compiling results after benchmarking and calculating metrics
#'
#' @author Sarah Endicott \email{sarah.endicott@canada.ca}
#'
#' @date 2024/03/12

# I copied these from the rocker/r-bspm Dockerfile because it sets them in the
# RProfile but it wasn't working
if(requireNamespace("bspm", quietly = TRUE)){
  message("setting up bspm for use on cloud")
  bspm::enable()
  options(pkgType="binary", install.packages.check.source = "no")
} else {
  r = getOption("repos")
  r["CRAN"] = "https://cran.rstudio.com/"
  options(repos = r)
}

real <- as.character(commandArgs(trailingOnly = TRUE)[[1]])
cat(print(real))

# set PAT to avoid api limit errors on GitHub
Sys.setenv(GITHUB_PAT = "<pat>")

install.packages("remotes")
## Install Dependencies (listed in DESCRIPTION) ----
print("install deps")
install.packages("dplyr")
install.packages("sf")
install.packages("terra")
install.packages("purrr")
install.packages("tidyr")
install.packages("tibble")
remotes::install_github("LandSciTech/roads")
remotes::install_github("LandSciTech/caribouMetrics")
remotes::install_github("LandSciTech/pfocal")
# out <- try(Require::Require(c("dplyr", "sf", "terra", "tpq/peakRAM", "LandSciTech/roads")))
# cat("installed from binary:\n")
# print(out)
#
# out2 <- try(Require::Require(c("dplyr", "sf", "terra", "tpq/peakRAM", "LandSciTech/roads"),
#                              type = "source", upgrade = "never"))
# cat("installed from source?:\n")
# print(out2)
flnm <- paste0("7_compile_results_cloud_", real, ".R")
source(flnm)
