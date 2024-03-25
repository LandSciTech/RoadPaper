#' Road Paper cloud benchmarking
#'
#' @description
#' Benchmarking the roads paper algorithms on the cloud
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

row_ind <- as.numeric(commandArgs(trailingOnly = TRUE)[[1]])
cat(print(row_ind))

# set PAT to avoid api limit errors on GitHub
Sys.setenv(GITHUB_PAT = "<pat>")

install.packages("remotes")
## Install Dependencies (listed in DESCRIPTION) ----
print("install deps")
install.packages("dplyr")
install.packages("sf")
install.packages("terra")
remotes::install_github("tpq/peakRAM")
remotes::install_github("LandSciTech/roads")
# out <- try(Require::Require(c("dplyr", "sf", "terra", "tpq/peakRAM", "LandSciTech/roads")))
# cat("installed from binary:\n")
# print(out)
#
# out2 <- try(Require::Require(c("dplyr", "sf", "terra", "tpq/peakRAM", "LandSciTech/roads"),
#                              type = "source", upgrade = "never"))
# cat("installed from source?:\n")
# print(out2)

source("6_benchmark_methods.R")
