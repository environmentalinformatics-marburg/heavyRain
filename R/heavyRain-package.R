#' Download and Pre-Process CHIRPS and TRMM Rainfall Data Sets in R
#'
#' This is a set of functions to download and pre-process CHIRPS and TRMM
#' rainfall data sets in R.
#'
#' @name heavyRain-package
#' @aliases heavyRainpackage
#' @docType package
#' @title Download and Pre-Process CHIRPS and TRMM Rainfall Data Sets in R
#' @author Florian Detsch
#'
#' @import parallel raster
#' @importFrom gdalUtils gdal_translate get_subdatasets
#' @importFrom gimms significantTau
#' @importFrom ncdf4 nc_open
#' @importFrom R.utils gunzip
#' @importFrom RCurl getURL
#'
#'
#' @keywords package
#'
NULL
#'
#' @docType data
#' @name baleCHIRPS.v2
#' @title Bale Mountains CHIRPS.v2
#' @description Bale Mountains CHIRPS.v2.
#' @details This dataset contains CHIRPS.v2 observations for the Bale Mountains
#' National Park, southern Ethiopia (Jan 1981 to Dec 2016).
#' @format \code{raster::RasterStack}
NULL
