#' Download Rainfall Data Set
#'
#' @description
#' Download a particular rainfall data set. Currently available products
#' originate from Climate Hazards Group InfraRed Precipitation with Station Data
#' (\href{http://chg.geog.ucsb.edu/data/chirps/}{CHIRPS}) and the Tropical
#' Rainfall Measuring Mission (\href{https://trmm.gsfc.nasa.gov/}{TRMM}).
#'
#' @param name \code{character}, currently one of \code{"chirps"} (default) or
#' \code{"trmm"}.
#' @param ... Additional arguments passed to underlying functions listed under
#' See Also.
#'
#' @return
#' A \code{character} vector of file paths.
#'
#' @seealso
#' \code{\link{getCHIRPS}}, \code{\link{getTRMM}}.
#'
#' @export getRain
#' @name getRain
getRain <- function(name = c("chirps", "trmm"), ...) {

  if (!(name[1] %in% c("chirps", "trmm")))
    stop("Specified product (currently) not available.\n")

  if (name[1] == "chirps")
    getCHIRPS(...)
  else
    getTRMM(...)
}
