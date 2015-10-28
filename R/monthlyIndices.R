#' Create timestamps or monthly indices from CHIRPS files
#'
#' @description
#' Extract time information or, based upon this, monthly indices from regularly
#' named CHIRPS files (compressed or decompressed).
#'
#' @param x 'character'. Input filename(s).
#' @param pos1,pos2 'integer'. The first and last element of the date string in
#' 'x', defaults to the CHIRPS naming convention; see \code{\link{substr}}.
#' @param timestamp 'logical'. If TRUE, an actual timestamp (formatted according
#' to \code{...}) is returned rather than a vector of indices.
#' @param ... Further arguments passed on to \code{\link{strftime}}.
#'
#' @return An 'integer' vector with unique monthly indices or, if
#' \code{timestamp = TRUE}, a character vector with formatted timestamps.
#'
#' @author Florian Detsch
#'
#' @export monthlyIndices
#' @name monthlyIndices
monthlyIndices <- function(x, pos1 = 13L, pos2 = 22L, timestamp = FALSE, ...) {

  ## extract timestamp
  ch_id <- substr(basename(x), pos1, pos2)

  ## return formatted date
  dt_time <- as.Date(ch_id, format = "%Y.%m.%d")
  ch_time <- strftime(dt_time, ...)

  if (!timestamp) {
    ## return numeric indices
    fc_time <- factor(ch_time, levels = unique(ch_time))
    num_id <- as.numeric(fc_time)

    return(num_id)

  } else {

    return(ch_time)
  }
}
