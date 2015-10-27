#' Extract compressed CHIRPS files
#'
#' @description
#' This wrapper function around \code{\link{gunzip}} extracts the contents of
#' a single or multiple CHIRPS *.gz files to a specified destination folder.
#'
#' @param x 'character'. Input filename(s).
#' @param exdir 'character'. The destination folder for file extraction.
#' @param cores 'integer'. The number of cores used for parallel processing via
#' \strong{doParallel}.
#' @param ... Further arguments passed on to \code{\link{gunzip}}, e.g. 'skip',
#' 'overwrite' and 'remove'.
#'
#' @return A 'character' vector of filenames.
#'
#' @author Florian Detsch
#'
#' @export extractChirps
#' @name extractChirps
extractChirps <- function(x, exdir, cores = 1L, ...) {

  ## if not supplied, 'exdir' defaults to unique location of 'x'
  if (missing(exdir))
    exdir <- unique(dirname(x))

  ## target filenames
  destname <- paste0(exdir, "/", substr(basename(x), 1, nchar(basename(x))-3))

  if (cores > 1L) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
  }

  suppressWarnings(
    jnk <- foreach(i = 1:length(x), .packages = "R.utils",
                   .export = ls(envir = globalenv())) %dopar%
      R.utils::gunzip(x[i], destname = destname[i], ...)
  )

  ## deregister parallel backend
  if (cores > 1L)
    parallel::stopCluster(cl)

  ## return names of extracted files
  return(destname)
}
