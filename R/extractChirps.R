#' Extract compressed CHIRPS files
#'
#' @description
#' This wrapper function around \code{\link{gunzip}} extracts the contents of
#' a single or multiple CHIRPS *.gz files to a specified destination folder.
#'
#' @param x 'character'. Input filename(s).
#' @param dsn 'character'. The destination folder for file extraction.
#' @param cores 'integer'. The number of cores used for parallel processing via
#' \strong{doParallel}.
#' @param ... Further arguments passed to \code{\link{gunzip}}, e.g. 'skip',
#' 'overwrite' and 'remove'.
#'
#' @return A 'character' vector of filenames.
#'
#' @author Florian Detsch
#'
#' @export extractChirps
#' @name extractChirps
extractChirps <- function(x, dsn, cores = 1L, ...) {

  ## if not supplied, 'dsn' defaults to unique location of 'x'
  if (missing(dsn))
    dsn <- unique(dirname(x))

  ## target filenames
  destname <- paste0(dsn, "/", substr(basename(x), 1, nchar(basename(x)) - 3))

  ## extract files
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl))

  dots <- list(...)
  parallel::clusterExport(cl, c("x", "destname", "dots"), envir = environment())

    jnk <- parallel::parLapply(cl, 1:length(x), function(i) {
      dots_sub <- list(filename = x[i], destname = destname[i])
      dots_sub <- append(dots_sub, dots)

      do.call(R.utils::gunzip, args = dots_sub)
    })

  ## return names of extracted files
  return(destname)
}
