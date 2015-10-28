#' Download CHIRPS data
#'
#' @description
#' Download Climate Hazards Group InfraRed Precipitation with Station (CHIRPS)
#' data for a given time span from Climate Hazards Group's (CHG) FTP server
#' (\url{ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/}).
#'
#' @param region 'character. Region of interest; one of 'global', 'whem'
#' (western hemisphere), 'africa' or 'camer-carib' (Central America and the
#' Caribbean). See the official CHIRPS README (available online at
#' \url{ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/README-CHIRPS.txt})
#' for further information.
#' @param format 'character'. Desired file format; one of 'tifs', 'netcdf',
#' 'bils' or 'pngs'.
#' @param tres 'character'. Desired temporal resolution; one of 'daily',
#' 'pentad', 'dekad' or 'monthly'.
#' @param sres 'numeric'. Desired spatial resolution; one of
#' \code{c(0.05, 0.25)}.
#' @param begin 'date'. If not supplied, data download starts with the first
#' record available.
#' @param end 'date'. If not supplied, data download stops at the last record
#' available.
#' @param dsn 'character'. Destination folder for file download, defaults to the
#' current working directory.
#' @param overwrite Logical. If \code{TRUE}, already downloaded files in 'dsn'
#' will be overwritten.
#' @param cores 'integer'. The number of cores used for parallel download via
#' \strong{doParallel}.
#' @param ... Further arguments passed on to \code{\link{download.file}}.
#'
#' @return
#' A vector of filepaths.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{download.file}}
#'
#' @examples
#' \dontrun{
#' ## Download GIMMS NDVI3g binary data from 2000-2005 (this might take some time...)
#' gimms_files <- downloadGimms(x = 2000, y = 2005,
#'                              dsn = paste0(getwd(), "/data"))
#' gimms_files[1:10]
#' }
#' @export downloadChirps
#' @name downloadChirps
NULL

downloadChirps <- function(region = c("global", "whem", "africa", "camer-carib"),
                           format = c("tifs", "netcdf"),
                           tres = c("daily"),
                           sres = c(0.05, 0.25),
                           begin = NULL, end = NULL,
                           dsn = getwd(), overwrite = FALSE,
                           cores = 1L, ...) {

  ## server address
  ch_url <- "ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0"

  ## dataset
  ch_ext <- paste(region[1], tres[1], sep = "_")

  ## format
  ch_ext <- paste(ch_ext, format[1], sep = "/")

  ## spatial resolution
  sres <- formatC(sres[1] * 100, width = 2, flag = 0)
  sres <- paste0("p", sres, "/")
  ch_ext <- paste(ch_ext, sres, sep = "/")

  ## available years
  ch_url <- paste(ch_url, ch_ext, sep = "/")

  ch_yrs <- RCurl::getURL(ch_url, dirlistonly = TRUE)
  ch_yrs <- unlist(strsplit(ch_yrs, "\n"))

  ## available days
  if (is.null(begin)) begin <- as.Date(paste0(ch_yrs[1], "-01-01"))
  if (is.null(end)) end <- as.Date(paste0(ch_yrs[length(ch_yrs)], "-12-31"))

  ## download year by year
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
  }

  lst_fls <- lapply(ch_yrs, function(i) {

    cat("Year", i, "is in. Initializing data download...\n")
    ch_url_yr <- paste0(ch_url, i, "/")

    if (i == substr(Sys.Date(), 1, 4)) {

      ch_fls <- RCurl::getURL(ch_url_yr, dirlistonly = TRUE)
      ch_fls <- unlist(strsplit(ch_fls, "\n"))

      ch_dys <- substr(ch_fls, 13, 22)
      dt_dys <- as.Date(ch_dys, format = "%Y.%m.%d")

    } else {
      dt_dys <- seq(as.Date(paste0(i, "-01-01")), # start
                    as.Date(paste0(i, "-12-31")), # end
                    1)                            # by
    }

    suppressWarnings(
      ch_fls <- foreach(j = dt_dys, .export = ls(envir = globalenv()),
                        .combine = "c") %dopar% {

        ch_dy <- strftime(j, format = "%Y.%m.%d")
        ch_dy <- paste("chirps-v2.0", ch_dy, "tif.gz", sep = ".")
        ch_url_dy <- paste0(ch_url_yr, ch_dy)

        destfile <- paste(dsn, ch_dy, sep = "/")

        if (!file.exists(destfile) | overwrite)
          try(download.file(ch_url_dy, destfile, ...), silent = FALSE)

        return(destfile)
      }
    )

    return(ch_fls)
  })

  ## deregister parallel backend
  if (cores > 1)
    parallel::stopCluster(cl)

  ## return files
  ch_fls <- do.call("c", lst_fls)
  return(ch_fls)
}
