#' Download CHIRPS data
#'
#' @description
#' Download Climate Hazards Group InfraRed Precipitation with Station (CHIRPS)
#' data for a given time span from Climate Hazards Group's (CHG) FTP server
#' (\url{ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/}).
#'
#' @param region 'character'. Region of interest; one of 'global', 'whem'
#' (western hemisphere), 'africa' or 'camer-carib' (Central America and the
#' Caribbean). See the official CHIRPS README (available online at
#' \url{ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/README-CHIRPS.txt})
#' for further information.
#' @param format 'character'. Desired file format; one of 'tifs' (default),
#' 'bils' or 'pngs'.
#' @param tres 'character'. Desired temporal resolution; one of '6-hourly'
#' (default), 'daily', 'pentad', 'dekad' or 'monthly'.
#' @param sres 'numeric'. Desired spatial resolution; one of
#' \code{c(0.05, 0.25)}.
#' @param begin,end 'date'. If not supplied, data download starts (stops) with
#' the first (last) record available.
#' @param dsn 'character'. Destination folder for file download, defaults to the
#' current working directory.
#' @param overwrite Logical. If \code{TRUE}, already downloaded files in 'dsn'
#' will be overwritten.
#' @param cores 'integer'. The number of cores used for parallel downloads.
#' @param ... Further arguments passed to \code{\link{download.file}}.
#'
#' @return
#' A \code{character} vector of file paths.
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
#'
#' @export getCHIRPS
#' @name getCHIRPS
getCHIRPS <- function(region = c("global", "whem", "africa", "camer-carib"),
                      format = c("tifs", "bils", "pngs"),
                      tres = c("6-hourly", "daily", "pentad", "dekad", "monthly"),
                      sres = c(0.05, 0.25),
                      begin = NULL, end = NULL,
                      dsn = getwd(), overwrite = FALSE,
                      cores = 1L, ...) {

  region = region[1]; format = format[1]; tres = tres[1]; sres = sres[1]

  ## time frame
  if (is.null(begin)) begin <- as.Date("1981-01-01")
  if (is.null(end)) end <- Sys.Date()

  ## destination folder
  if (!dir.exists(dsn)) dir.create(dsn, recursive = TRUE)

  ## dataset
  ch_ext <- paste(region, tres, sep = "_")

  ## format (not required for 6-hourly files)
  ch_ext <- if (tres == "6-hourly") {
    paste0(ch_ext, "/p1_bin")
  } else {
    paste0(ch_ext, "/", format)
  }

  ## spatial resolution (only required for daily files)
  if (tres == "daily") {
    sres <- formatC(sres * 100, width = 2, flag = 0)
    sres <- paste0("p", sres)
    ch_ext <- paste0(ch_ext, "/", sres)
  }

  ## retrieve file list
  ch_url <- paste0(serverPath(), "/", ch_ext, "/")

  onl <- if (tres %in% c("pentad", "dekad", "monthly")) {
    lsCat1(ch_url)
  } else if (tres == "6-hourly") {
    lsCat2(ch_url, begin, end)
  } else if (tres == "daily") {
    lsCat3(ch_url, begin, end)
  } else {
    stop("Specified temporal resolution (currently) not available.\n")
  }

  ## download files
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl))

  dots <- list(...)
  parallel::clusterExport(cl, c("dsn", "overwrite", "dots"),
                          envir = environment())

  do.call("c", parallel::parLapply(cl, onl, function(j) {

    # download current file
    destfile <- file.path(dsn, basename(j))
    jnk <- if (!file.exists(destfile) | overwrite) {
      dots_sub <- list(url = j, destfile = destfile)
      dots_sub <- append(dots_sub, dots)

      try(do.call(utils::download.file, args = dots_sub), silent = TRUE)
    }

    # if file download failed, return nothing, else return local file
    if (inherits(jnk, "try-error")) {
      return(invisible())
    } else {
      return(destfile)
    }
  }))
}
