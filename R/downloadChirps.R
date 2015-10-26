downloadChirps <- function(region = c("global", "whem", "africa", "camer"),
                           format = c("tifs", "netcdf"),
                           tres = c("daily"),
                           sres = c(0.05, 0.25),
                           begin = NULL, end = NULL, ...) {

  ## server address
  ch_url <- "ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0"

  ## dataset
  ch_ext <- paste(region, tres, sep = "_")

  ## format
  ch_ext <- paste(ch_ext, format, sep = "/")

  ## spatial resolution
  sres <- formatC(sres * 100, width = 2, flag = 0)
  sres <- paste0("p", sres, "/")
  ch_ext <- paste(ch_ext, sres, sep = "/")

  ## available years
  ch_url <- paste(ch_url, ch_ext, sep = "/")

  ch_yrs <- RCurl::getURL(ch_url, dirlistonly = TRUE)
  ch_yrs <- unlist(strsplit(ch_yrs, "\n"))


  ch_url <- paste(ch_url)
}
