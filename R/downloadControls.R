### List inventory of CHIRPS pentad, dekad, and monthly data -----

lsCat1 <- function(url) {
  onl <- getSplitURL(url)
  paste0(url, onl)
}


### List inventory of CHIRPS 6-hourly data -----

lsCat2 <- function(url, begin, end) {

  ## available months
  mts <- getSplitURL(url)
  mts <- mts[nchar(mts) == 6]

  ## subset with months of interest
  ids <- match(sapply(c(begin, end), function(i) format(i, "%Y%m")), mts)

  if (any(is.na(ids))) {
    if (is.na(ids[1])) ids[1] <- 1
    if (is.na(ids[2])) ids[2] <- length(mts)
  }

  mts <- mts[ids[1]:ids[2]]
  drs <- paste0(url, mts, "/")

  ## available time steps
  do.call("c", lapply(drs, function(i) {
    fls <- getSplitURL(i)
    paste0(i, fls)
  }))
}


### List inventory of CHIRPS daily data -----

lsCat3 <- function(url, begin, end) {

  ## available years
  yrs <- getSplitURL(url)
  yrs <- yrs[nchar(yrs) == 4]

  ## subset with years of interest
  ids <- match(sapply(c(begin, end), function(i) format(i, "%Y")), yrs)

  if (any(is.na(ids))) {
    if (is.na(ids[1])) ids[1] <- 1
    if (is.na(ids[2])) ids[2] <- length(yrs)
  }

  yrs <- yrs[ids[1]:ids[2]]
  drs <- paste0(url, yrs, "/")

  ## available days
  do.call("c", lapply(drs, function(i) {
    fls <- getSplitURL(i)
    paste0(i, fls)
  }))
}


### Server paths for CHIRPS or TRMM data download -----

serverPath <- function(server = c("chirps", "trmm"), version = "2.0") {

  ## server not implemented
  if (!(server[1] %in% c("chirps", "trmm")))
    stop("Specified product not available or (currently) not supported.\n")

  ## chirps
  if (server[1] == "chirps") {
    if (!(version %in% c("2.0", "1.8")))
      stop("Specified version not available or (currently) not supported.\n")

    paste0("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-", version)

  ## trmm
  } else if (server[1] == "trmm") {
    "https://disc3.nascom.nasa.gov/data/TRMM_L3/"
  }
}


getSplitURL <- function(url) {
  onl <- RCurl::getURL(url, dirlistonly = TRUE)
  unlist(strsplit(onl, "\r\n"))
}
