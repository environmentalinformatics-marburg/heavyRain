bfastTemplate <- function(local = FALSE, dsn, ...) {
  ## gimms raw data
  if (local & !missing(dsn)) {
    gimms_files <- gimms::rearrangeFiles(dsn = "data", full.names = TRUE)
    gimms_raster <- raster::stack(gimms_files[1])
  } else {
    gimms_files <- gimms::updateInventory()
    gimms_files <- gimms::downloadGimms(gimms_files[1], ...)
    gimms_raster <- gimms::rasterizeGimms(gimms_files, remove_header = TRUE)
    file.remove(gimms_files)
  }

  ## reference extent
  library(rworldmap)
  data("countriesCoarse")
  spy_iran <- subset(countriesCoarse, ADMIN == "Iran")

  ## crop global gimms images
  rst_template <- raster::crop(gimms_raster, spy_iran)
  rst_template <- raster::setValues(rst_template, NA)
  return(rst_template)
}
