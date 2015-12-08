bfastTemplate <- function(local = FALSE, dsn, ...) {
  ## chirps raw data
  if (local & !missing(dsn)) {
    chirps_files <- list.files(dsn, full.names = TRUE, pattern = ".tif$")
    chirps_raster <- raster::stack(chirps_files[1])
  } else {
    chirps_files <- chirps::updateInventory()
    chirps_files <- chirps::downloadchirps(chirps_files[1], ...)
    chirps_raster <- chirps::rasterizechirps(chirps_files, remove_header = TRUE)
    file.remove(chirps_files)
  }

  #   ## reference extent
  #   library(rworldmap)
  #   data("countriesCoarse")
  #   spy_iran <- subset(countriesCoarse, ADMIN == "Iran")
  #
  #   ## crop global chirps images
  #   rst_template <- raster::crop(chirps_raster, spy_iran)
  #   rst_template <- raster::setValues(rst_template, NA)

  rst_template <- chirps_raster
  rst_template <- raster::setValues(rst_template, NA)
  return(rst_template)
}
