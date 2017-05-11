rasterizeTRMMnc4 <- function(x, SDSstring = "11111111", ext = NULL, cores = 1L,
                             ...) {

  ## extract names of relevant variables based on specified 'SDSstring'
  ids <- strsplit(SDSstring, "")[[1]] == "1"
  nc4 <- ncdf4::nc_open(x[1])
  vrn <- names(nc4$var)[ids]; len <- length(vrn)

  ## parallelization
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterExport(cl, c("ids", "ext", "vrn", "len"),
                          envir = environment())

  ## rasterize and, optionally, crop images in parallel
  lst <- parallel::parLapply(cl, x, function(i) {
    rst <- if (length(vrn) == 1) {
      raster::raster(i, varname = vrn, crs = "+init=epsg:4326")
    } else {
      raster::stack(lapply(vrn, function(j) {
        raster::raster(i, varname = j, crs = "+init=epsg:4326")
      }))
    }; names(rst) <- vrn

    if (!is.null(ext))
      rst <- raster::crop(rst, ext, snap = "out")

    return(rst)
  })

  return(if (len == 1) raster::stack(lst) else lst)
}
