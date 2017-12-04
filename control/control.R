## working directory
# library(Orcs)
# setwdOS(path_ext = "programming/r/chirps")
# setwd("/media/fdetsch/dev/chirps")

## packages
lib <- c("chirps", "doParallel", "gimms", "Kendall", "RColorBrewer", "Rsenal")
jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

## parallelization
supcl <- makeCluster(3)
registerDoParallel(supcl)

# ## extract files from .gz
# chirps_files_gz <- list.files("data/gz", pattern = ".gz$", full.names = TRUE)
#
# chirps_files <- extractCHIRPS(chirps_files_gz, exdir = "data/tif", cores = 3L,
#                               remove = FALSE, overwrite = TRUE)

chirps_files <- list.files("data/tif", pattern = ".tif$", full.names = TRUE)

## reference extent
# data("countriesCoarse")
# spy_iran <- subset(countriesCoarse, ADMIN == "Iran")

spy_iran <- if (!file.exists("data/GADM_2.7_IRN_adm0.rds")) {
  getData(country = "IRN", level = 0, path = "data")
} else {
  readRDS("data/GADM_2.7_IRN_adm0.rds")
}
suppressWarnings(proj4string(spy_iran) <- "+init=epsg:4326")


################################################################################
## crop global data
################################################################################

overwrite <- TRUE
lst_chirps_crp <-
  foreach(i = chirps_files, .packages = c("raster", "rgdal")) %dopar% {
    # output filename
    filename <- paste0("data/crp/CRP_", basename(i))
    # if file does not exist or overwrite is TRUE, crop image
    if (!file.exists(filename) | overwrite) {
      rst <- raster::raster(i)
      raster::crop(rst, spy_iran, snap = "out", filename = filename,
                   format = "GTiff", overwrite = TRUE)
    # otherwise return existing cropped image
    } else {
      raster::raster(filename)
    }
  }

## reject invalid values
lst_chirps_scl <-
  foreach(i = lst_chirps_crp, .packages = c("raster", "rgdal")) %dopar% {
    # output filename
    filename <- paste0("data/scl/SCL_", names(i), ".tif")
    # if file does not exist or overwrite is TRUE, rescale image
    if (!file.exists(filename) | overwrite) {
      rst <- i
      rst[rst[] < 0] <- NA
      raster::writeRaster(rst, filename = filename,
                          format = "GTiff", overwrite = TRUE)
      # otherwise return existing cropped image
    } else {
      raster(filename)
    }
  }

## cells with gsod stations
gsod_shp <- GSODTools::gsodReformat(data = GSODTools::updateInventory(),
                                    elevation = TRUE,
                                    coords = TRUE,
                                    df2sp = TRUE)

gsod_shp <- gsod_shp[spy_iran, ]

cells_gsod <- cellFromXY(lst_chirps_scl[[1]], gsod_shp)

## extract cell values from all chirps layers
val_stat <-
  foreach(i = 1:length(lst_chirps_scl), .combine = "rbind", .packages = lib) %dopar% {
    matrix(lst_chirps_scl[[i]][cells_gsod],
           ncol = length(cells_gsod), byrow = TRUE)
  }

## convert to wide format and save
mat_stat <- t(val_stat)
names(mat_stat) <- heavyRain::monthlyIndices(chirps_files, timestamp = TRUE)
saveRDS(mat_stat, file = "/media/fdetsch/modis_data/chirps/data/chirps_scl_gsod.rds")


################################################################################
## resample to regular 8-km gimms grid
################################################################################

## gimms reference grid
fls_gimms <- gimms::updateInventory(sort = TRUE)[1]
fls_gimms <- downloadGimms(fls_gimms, dsn = "data")
rst_gimms <- rasterizeGimms(fls_gimms,
                            filename = fls_gimms, format = "GTiff",
                            overwrite = TRUE)
rst_gimms <- crop(rst_gimms, spy_iran, snap = "out")

lst_chirps_rsmpl <-
  foreach(i = lst_chirps_scl, .packages = c("raster", "rgdal")) %dopar% {
    # output filename
    file_out <- paste0("data/rsmpl/RSMPL_", names(i), ".tif")
    # if file does not exist or overwrite is TRUE, rescale image
    if (!file.exists(file_out) | overwrite) {
      resample(i, rst_gimms,
               filename = file_out, format = "GTiff", overwrite = TRUE)
      # otherwise return existing cropped image
    } else {
      raster(file_out)
    }
  }


################################################################################
## aggregate bi-monthly images
################################################################################

## monthly indices
int_months <- heavyRain::monthlyIndices(chirps_files, format = "%b %y")

lst_chirps_files <- split(chirps_files, f = int_months)
lst_chirps_split <- split(lst_chirps_rsmpl, f = int_months)

## per month, aggregate days 1-15 and remaining days (standard gimms procedure)
lst_chirps_bmn <- foreach(i = lst_chirps_split, j = lst_chirps_files,
        .packages = c("raster", "rgdal")) %dopar% {
  indices <- c(rep(1, 15), rep(2, length(i)-15))

  filename <- paste0(unique(substr(basename(j), 1, nchar(basename(j))-7)), ".tif")
  stackApply(stack(i), indices = indices, fun = sum, na.rm = FALSE,
             filename = paste0("data/bimonthly/", filename),
             bylayer = TRUE, suffix = c("15a", "15b"),
             format = "GTiff", overwrite = TRUE)
        }

## reimport files
fls_chirps_bmn <- list.files("data/bimonthly", pattern = ".tif$",
                             full.names = TRUE)
rst_chirps_bmn <- stack(fls_chirps_bmn)


################################################################################
## remove seasonal signal
################################################################################

## calculate long-term bi-monthly means
# lst_chirps_means <-
#   foreach(i = 1:24, j = rep(month.abb, each = 2),
#           k = rep(c("15a", "15b"), 12),
#           .packages = c("raster", "rgdal")) %dopar% {
#
#   # layers corresponding to current period (e.g. '82jan15a')
#   id <- seq(i, nlayers(rst_chirps_bmn), 24)
#   rst_chirps_tmp <- rst_chirps_bmn[[id]]
#
#   # calculate long-term mean of current period (e.g. for 1982-2013 'jan15a')
#   calc(rst_chirps_tmp, fun = mean, na.rm = TRUE,
#        filename = paste0("data/longterm_means/mean_", j, k),
#        format = "GTiff", overwrite = TRUE)
# }
#
# rst_chirps_means <- stack(lst_chirps_means)

# reimport
fls_chirps_means <- list.files("data/longterm_means", pattern = "mean.*.tif",
                               full.names = TRUE)

int_id <- do.call("c", lapply(month.abb, function(i) grep(i, fls_chirps_means)))
fls_chirps_means <- fls_chirps_means[int_id]
rst_chirps_means <- stack(fls_chirps_means)

## replicate bi-monthly 'gimms_raster_means' to match up with number of layers of
## initial 'gimms_raster_agg' (as `foreach` does not support recycling!)
lst_chirps_means <- replicate(nlayers(rst_chirps_bmn) / nlayers(rst_chirps_means),
                              rst_chirps_means)
rst_chirps_means <- stack(lst_chirps_means)
rst_chirps_means <- stack(rst_chirps_means, rst_chirps_means[[1:18]])

## subtract long-term mean from bi-monthly values
# lst_chirps_dsn <-
#   foreach(i = 1:nlayers(rst_chirps_bmn),
#           .packages = c("raster", "rgdal")) %dopar% {
#
#             overlay(rst_chirps_bmn[[i]], rst_chirps_means[[i]],
#                     fun = function(x, y) {x - y},
#                     filename = paste0("data/bimonthly_dsn/DSN_",
#                                       names(rst_chirps_bmn[[i]]), ".tif"),
#                     format = "GTiff", overwrite = TRUE)
#
#   }
#
# rst_chirps_dsn <- stack(lst_chirps_dsn)

# reimport
fls_chirps_dsn <- list.files("data/bimonthly_dsn", pattern = "DSN.*.tif",
                             full.names = TRUE)
rst_chirps_dsn <- stack(fls_chirps_dsn)


################################################################################
## mann-kendall trend test (p < 0.001)
################################################################################

fls_chirps_dsn <- list.files("data/bimonthly_dsn", pattern = "^DSN_.*.tif",
                             full.names = TRUE)

id_82 <- grep("1982.01_15a", fls_chirps_dsn)
id_13 <- grep("2013.12_15b", fls_chirps_dsn)
fls_chirps_dsn <- fls_chirps_dsn[id_82:id_13]
rst_chirps_dsn <- stack(fls_chirps_dsn)


## apply custom function on a pixel basis
blues <- colorRampPalette(brewer.pal(5, "Blues"))
cols <- colorRampPalette(brewer.pal(5, "RdBu"))

lst_chirps_trend <-
  foreach(p = c(1, 0.05, 0.01, 0.001)) %do% {

    # compute pixel-based kendall's tau
    rst_chirps_trend <- calc(rst_chirps_dsn,
                             fun = function(x) {
                               significantTau(x, p = p, prewhitening = TRUE, conf.intervals = FALSE)
                             }, filename = paste0("data/out/chirps_mk", gsub("0\\.", "", p), "_8213"),
                             format = "GTiff", overwrite = TRUE)

  # reimport
  file_in <- paste0("data/out/chirps_mk", gsub("0\\.", "", p), "_8213.tif")
  rst_chirps_trend <- raster(file_in)

  png(paste0("data/vis/overall/chirps_mk", gsub("0\\.", "", p), ".png"),
      width = 14, height = 14, units = "cm", res = 500)
  p <- spplot(mask(rst_chirps_trend, spy_iran), col.regions = blues(1000),
              scales = list(draw = TRUE), maxpixels = 100000,
              at = seq(0, 0.25, 0.01)) +
    latticeExtra::layer(sp.polygons(spy_iran, col = "black"))
  print(p)
  dev.off()

  return(rst_chirps_trend)
}


################################################################################
## mann-kendall trend test (p < 0.001) per month
################################################################################

## monthly composites
# indices <- rep(1:(length(fls_chirps_dsn)/2), each = 2)
# rst_chirps_dsn_mnth <- stackApply(rst_chirps_dsn, indices, fun = sum)
# saveRDS(rst_chirps_dsn_mnth, file = "/media/fdetsch/dev/repositories/chirps/inst/extdata/dsn_mnth.rds")
rst_chirps_dsn_mnth <- readRDS("/media/fdetsch/dev/repositories/chirps/inst/extdata/dsn_mnth.rds")

lst_mnth_trends <- foreach(p = c(0.05, 0.01, 0.001)) %do% {

  # status message
  cat("Processing significance level", p, "...\n")

#   lst_mnth_trends <-
#     foreach(i = 1:12, j = month.abb,
#             .packages = lib) %dopar% {
#
#               rst_mnth <- rst_chirps_dsn_mnth[[seq(i, nlayers(rst_chirps_dsn_mnth), 12)]]
#
#               calc(rst_mnth, fun = function(x) {
#                 gimms::significantTau(x, p = p, prewhitening = TRUE)
#               }, filename = paste0("data/out/monthly/chirps_mk", gsub("0\\.", "", p), "_", j, "_8213"),
#               format = "GTiff", overwrite = TRUE)
#             }
#   rst_mnth_trends <- stack(lst_mnth_trends)

  # reimport
  lst_mnth_trends <- lapply(month.abb, function(i) {
    file_in <- paste0(paste0("data/out/monthly/chirps_mk", gsub("0\\.", "", p), "_", i, "_8213.tif"))
    raster(file_in)
  })
  rst_mnth_trends <- stack(lst_mnth_trends[c(12, 1:11)])

  png(paste0("data/vis/monthly/chirps_mk", gsub("0\\.", "", p), "_monthly.png"),
      width = 20, height = 24, units = "cm", res = 500)
  p <- spplot(mask(rst_mnth_trends, spy_iran), col.regions = cols(1000),
              at = seq(-.75, .75, .01), layout = c(3, 4)) +
    latticeExtra::layer(sp.polygons(spy_iran, col = "black"))
  print(p)
  dev.off()

  return(rst_mnth_trends)
}

rst_mnth_trends <- stack(lst_mnth_trends)

# cols <- colorRampPalette(brewer.pal(5, "RdBu"))
# spplot(rst_mnth_trends, col.regions = cols(100), at = seq(-.5, .5, .01)) +
#   latticeExtra::layer(sp.polygons(spy_iran, col = "black"))



################################################################################
## mann-kendall trend test (p < 0.001) per season
################################################################################

# fls_chirps_dsn <- list.files("data/bimonthly_dsn", pattern = "^DSN_.*.tif",
#                              full.names = TRUE)
#
# id_82 <- grep("1981.12_15a", fls_chirps_dsn)
# id_13 <- grep("2013.11_15b", fls_chirps_dsn)
# fls_chirps_dsn <- fls_chirps_dsn[id_82:id_13]
# rst_chirps_dsn <- stack(fls_chirps_dsn)
#
# yrmn <- zoo::as.yearmon(substr(basename(fls_chirps_dsn), 17, 23), "%Y.%m")
# indices <- rep(1:(length(yrmn) / 6), each = 6)
# rst_chirps_dsn_ssn <- stackApply(rst_chirps_dsn, indices = indices, fun = sum)
# saveRDS(rst_chirps_dsn_ssn, file = "/media/fdetsch/dev/repositories/chirps/inst/extdata/dsn_ssn.rds")
rst_chirps_dsn_ssn <- readRDS("/media/fdetsch/dev/repositories/chirps/inst/extdata/dsn_ssn.rds")

lst_ssn_trends <- foreach(p = c(0.05, 0.01, 0.001)) %do% {

  # status message
  cat("Processing significance level", p, "...\n")

  lst_ssn_trends <-
    foreach(i = 1:4, j = list("DJF", "MAM", "JJA", "SON"),
            .packages = c("raster", "rgdal", "Kendall")) %dopar% {

              rst_ssn <- rst_chirps_dsn_ssn[[seq(i, nlayers(rst_chirps_dsn_ssn), 4)]]

              raster::calc(rst_ssn, fun = function(x) {
                gimms::significantTau(x, p = p, prewhitening = TRUE, conf.intervals = FALSE)
              }, filename = paste0("data/out/seasonal/chirps_mk", gsub("0\\.", "", p), "_", j, "_8213"),
              format = "GTiff", overwrite = TRUE)
            }

  rst_ssn_trends <- stack(lst_ssn_trends)

  png(paste0("data/vis/seasonal/chirps_mk", gsub("0\\.", "", p), "_ssn.png"),
      width = 14, height = 14, units = "cm", res = 500)
  p <- spplot(mask(rst_ssn_trends, spy_iran), col.regions = cols(1000),
              at = seq(-.75, .75, .01)) +
    latticeExtra::layer(sp.polygons(spy_iran, col = "black"))
  print(p)
  dev.off()

  return(rst_ssn_trends)
}

## deregister parallel backend
stopCluster(supcl)
