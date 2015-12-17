### environmental settings -----------------------------------------------------

## clear workspace
rm(list = ls(all = TRUE))

## required packages
lib <- c("gimms", "Rsenal", "doParallel", "RColorBrewer", "grid", "dplyr")
Orcs::loadPkgs(lib)

## parallelization
supcl <- makeCluster(detectCores() - 1)
registerDoParallel(supcl)

## paths
dir_extdata_gimms <- switch(Sys.info()[["sysname"]],
                            "Linux" = "/media/fdetsch/modis_data/gimms_iran",
                            "Windows" = "D:/gimms_iran")

dir_extdata_chirps <- switch(Sys.info()[["sysname"]],
                             "Linux" = "/media/fdetsch/modis_data/chirps",
                             "Windows" = "D:/chirps")


### data import ----------------------------------------------------------------

## reference extent from gadm
spy_iran <- if (!file.exists("data/GADM_2.7_IRN_adm0.rds")) {
  getData(country = "IRN", level = 0, path = "data")
} else {
  readRDS("data/GADM_2.7_IRN_adm0.rds")
}
suppressWarnings(proj4string(spy_iran) <- "+init=epsg:4326")

## gimms
# fls_gimms <- rearrangeFiles(dsn = paste0(dir_extdata_gimms, "/data/crp"),
#                             pattern = "^CRP_.*.tif", pos = c(4, 6, 11) + 4,
#                             full.names = TRUE)
#
# st_gimms <- grep("geo82jan15a", fls_gimms)
# nd_gimms <- grep("geo13dec15b", fls_gimms)
# fls_gimms <- fls_gimms[st_gimms:nd_gimms]
#
# # output filepath and suffixes
# file_out <- paste0(dir_extdata_gimms, "/data/mvc/MVC")
#
# suffix <- sapply(fls_gimms, Orcs::pureBasename)
# jnk <- foreach(i = c("15a", "15b"), .combine = "c") %do% {
#   suffix <- gsub(i, "", suffix)
# }
# suffix <- unique(suffix)
# suffix <- suffix[-which(duplicated(substr(suffix, 8, 12)))]
#
# # monthly maximum value composites (mvc)
# rst_gimms <- monthlyComposite(fls_gimms, pos1 = 4+4, pos2 = 8+4,
#                               filename = file_out, bylayer = TRUE,
#                               suffix = suffix, format = "GTiff",
#                               overwrite = TRUE)

# reimport
fls_gimms <- rearrangeFiles(dsn = paste0(dir_extdata_gimms, "/data/mvc"),
                            pattern = "^MVC.*.tif", full.names = TRUE)
rst_gimms <- stack(fls_gimms)

rst_gimms <- mask(rst_gimms, spy_iran)
mat_gimms <- as.matrix(rst_gimms)

## chirps
# fls_chirps <- list.files(paste0(dir_extdata_chirps, "/data/bimonthly"),
#                          pattern = "^chirps.*.tif$", full.names = TRUE)
#
# st_chirps <- grep("1982.01_15a", fls_chirps)
# nd_chirps <- grep("2013.12_15b", fls_chirps)
# fls_chirps <- fls_chirps[st_chirps:nd_chirps]
#
# # output filepath and suffixes
# file_out <- paste0(dir_extdata_chirps, "/data/svc/SVC")
#
# suffix <- sapply(fls_chirps, Orcs::pureBasename)
# jnk <- foreach(i = c("_15a", "_15b"), .combine = "c") %do% {
#   suffix <- gsub(i, "", suffix)
# }
# suffix <- unique(suffix)
#
# # monthly sums
# rst_chirps <- monthlyComposite(fls_chirps, pos1 = 13, pos2 = 19, fun = sum,
#                                filename = file_out, bylayer = TRUE,
#                                suffix = suffix, format = "GTiff",
#                                overwrite = TRUE)

# reimport
fls_chirps <- list.files(paste0(dir_extdata_chirps, "/data/svc"),
                         full.names = TRUE, pattern = "^SVC.*.tif")
rst_chirps <- stack(fls_chirps)
rst_chirps <- mask(rst_chirps, spy_iran)
mat_chirps <- as.matrix(rst_chirps)


### cross-correlation ----------------------------------------------------------

## loop over cells
dat_ccf <- foreach(i = 1:nrow(mat_chirps), .combine = "rbind",
                   .packages = "foreach") %dopar% {

  # per cell, loop over months (all values from jan, feb, ...)
  suppressWarnings(
    foreach(j = seq(12), month = month.abb, .combine = "rbind",
            .export = ls(envir = globalenv())) %do% {

      num_rain <- mat_chirps[i, seq(j, ncol(mat_chirps), 12)]

      # per monthly rain events, extract ndvi from lags 0 to 3
      dat_mod <- foreach(k = 0:3, .combine = "rbind",
                         .export = ls(envir = globalenv())) %do% {

        num_ndvi <- mat_gimms[i, seq(j+k, ncol(mat_chirps), 12)]

        # adjust length of rain vector to ndvi (towards the end of 2013)
        if (length(num_ndvi) < length(num_rain)) {
          int_diff <- length(num_rain) - length(num_ndvi)
          num_rain <- num_rain[-int_diff]
        }

        # correlation coefficient
        num_r <- cor(num_rain, num_ndvi)

        if (!is.na(num_r)) {
          num_p <- Orcs::pvalue(lm(num_ndvi ~ num_rain))
          data.frame(r = num_r, p = num_p, lag = k)
        } else {
          data.frame(r = NA, p = NA, lag = k)
        }
      }

      if (all(is.na(dat_mod$r)))
        data.frame(cell = i, month = month, r = NA, p = NA, lag = NA)
      else
        data.frame(cell = i,
                   month = month,
                   r = dat_mod$r,
                   p = dat_mod$p,
                   lag = dat_mod$lag)
    }
  )
}

saveRDS(dat_ccf, "data/ccf.rds")
dat_ccf <- readRDS("data/ccf.rds")

## limit to lag = [0;2] and identify maximum r, minimum p
dat_ccf %>%
  filter(lag != 3 | is.na(lag)) %>%
  group_by(cell, month) %>%
  filter(abs(r) == max(abs(r)) | is.na(r)) -> dat_ccf_r

dat_ccf %>%
  filter(lag != 3 | is.na(lag)) %>%
  group_by(cell, month) %>%
  filter(p == min(p) | is.na(p)) -> dat_ccf_p

## insert values
rst_lag <- rst_cor <- raster("inst/templates/template_chirps.tif")
rst_lag[] <- rst_cor[] <- NA

## colors
cols <- colorRampPalette(brewer.pal(9, "RdBu"))
cols_lag <- brewer.pal(9, "YlOrRd")[c(3, 5, 8)]

lst_ccf <- foreach(i = 1:12, month = month.abb, .packages = "Rsenal") %dopar% {
  id_month <- seq(i, nrow(dat_ccf_r), 12)

  dat <- dat_ccf_r[id_month, ]
  if (any(duplicated(dat$cell)))
    dat <- dat[-which(duplicated(dat$cell)), ]

  rst_cor_tmp <- raster::setValues(rst_cor, dat$r)

  # reject non-significant pixels
  rst_p_tmp <- raster::setValues(rst_cor, dat$p)
  rst_cor_tmp <- raster::overlay(rst_cor_tmp, rst_p_tmp, fun = function(x, y) {
    x[y[] >= 0.05] <- NA
    return(x)
  })

  p_cor_tmp <- sp::spplot(rst_cor_tmp, scales = list(draw = TRUE, cex = .7),
                          at = seq(-1, 1, .01), col.regions = cols(1000),
                          colorkey = list(space = "top", width = .5, height = .5),
                          main = list("Pearson's r", cex = .8),
                          sp.layout = list("sp.text", loc = c(45.5, 25.85),
                                           txt = month, font = 2, cex = .7),
                          xlab = list("Longitude", cex = .8),
                          ylab = list("Latitude", cex = .8)) +
    latticeExtra::layer(sp.polygons(spy_iran))


  rst_lag_tmp <- raster::setValues(rst_lag, dat$lag)

  # reject non-significant pixels
  rst_lag_tmp <- raster::overlay(rst_lag_tmp, rst_p_tmp, fun = function(x, y) {
    x[y[] >= 0.05] <- NA
    return(x)
  })

  rst_lag_tmp <- raster::ratify(rst_lag_tmp)
  rat_lag_tmp <- levels(rst_lag_tmp)[[1]]
  levels(rst_lag_tmp) <- rat_lag_tmp

  #   p_lag_tmp <- sp::spplot(rst_lag_tmp, "ID", scales = list(draw = TRUE, cex = .7),
  #                           at = -.5:3.5, col.regions = envinmrPalette(4),
  #                           colorkey = list(space = "top", width = .5, height = .5),
  #                           main = list("lag (months)", cex = .8),
  #                           sp.layout = list("sp.text", loc = c(45.5, 25.85),
  #                                            txt = month, font = 2, cex = .7)) +
  #     latticeExtra::layer(sp.polygons(spy_iran))

  p_lag_tmp <- rasterVis::levelplot(rst_lag_tmp, att = "ID",
                                    scales = list(draw = TRUE, cex = .7),
                                    col.regions = cols_lag,
                                    colorkey = list(space = "top", width = .5,
                                                    height = .25, labels = list(cex = .7)),
                                    main = list("Lag (months)", cex = .8, hjust = .1),
                                    xlab = list("Longitude", cex = .8),
                                    ylab = list("Latitude", cex = .8)) +
    latticeExtra::layer(sp.polygons(spy_iran)) +
    latticeExtra::layer(sp.text(loc = c(45.5, 25.85), txt = month, font = 2,
                                cex = .7), data = list(month = month))


  list(p_cor_tmp, p_lag_tmp)
}

# figure r
lst_p_cor <- lapply(lst_ccf, function(i) i[[1]])
p_cor <- latticeCombineGrid(lst_p_cor, layout = c(4, 3))

png(paste0(dir_extdata_chirps, "/vis/ccf_r.png"), width = 20, height = 16,
    units = "cm", res = 500)
print(p_cor)
dev.off()

# figure lag
lst_p_lag <- lapply(lst_ccf, function(i) i[[2]])
p_lag <- latticeCombineGrid(lst_p_lag, layout = c(4, 3))

png(paste0(dir_extdata_chirps, "/vis/ccf_lag.png"), width = 20, height = 16,
    units = "cm", res = 500)
print(p_lag)
dev.off()

## deregister parallel backend
stopCluster(cl)
