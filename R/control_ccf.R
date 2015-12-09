### environmental settings -----------------------------------------------------

## clear workspace
rm(list = ls(all = TRUE))

## required packages
lib <- c("gimms", "Rsenal", "doParallel", "RColorBrewer", "grid")
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
fls_gimms <- rearrangeFiles(dsn = paste0(dir_extdata_gimms, "/data/crp"),
                            pattern = "^CRP_.*.tif", pos = c(4, 6, 11) + 4,
                            full.names = TRUE)

st_gimms <- grep("geo82jan15a", fls_gimms)
nd_gimms <- grep("geo13dec15b", fls_gimms)
fls_gimms <- fls_gimms[st_gimms:nd_gimms]
rst_gimms <- stack(fls_gimms)
rst_gimms <- mask(rst_gimms, spy_iran)
mat_gimms <- as.matrix(rst_gimms)

## chirps
fls_chirps <- list.files(paste0(dir_extdata_chirps, "/data/bimonthly"),
                         pattern = "^chirps.*.tif$", full.names = TRUE)

st_chirps <- grep("1982.01_15a", fls_chirps)
nd_chirps <- grep("2013.12_15b", fls_chirps)
fls_chirps <- fls_chirps[st_chirps:nd_chirps]
rst_chirps <- stack(fls_chirps)
rst_chirps <- mask(rst_chirps, spy_iran)
mat_chirps <- as.matrix(rst_chirps)

## ccf
dat_ccf <- foreach(i = 1:nrow(mat_gimms), .combine = "rbind") %dopar% {

  # current values
  val_chirps <- mat_chirps[i, ]
  val_gimms <- mat_gimms[i, ]

  if (any(is.na(val_chirps)) | any(is.na(val_gimms)) |
      length(unique(val_chirps)) == 1 | length(unique(val_gimms)) == 1) {
    return(data.frame(cell = i, acf = NA, lag = NA))
  } else {
    ccf_results <- ccf(val_gimms, val_chirps, lag.max = 6, na.action = na.omit)
    num_acf <- max(ccf_results$acf[,, 1][7:13])
    num_lag <- ccf_results$lag[,, 1][7:13][which.max(ccf_results$acf[,, 1][7:13])]

    return(data.frame(cell = i, acf = num_acf, lag = num_lag))
  }
}

## insert values
rst_lag <- rst_acf <- rst_chirps[[1]]
rst_lag[] <- rst_acf[] <- NA
rst_lag[] <- dat_ccf$lag
rst_acf[] <- dat_ccf$acf

## create figure
cols_acf <- colorRampPalette(rev(brewer.pal(5, "RdGy")))
p_acf <- spplot(rst_acf, scales = list(draw = TRUE),
                at = seq(-.65, .65, .01), col.regions = cols_acf(1000),
                colorkey = list(space = "top", width = .75, height = 1),
                main = "r\n") +
  latticeExtra::layer(sp.polygons(spy_iran))

cols_lag <- colorRampPalette(brewer.pal(5, "YlGn"))
p_lag <- spplot(rst_lag, scales = list(draw = TRUE),
                at = seq(-.5, 6.5, 1), col.regions = envinmrPalette(7)) +
  latticeExtra::layer(sp.polygons(spy_iran))

p <- latticeCombineGrid(list(p_acf, p_lag), layout = c(1, 2))

png("data/vis/ccf.png", width = 14, height = 20, units = "cm", res = 500)
grid.newpage()
vp0 <- viewport(x = 0, y = .1,
                height = .9, width = 1,
                just = c("left", "bottom"),
                name = "fig.vp")
pushViewport(vp0)
print(p, newpage = FALSE)

vp1 <- viewport(x = .5, y = -.06,
                height = 0.1, width = .75,
                just = c("center", "bottom"),
                name = "key.vp")
pushViewport(vp1)
lattice::draw.colorkey(key = list(col = envinmrPalette(7), width = .75, height = .75,
                                  at = 0:7, space = "bottom"), draw = TRUE)
grid.text("lag (1/2 months)", x = 0.5, y = -.1, just = c("centre", "bottom"),
          gp = gpar(font = 2, cex = .85))

dev.off()

## deregister parallel backend
stopCluster(cl)
