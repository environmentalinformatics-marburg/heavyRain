################################################################################
### global settings
################################################################################

## working directory
setwd("/media/fdetsch/dev/chirps/")

## required packages
lib <- c("plyr", "Rsenal", "RColorBrewer", "grid", "lattice")
Orcs::loadPkgs(lib)

## required functions
source("R/bfastTemplate.R")

################################################################################
### breakpoint detection (see http://www.loicdutrieux.com/bfastSpatial/)
################################################################################

## import bfast data
fls_bfast <- list.files("data", pattern = "bfast", full.names = TRUE)
lst_bfast <- lapply(fls_bfast, function(i) {
  do.call("rbind.fill", readRDS(i))
})
dat_bfast <- do.call("rbind.fill", lst_bfast)

## raster template
rst_template <- bfastTemplate(local = TRUE, dsn = "data")
rst_breaks_counts <- rst_breaks_maxtime <- rst_breaks_maxmagn <- rst_template

## number of breakpoints
rst_breaks_counts[] <- dat_bfast[, "bp_vt_ct"]
rst_breaks_counts <- mask(rst_breaks_counts, spy_iran)

# col_counts <- brewer.pal(5, "YlGnBu")
p_counts <- spplot(rst_breaks_counts, col.regions = "cornflowerblue",
                   at = seq(.5, 1.5, 1), scales = list(draw = TRUE),
                   colorkey = FALSE) +
  latticeExtra::layer(sp.polygons(spy_iran))

## most intense breakpoint
rst_breaks_maxmagn[] <- dat_bfast[, "bp_vt_maxmagn"]
rst_breaks_maxmagn <- mask(rst_breaks_maxmagn, spy_iran)

col_maxmagn <- colorRampPalette(brewer.pal(9, "BrBG"))

# int_id <- which(rst_breaks_maxmagn[] > .25)
# spt_maxval <- xyFromCell(rst_breaks_maxmagn, int_id, spatial = TRUE)
# rst_breaks_maxmagn[int_id] <- NA

# brks <- quantile(rst_breaks_maxmagn, seq(0, 1, length.out = 256))
p_maxmagn <- spplot(rst_breaks_maxmagn, col.regions = col_maxmagn(1000),
                    at = seq(-39, 39, .1), scales = list(draw = TRUE)) +
  latticeExtra::layer(sp.polygons(spy_iran))

rst_breaks_maxtime[] <- dat_bfast[, "bp_vt_maxtime"]
rst_breaks_maxtime <- mask(rst_breaks_maxtime, spy_iran)

col_maxtime <- envinmrPalette(1000)[100:1000]
p_maxtime <- spplot(rst_breaks_maxtime, col.regions = col_maxtime,
                    at = seq(1985, 2010, 1), scales = list(draw = TRUE),
                    colorkey = FALSE) +
  latticeExtra::layer(sp.polygons(spy_iran))

p_comb <- latticeCombineGrid(list(p_counts, p_maxtime, p_maxmagn),
                             layout = c(3, 1))

# store
png("out/chirps_bfast_trend.png", width = 21, height = 13, units = "cm",
    res = 500)
grid.newpage()

vp0 <- viewport(x = 0, y = 0, width = 1, height = .9,
                just = c("left", "bottom"), name = "vp_count")
pushViewport(vp0)
print(p_comb, newpage = FALSE)

# key left
upViewport()
vp1 <- viewport(x = .06, y = .72, width = 1/3, height = .1,
                just = c("left", "bottom"), name = "vp_key1")
pushViewport(vp1)
# draw.colorkey(key = list(col = "cornflowerblue", width = 1, height = .5,
#                          at = seq(.5, 1.5, 1),
#                          space = "bottom"), draw = TRUE)
grid.text("Trend breakpoints", x = 0.5, y = 1.2, just = c("centre", "top"),
          gp = gpar(font = 2, cex = .85))

# key middle
upViewport()
vp2 <- viewport(x = 1/3 + .02, y = .72, width = 1/3, height = .1,
                just = c("left", "bottom"), name = "vp_key2")
pushViewport(vp2)
draw.colorkey(key = list(col = col_maxtime,
                         width = 1, height = .5,
                         at = 1985:2010, space = "bottom"), draw = TRUE)
grid.text("Timing of biggest change", x = 0.5, y = 1.2,
          just = c("centre", "top"), gp = gpar(font = 2, cex = .85))

# key right
upViewport()
vp3 <- viewport(x = 2/3 - .02, y = .72, width = 1/3, height = .1,
                just = c("left", "bottom"), name = "vp_key3")
pushViewport(vp3)
draw.colorkey(key = list(col = col_maxmagn(1000), width = 1, height = .5,
                         at = seq(-39, 39, .1), space = "bottom"), draw = TRUE)
grid.text("Magnitude of biggest change", x = 0.5, y = 1.2,
          just = c("centre", "top"), gp = gpar(font = 2, cex = .85))

dev.off()
