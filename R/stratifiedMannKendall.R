#' Compute stratified Mann-Kendall trend tests
#'
#' @description
#' In contrast to most existing implementations, this function allows you to
#' apply pixel-based Mann-Kendall trend tests not only to a raw 'RasterStack' of
#' time series observations, but also to stratified subsets of the very same,
#' i.e., across monthly (Jan-Dec) and seasonal ('DJF', 'MAM', 'JJA', 'SON')
#' chunks of layers.
#'
#' @param x A 'RasterStack' consisting of consecutive time series observations.
#' @param type 'character'; one of 'raw' (take the input series as is),
#' 'monthly' (Mann-Kendall test across unique months) and 'seasonal'
#' (Mann-Kendall test across unique seasons).
#' @param p Significance level to be tested.
#' @param prewhitening 'logical'. If \code{TRUE}, pre-whitening is applied prior
#' to the actual Mann-Kendall trend test.
#' @param cores Number of cores for parallel processing.
#'
#' @return
#' A 'Raster*' object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{significantTau}}, \code{\link{confint}}.
#'
#' @examples
#' \dontrun{
#' ## overall trend
#' overall_trend = stratifiedMannKendall(baleCHIRPS.v2, p = 1)
#' spplot(overall_trend)
#'
#' ## monthly trend
#' monthly_trend = stratifiedMannKendall(baleCHIRPS.v2, type = "monthly", p = 0.05)
#' names(monthly_trend) = month.abb
#' spplot(monthly_trend)
#'
#' ## seasonal trend
#' seasons_trend = stratifiedMannKendall(baleCHIRPS.v2, type = "seasonal", p = 0.05)
#' names(seasons_trend) = c("DJF", "MAM", "JJA", "SON")
#' spplot(seasons_trend)
#' }
#'
#' @export stratifiedMannKendall
#' @name stratifiedMannKendall
stratifiedMannKendall <- function(x, type = c("raw", "monthly", "seasonal"),
                                  p = 0.001, prewhitening = TRUE, cores = 1L) {

  type = type[1]

  ## parallelize (optional)
  cl <- parallel::makePSOCKcluster(cores)
  on.exit(parallel::stopCluster(cl))


  ### raw method -----

  if (type == "raw") {

    # loop over all desired significance levels
    lst_trends <- raster::calc(x, fun = function(y) {
      gimms::significantTau(y, p = p, prewhitening = prewhitening)
    })

  } else if (type %in% c("monthly", "seasonal")) {

    n = raster::nlayers(x)
    parallel::clusterExport(cl, c("x", "n"), envir = environment())


    ### monthly method -----

    if (type == "monthly") {

      # loop over monthly layers
      lst_trends <- parallel::parLapply(cl, 1:12, function(i) {

        rst_mts = raster::subset(x, seq(i, n, 12))

        raster::calc(rst_mts, fun = function(y) {
          gimms::significantTau(y, p = p, prewhitening = prewhitening)
        })
      })


    ### seasonal method -----

    } else if (type == "seasonal") {

      # loop over seasonal layers
      lst_trends <- parallel::parLapply(cl, 1:4, function(i) {

        rst_ssn = raster::subset(x, seq(i, n, 4))

        raster::calc(rst_ssn, fun = function(y) {
          gimms::significantTau(y, p = p,
                                prewhitening = prewhitening)
        })
      })
    }


  } else {
    stop("Invalid 'type' argument.\n")
  }

  ## return list with different significance layers
  return(raster::stack(lst_trends))
}
