#' Compute stratified Mann-Kendall trend tests
#'
#' @description
#' In contrast to most existing implementation, this function allows you to
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
#' to the actual Mann-Kendall trend test. See also \code{\link{significantTau}}
#' and description of package \strong{zyp}
#' (\url{https://cran.r-project.org/web/packages/zyp/zyp.pdf}).
#' @param cores If desired number of cores to use for parallel processing.
#'
#' @return
#' A 'list' with single entries corresponding to the significance levels in
#' \code{p}.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link{significantTau}}.
#'
#' @export stratifiedMannKendall
#' @name stratifiedMannKendall
stratifiedMannKendall <- function(x, type = c("raw", "monthly", "seasonal"),
                                  p = 0.001, prewhitening = TRUE, cores = 1L) {

  ## packages
  lib <- c("foreach", "Rsenal")
  jnk <- sapply(lib, function(x) library(x, character.only = TRUE))

  ## parallelize
  if (cores > 1L) {
    library(doParallel)
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
  }

  ##############################################################################
  ## raw method
  if (type == "raw") {

    # status message
    cat("Raw data is in. Start processing...\n")

    # loop over all desired significance levels
    lst_trends <- foreach(p = p, .packages = lib) %dopar% {
      calc(x, fun = function(x) {
             significantTau(x, p = p, prewhitening = prewhitening,
                            conf.intervals = FALSE)
           })
    }

    ##############################################################################
    ## monthly method
  } else if (type == "monthly") {

    # loop over all desired significance levels
    lst_trends <- foreach(p = p) %do% {

      # status message
      cat("Processing significance level", p, "...\n")

      # loop over monthly layers
      suppressWarnings(
        lst_trend <-
          foreach(i = 1:12, j = month.abb,
                  .packages = lib, .export = "significantTau") %dopar% {

                    rst_mnth <- x[[seq(i, nlayers(x), 12)]]

                    raster::calc(rst_mnth, fun = function(x) {
                      Rsenal::significantTau(x, p = p,
                                             prewhitening = prewhitening,
                                             conf.intervals = FALSE)
                    })
                  }
      )

      # return stacked trend layers
      return(stack(lst_trend))
    }


    ##############################################################################
    ## seasonal method
  } else if (type == "seasonal") {

    # loop over all desired significance levels
    lst_trends <- foreach(p = p) %do% {

      # status message
      cat("Processing significance level", p, "...\n")

      # loop over seasonal layers
      suppressWarnings(
        lst_trend <-
          foreach(i = 1:4, j = list("DJF", "MAM", "JJA", "SON"),
                  .packages = c("raster", "rgdal", "Kendall"),
                  .export = "significantTau") %dopar% {

                    rst_ssn <- x[[seq(i, nlayers(x), 4)]]

                    raster::calc(rst_ssn, fun = function(x) {
                      Rsenal::significantTau(x, p = p,
                                             prewhitening = prewhitening,
                                             conf.intervals = FALSE)
                    })
                  }
      )

      # return stacked trend layers
      return(raster::stack(lst_trend))
    }

  } else {
    stop("Invalid 'type' argument.\n")
  }

  ## deregister parallel backend
  if (cores > 1L)
    parallel::stopCluster(cl)

  ## return list with different significance layers
  return(lst_trends)
}
