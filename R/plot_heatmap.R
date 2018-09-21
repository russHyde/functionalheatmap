###############################################################################

#

###############################################################################

#' ? should be plot.heatmap_data method
#'
#' @param        x             A `heatmap_data` object. As returned by
#'   `setup_heatmap` or `setup_heatmap_formatting`.
#'
#' @importFrom   methods       is
#' @export

plot_heatmap <- function (x) {
  if(missing(x) || !is(x, "heatmap_data")) {
    stop("`x` should be a defined `heatmap_data` object in `plot_heatmap`")
  }
}

###############################################################################
