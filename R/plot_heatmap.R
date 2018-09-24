###############################################################################

#

###############################################################################

#' ? should be plot.heatmap_data method
#'
#' @param        x             A `heatmap_data` object. As returned by
#'   `setup_heatmap` or `format_heatmap`.
#'
#' @importFrom   methods       is
#' @importFrom   ComplexHeatmap   Heatmap
#'
#' @export

plot_heatmap <- function(x) {
  if (missing(x) || !methods::is(x, "heatmap_data")) {
    stop("`x` should be a defined `heatmap_data` object in `plot_heatmap`")
  }
  do.call(
    ComplexHeatmap::Heatmap,
    append(
      list(x$body_matrix),
      x$formats
    )
  )
}

###############################################################################
