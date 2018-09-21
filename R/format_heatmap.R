###############################################################################

#

###############################################################################

#' format_heatmap
#'
#' @param        x             A heatmap_data object. As returned by
#'   `setup_heatmap`.
#'
#' @importFrom   methods       is
#' @export

format_heatmap <- function(x) {
  if (!methods::is(x, "heatmap_data")) {
    stop("`x` should be a `heatmap_data` object in `format_heatmap`")
  }
  x
}

###############################################################################
