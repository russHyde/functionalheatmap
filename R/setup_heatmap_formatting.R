###############################################################################

#

###############################################################################

#' setup_heatmap_formatting
#'
#' @param        x             A heatmap_data object. As returned by
#'   `setup_heatmap`.
#'
#' @importFrom   methods       is
#' @export

setup_heatmap_formatting <- function(x) {
  if (! methods::is(x, "heatmap_data")) {
    stop("`x` should be a `heatmap_data` object in `setup_heatmap_formatting`")
  }
  x
}

###############################################################################
