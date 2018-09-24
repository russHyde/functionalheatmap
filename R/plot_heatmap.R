###############################################################################

#

###############################################################################

#' ? should be plot.heatmap_data method
#'
#' @param        x             A `heatmap_data` object. As returned by
#'   `setup_heatmap` or `format_heatmap`.
#'
#' @importFrom   methods       is
#' @importFrom   ComplexHeatmap   Heatmap   add_heatmap   HeatmapAnnotation
#'
#' @export

plot_heatmap <- function(x) {
  if (missing(x) || !is(x, "heatmap_data")) {
    stop("`x` should be a defined `heatmap_data` object in `plot_heatmap`")
  }
  heatmap <- do.call(
    ComplexHeatmap::Heatmap,
    append(
      list(x$body_matrix),
      x$formats
    )
  )

  if ("row_annotations" %in% names(x)) {
    ra <- ComplexHeatmap::HeatmapAnnotation(
      x$row_annotations,
      which = "row"
    )
    heatmap <- ComplexHeatmap::add_heatmap(heatmap, ra)
  }
  heatmap
}

###############################################################################
