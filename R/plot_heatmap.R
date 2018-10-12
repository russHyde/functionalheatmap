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
#' @include      to_complex_heatmap_object.R
#' @export

plot_heatmap <- function(x) {
  if (missing(x) || !methods::is(x, "heatmap_data")) {
    stop("`x` should be a defined `heatmap_data` object in `plot_heatmap`")
  }

  top_annot <- .get_top_annotation_object(x)

  heatmap <- do.call(
    ComplexHeatmap::Heatmap,
    append(
      list(matrix = x$body_matrix, top_annotation = top_annot),
      x$formats
    )
  )

  if ("row_annotations" %in% names(x)) {
    ra <- .get_row_annotation_object(x)
    heatmap <- ComplexHeatmap::add_heatmap(heatmap, ra)
  }
  heatmap
}

###############################################################################
