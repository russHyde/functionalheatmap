###############################################################################

# Obtain `ComplexHeatmap` objects from `heatmap_data` objects

###############################################################################

.get_row_annotation_object <- function(x) {
  stopifnot(is(x, "heatmap_data"))

  stopifnot("row_annotations" %in% names(x))

  do.call(
    ComplexHeatmap::HeatmapAnnotation,
    append(
      list(df = x$row_annotations, which = "row"),
      x$row_dots
    )
  )
}

#' @importFrom   methods       new
#'
.get_top_annotation_object <- function(x) {
  stopifnot(is(x, "heatmap_data"))

  if ("top_annotations" %in% names(x)) {
    do.call(
      ComplexHeatmap::HeatmapAnnotation,
      append(
        list(df = x$top_annotations, which = "column"),
        x$top_dots
      )
    )
  } else {
    methods::new("HeatmapAnnotation")
  }
}

###############################################################################
