###############################################################################

#

###############################################################################

#' Add row annotations to a heatmap based on columns from the `row_data` entry
#' of a `heatmap_data` object
#'
#' @param        x             A heatmap_data object.
#' @param        row_annotations   Which columns of the `row_data` entry in `x`
#'   should be used when plotting annotations alongside the rows of the
#'   heatmap?
#'
#' @importFrom   methods       is
#' @export

annotate_heatmap <- function(x,
                             row_annotations = NULL) {
  if (!methods::is(x, "heatmap_data")) {
    stop("`x` should be a `heatmap_data` object in `annotate_heatmap`")
  }

  .append_row_annotations(x, row_annotations)
}

###############################################################################

.append_row_annotations <- function(x, row_annotations = NULL) {
  if (!is.null(row_annotations)) {
    x$row_annotations <- x$row_data[row_annotations]
  }
  x
}

###############################################################################
