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
#' @param        row_dots      Additional arguments to be passed to
#'   HeatmapAnnotation for the row-annotations. For example, `show_legend`,
#'   `col`. Check the docs for HeatmapAnnotation for parameter names.
#'
#' @importFrom   magrittr      %>%
#' @importFrom   methods       is
#' @export

annotate_heatmap <- function(x,
                             row_annotations = NULL,
                             row_dots = NULL,
                             top_annotations = NULL,
                             top_dots = NULL) {
  if (!methods::is(x, "heatmap_data")) {
    stop("`x` should be a `heatmap_data` object in `annotate_heatmap`")
  }

  x %>%
    .append_row_annotations(row_annotations) %>%
    .append_top_annotations(top_annotations) %>%
    .append_argument_list(arg_list_name = "row_dots", arg_list = row_dots) %>%
    .append_argument_list(arg_list_name = "top_dots", arg_list = top_dots)
}

###############################################################################

.append_argument_list <- function(x, arg_list_name = NULL, arg_list = NULL) {
  if (!is.null(arg_list)) {
    x[[arg_list_name]] <- arg_list
  }
  x
}

.append_to_heatmap_data_by_column <- function(x,
                                              annotation_track_names = NULL,
                                              annotation_df_name = NULL,
                                              output_df_name = NULL) {
  if (!is.null(annotation_track_names)) {
    stopifnot(
      annotation_df_name %in% names(x) &&
        is.data.frame(x[[annotation_df_name]])
    )
    x[[output_df_name]] <- x[[annotation_df_name]][annotation_track_names]
  }
  x
}

###############################################################################

.append_row_annotations <- function(x, row_annotations = NULL) {
  .append_to_heatmap_data_by_column(
    x, row_annotations, "row_data", "row_annotations"
  )
}

.append_top_annotations <- function(x, top_annotations = NULL) {
  .append_to_heatmap_data_by_column(
    x, top_annotations, "column_data", "top_annotations"
  )
}

###############################################################################
