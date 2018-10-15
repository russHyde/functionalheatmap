###############################################################################

# setup data-structures for a heatmap

###############################################################################

#' setup_heatmap: defines the data-structures for use in constructing heatmaps
#'
#' @param        x             A poly_frame or list of named data-frames. This
#'   must contain a data-frame called body_data which defines the data used in
#'   the body of the heatmap (by two index columns and a value column).
#' @param        row_index,column_index,value_index   In the data-frames
#'   provided, which column contains the data for the rows / columns / body of
#'   the heatmap?
#'
#' @importFrom   magrittr      %>%
#' @importFrom   tidyr         spread_
#'
#' @include      data_manipulation.R
#'
#' @export

setup_heatmap <- function(x,
                          row_index = "feature_id",
                          column_index = "sample_id",
                          value_index = "fitted_value") {
  stopifnot(
    .is_nonempty_list_of_data_frames(x) && "body_data" %in% names(x)
  )

  keep_features <- .get_features_from_heatmap_input(x, row_index)
  keep_samples <- .get_samples_from_heatmap_input(x, column_index)

  body_matrix <- as_body_matrix(
    x$body_data,
    row_index, column_index, value_index
  )[keep_features, keep_samples]

  heatmap_list <- list(
    body_matrix = body_matrix
  )

  if ("row_data" %in% names(x)) {
    # add the row data to the heatmap-data, ensureing the row-ordering matches
    # that for the body-data
    reordering <- match(keep_features, x$row_data[[row_index]])
    heatmap_list$row_data <- x$row_data[reordering, ]
  }

  if ("column_data" %in% names(x)) {
    # order of the 'samples' in the column_data data-frame should determine
    # the order of the samples in the resulting heatmap
    reordering <- match(keep_samples, x$column_data[[column_index]])
    heatmap_list$column_data <- x$column_data[reordering, ]
  }

  as_heatmap_data(heatmap_list)
}

###############################################################################

# helper functions

.get_features_from_heatmap_input <- function(x, row_index) {
  features <- unique(x$body_data[[row_index]])

  if ("row_data" %in% names(x)) {
    features <- intersect(features, x$row_data[[row_index]])
  }

  sort(features)
}

.get_samples_from_heatmap_input <- function(x, column_index) {
  samples <- unique(x$body_data[[column_index]])

  if ("column_data" %in% names(x)) {
    # ensure smaple-ordering is as specified by the column-data
    samples <- intersect(x$column_data[[column_index]], samples)
  }

  samples
}

###############################################################################
