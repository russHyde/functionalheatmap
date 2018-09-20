###############################################################################

# setup data-structures for a heatmap

###############################################################################

#' setup_heatmap: defines the data-structures for use in constructing heatmaps
#'
#' @param        x             A poly_frame or list of named data-frames. This
#'   must contain a data-frame called body_data which defines the data used in
#'   the body of the heatmap (by two index columns and a value column).
#'
#' @importFrom   magrittr      %>%
#' @importFrom   tidyr         spread_
#'
#' @include      data_manipulation.R
#'
#' @export

setup_heatmap <- function(x) {
  row_index <- "feature_id"
  col_index <- "sample_id"
  value_index <- "fitted_value"

  stopifnot(
    .is_nonempty_list_of_data_frames(x) && "body_data" %in% names(x)
  )
  body_matrix <- tidyr::spread_(
    x$body_data,
    value_col = value_index,
    key_col = col_index
  ) %>%
    as_matrix(rowname_col = row_index)

  list(body_matrix = body_matrix)
}

###############################################################################
