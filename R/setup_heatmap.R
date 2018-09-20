###############################################################################

# setup data-structures for a heatmap

###############################################################################

#' setup_heatmap: defines the data-structures for use in constructing heatmaps
#'
#' @export
setup_heatmap <- function(x) {
  stopifnot(
    .is_nonempty_list_of_data_frames(x) && "bodydata" %in% names(x)
  )
  x
}

###############################################################################
