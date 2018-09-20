###############################################################################

# setup data-structures for a heatmap

###############################################################################

#' setup_heatmap: defines the data-structures for use in constructing heatmaps
#'
#' @export
setup_heatmap <- function(x) {
  if(is.matrix(x$bodydata)){
    stop()
  }
  x
}

###############################################################################
