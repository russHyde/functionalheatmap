###############################################################################

#

###############################################################################

.get_default_formatting <- function() {
  list(
    show_row_names = FALSE,
    cluster_columns = FALSE
  )
}

# Default formatting: no rownames, no column clustering

#' format_heatmap
#'
#' @param        x             A heatmap_data object. As returned by
#'   `setup_heatmap`.
#' @param        ...           Any user-specific formatting flags to be passed
#'   to Heatmap().
#'
#' @importFrom   methods       is
#' @export

format_heatmap <- function(x, ...) {
  if (!methods::is(x, "heatmap_data")) {
    stop("`x` should be a `heatmap_data` object in `format_heatmap`")
  }

  formats <- list(...)
  defaults <- .get_default_formatting()

  formats <- append(
    formats,
    defaults[!names(defaults) %in% names(formats)]
  )

  x %>%
    append(list(formats = formats)) %>%
    as_heatmap_data()
}

###############################################################################
