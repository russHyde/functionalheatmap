###############################################################################

#

###############################################################################

# Default formatting: no rownames, no column clustering

#' format_heatmap
#'
#' @param        x             A heatmap_data object. As returned by
#'   `setup_heatmap`.
#' @param        ...           Any user-specific formatting flags to be passed
#'   to `Heatmap()`. An exception is `split`: if this flag is set it should
#'   define a vector of column-names from the `row_data` part of `x`, these
#'   columns are then extracted and passed to `Heatmap()`.
#'
#' @importFrom   methods       is
#' @export

format_heatmap <- function(x, ...) {
  if (!methods::is(x, "heatmap_data")) {
    stop("`x` should be a `heatmap_data` object in `format_heatmap`")
  }

  dots <- list(...)

  initial_formats <- if ("formats" %in% names(x)) {
    x$formats
  } else {
    .get_default_formatting()
  }

  formats <- initial_formats %>%
    .append_or_update(dots) %>%
    .append_split_args_if_defined(x, dots$split)

  x %>%
    .append_or_update(
      list(formats = formats)
    ) %>%
    as_heatmap_data()
}

###############################################################################

.get_default_formatting <- function() {
  list(
    show_row_names = FALSE,
    cluster_columns = FALSE
  )
}

###############################################################################

.append_or_update <- function(current_list, new_list) {
  non_updated_args <- setdiff(names(current_list), names(new_list))
  append(new_list, current_list[non_updated_args])
}

.get_split_from_row_data <- function(x, split_columns) {
  stopifnot("row_data" %in% names(x))
  row_data <- x$row_data
  row_data[split_columns]
}

.append_split_args_if_defined <- function(current_args,
                                          heatmap_data,
                                          split_columns = NULL) {
  if (is.null(split_columns)) {
    return(current_args)
  }
  .append_or_update(
    current_args,
    list(split = .get_split_from_row_data(heatmap_data, split_columns))
  )
}


###############################################################################
