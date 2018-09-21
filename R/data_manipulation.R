###############################################################################

#' converts a data-frame into a matrix
#'
#' If the user specifies `rowname_col`, this column is extracted from the
#' data-frame and used to define the rownames of the matrix
#'
#' @importFrom   dplyr         select_
#' @importFrom   magrittr      set_rownames   %>%
#' @noRd

as_matrix <- function(df, rowname_col = NULL) {
  if (is.null(rowname_col)) {
    as.matrix(df)
  } else {
    as.matrix(
      dplyr::select_(df, .dots = paste0("-", rowname_col))
    ) %>%
      magrittr::set_rownames(df[[rowname_col]])
  }
}

###############################################################################
