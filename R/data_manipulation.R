###############################################################################

#' Converts a tidy-data-frame into a matrix using specified columns of the
#' data-frame to index the rows and columns of the resulting matrix
#'
#' @importFrom   tidyr         spread_
#' @noRd

as_body_matrix <- function(df, row_index, column_index, value_index) {
  as_matrix(
    df = tidyr::spread_(
      df,
      value_col = value_index,
      key_col = column_index
    ),
    rowname_col = row_index
  )
}

#' Converts a data-frame into a matrix
#'
#' If the user specifies `rowname_col`, this column is extracted from the
#' data-frame and used to define the rownames of the matrix.
#'
#' @importFrom   magrittr      set_rownames
#' @noRd

as_matrix <- function(df, rowname_col = NULL) {
  if (is.null(rowname_col)) {
    as.matrix(df)
  } else {
    rownames <- df[[rowname_col]]
    colnames <- setdiff(colnames(df), rowname_col)
    magrittr::set_rownames(
      x = as.matrix(df[colnames]),
      value = rownames
    )
  }
}

###############################################################################
