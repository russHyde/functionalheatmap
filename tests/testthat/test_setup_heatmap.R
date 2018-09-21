###############################################################################

context("Tests for `setup_heatmap`")

###############################################################################

basic_hmd <- function(body_matrix) {
  as_heatmap_data(list(body_matrix = body_matrix))
}

###############################################################################

test_that("`setup_heatmap` does not work for invalid input", {
  expect_error(
    setup_heatmap(),
    info = "setup_heatmap without arguments should throw an error"
  )

  expect_error(
    setup_heatmap(x = list(body_data = matrix(), 1:10, TRUE)),
    info = "setup_heatmap should receive a list of data-frames"
  )

  expect_error(
    setup_heatmap(x = list(a = data.frame())),
    info = "`bodydata` should be present"
  )
})

###############################################################################

test_that("`setup_heatmap` works for valid input", {
  df1 <- data.frame(
    feature_id = letters[1:3],
    sample_id = rep(LETTERS[1:3], each = 3),
    fitted_value = 1:9
  )

  m1 <- matrix(
    1:9,
    nrow = 3, ncol = 3,
    dimnames = list(letters[1:3], LETTERS[1:3])
  )

  expect_equal(
    setup_heatmap(list(body_data = df1)),
    basic_hmd(m1),
    info = "If only bodydata is provided, it's converted to a matrix"
  )
})

###############################################################################

test_that("user can define row, column and value indexes in `setup_heatmap`", {
  df1 <- data.frame(
    ensembl_id = letters[1:3],
    treatment_group = rep(LETTERS[1:3], each = 3),
    normalised_expression = 1:9
  )

  m1 <- matrix(
    1:9,
    nrow = 3, ncol = 3,
    dimnames = list(letters[1:3], LETTERS[1:3])
  )

  expect_equal(
    object = setup_heatmap(
      list(body_data = df1),
      row_index = "ensembl_id",
      column_index = "treatment_group",
      value_index = "normalised_expression"
    ),
    expected = basic_hmd(m1),
    info = paste(
      "[row|column|value]_index should be used to construct body_matrix from",
      "body_df"
    )
  )
})

###############################################################################
