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

test_that("only features common to the body and row-data are heatmapped", {
  # row_data features and body_data features are identical and order-matched
  body1 <- data.frame(
    feature_id = letters[1:3],
    sample_id = rep(LETTERS[1:3], each = 3),
    fitted_value = 1:9
  )

  rows1 <- data.frame(
    feature_id = letters[1:3],
    annotation = c(TRUE, FALSE, TRUE)
  )

  body1_matrix <- matrix(
    1:9, nrow = 3, dimnames = list(letters[1:3], LETTERS[1:3])
  )

  obj1 <- setup_heatmap(list(body_data = body1, row_data = rows1))

  expect_equal(
    object = obj1,
    expected = as_heatmap_data(
      list(body_matrix = body1_matrix, row_data = rows1)
    ),
    info = paste(
      "body/row-data should be unfiltered if all features are in both",
      "body and row-data"
    )
  )

  # row_data features and body_data features are identical but disordered
  body2 <- data.frame(
    feature_id = letters[3:1],
    sample_id = rep(LETTERS[1:3], each = 3),
    fitted_value = 1:9
  )

  rows2 <- data.frame(
    feature_id = c("b", "a", "c"),
    annotation = c(TRUE, FALSE, TRUE)
  )

  body2_matrix <- matrix(
    c(3, 2, 1, 6, 5, 4, 9, 8, 7),
    nrow = 3, dimnames = list(letters[1:3], LETTERS[1:3])
  )

  obj2 <- setup_heatmap(list(body_data = body2, row_data = rows2))

  expect_equal(
    object = obj2,
    expected = as_heatmap_data(
      list(
        body_matrix = body2_matrix,
        row_data = rows2[order(rows2$feature_id), ]
      )
    ),
    info = paste(
      "body/row-data should be unfiltered if all features are in both",
      "body and row-data (regardless of their order in the input)",
      "and the `feature_id`s should be in identical order."
    )
  )

  # row_data features are a subset of body_data features - body_data should be
  # filtered
  rows3 <- data.frame(
    feature_id = letters[1:2],
    annotation = c(TRUE, FALSE)
  )

  body3_matrix <- matrix(
    c(1:2, 4:5, 7:8),
    nrow = 2, dimnames = list(letters[1:2], LETTERS[1:3])
  )

  obj3 <- setup_heatmap(list(body_data = body1, row_data = rows3))

  expect_equal(
    object = obj3,
    expected = as_heatmap_data(
      list(body_matrix = body3_matrix, row_data = rows3)
    ),
    info = paste(
      "row-data has a subset of the body-data features: so body-data should",
      "be filtered down by `setup_heatmap`"
    )
  )

  # row_data features are a superset of body_data features - row_data should be
  # filtered
  body4 <- data.frame(
    feature_id = letters[2:3],
    sample_id = rep(LETTERS[1:3], each = 2),
    fitted_value = 1:6
  )

  body4_matrix <- matrix(
    1:6, nrow = 2, dimnames = list(letters[2:3], LETTERS[1:3])
  )

  obj4 <- setup_heatmap(list(body_data = body4, row_data = rows1))

  expect_equal(
    object = obj4,
    expected = as_heatmap_data(
      list(body_matrix = body4_matrix, row_data = rows1[-1, ])
    ),
    info = paste(
      "row-data has a superset of the body-data features: so row-data should",
      "be filtered down by `setup_heatmap`"
    )
  )

})
