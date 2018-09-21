###############################################################################

context("Tests for data-manipulation in `functionalheatmap`")

###############################################################################

test_that("as_matrix", {
  df1 <- data.frame(a = 1:3)
  m1 <- matrix(1:3, nrow = 3, dimnames = list(NULL, c("a")))
  expect_equal(
    as_matrix(df1),
    m1,
    info = "no row-index in the input data-frame"
  )

  df2 <- data.frame(a = 1:3, b = letters[1:3])
  m2 <- matrix(1:3, nrow = 3, dimnames = list(letters[1:3], "a"))
  expect_equal(
    as_matrix(df2, rowname_col = "b"),
    m2,
    info = "df -> matrix using second column as rownames"
  )

  df3 <- data.frame(a = letters[1:3], b = 1:3)
  m3 <- matrix(1:3, nrow = 3, dimnames = list(letters[1:3], "b"))
  expect_equal(
    as_matrix(df3, rowname_col = "a"),
    m3,
    info = "df -> matrix using first column as rownames"
  )
})

###############################################################################

test_that("as_heatmap_data", {
  my_list <- list()

  expect_equal(
    class(as_heatmap_data(my_list)),
    "heatmap_data"
  )
})
