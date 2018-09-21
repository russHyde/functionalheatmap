###############################################################################

context("Tests for `format_heatmap` in `functionalheatmap`")

###############################################################################

test_that("format_heatmap: invalid input", {
  expect_error(
    format_heatmap(),
    info = "no input error to format_heatmap"
  )

  expect_error(
    format_heatmap("Not a `heatmap_data` object"),
    info = "wrong-input-type error in `format_heatmap`"
  )
})


test_that("format_heatmap: valid input", {
  hd1 <- as_heatmap_data(list(body_matrix = matrix()))

  expect_equal(
    format_heatmap(hd1),
    hd1,
    info = "Data should be unmodified if no other args are provided"
  )
})
