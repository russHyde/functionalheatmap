###############################################################################

context("Tests for `format_heatmap` in `functionalheatmap`")

###############################################################################

test_that("`format_heatmap` works on invalid input", {
  expect_error(
    format_heatmap(),
    info = "no input error to format_heatmap"
  )

  expect_error(
    format_heatmap("Not a `heatmap_data` object"),
    info = "wrong-input-type error in `format_heatmap`"
  )
})


test_that("`format_heatmap` works on valid input", {
  hd1 <- as_heatmap_data(list(body_matrix = matrix()))

  expect_is(
    format_heatmap(hd1),
    "heatmap_data",
    info = "output from `format_heatmap` should be a `heatmap_data` object"
  )

  expect_equal(
    format_heatmap(hd1)$body_matrix,
    matrix(),
    info = "Data should be unmodified by `format_heatmap`"
  )
})
