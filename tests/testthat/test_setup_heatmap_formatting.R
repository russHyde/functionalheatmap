###############################################################################

context("Tests for `setup_heatmap_formatting` in `functionalheatmap`")

###############################################################################

test_that("setup_heatmap_formatting: invalid input", {
  expect_error(
    setup_heatmap_formatting(),
    info = "no input error to setup_heatmap_formatting"
  )

  expect_error(
    setup_heatmap_formatting("Not a `heatmap_data` object"),
    info = "wrong-input-type error in `setup_heatmap_formatting`"
  )
})


test_that("setup_heatmap_formatting: valid input", {
  hd1 <- as_heatmap_data(list(body_matrix = matrix()))

  expect_equal(
    setup_heatmap_formatting(hd1),
    hd1,
    info = "Data should be unmodified if no other args are provided"
  )
})
