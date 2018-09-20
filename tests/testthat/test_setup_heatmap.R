###############################################################################

context("Tests for `setup_heatmap`")

###############################################################################

test_that("setup_heatmap: invalid input", {
  expect_error(
    setup_heatmap(),
    info = "setup_heatmap without arguments should throw an error"
  )
})

###############################################################################
