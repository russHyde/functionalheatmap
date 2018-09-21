###############################################################################

context("Tests for `plot_heatmap` in `functionalheatmap`")

###############################################################################

test_that("plot_heatmap: invalid input", {
  expect_error(
    plot_heatmap(),
    info = "no-input-error to `plot_heatmap`"
  )

  expect_error(
    plot_heatmap("Not a heatmap_data object"),
    info = "incorrect-input-type-error to `plot_heatmap`"
  )
})

###############################################################################
