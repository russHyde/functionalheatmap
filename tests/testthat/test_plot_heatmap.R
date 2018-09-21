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

test_that("plot_heatmap: valid input", {
  # Can't really test this function properly as it plots to the graphics device

  hd1 <- as_heatmap_data(list(body_matrix = matrix(1:4, nrow = 2)))

  expect_is(
    plot_heatmap(hd1),
    "Heatmap",
    info = "output from plot_heatmap should be a Heatmap object"
  )
})

###############################################################################
