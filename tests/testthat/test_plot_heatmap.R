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

  hd1 <- as_heatmap_data(list(
    body_matrix = matrix(1:4, nrow = 2),
    row_data = data.frame(sample_id = 1:2, annot = c(TRUE, FALSE))
  ))

  expect_is(
    plot_heatmap(hd1),
    "Heatmap",
    info = paste(
      "output from plot_heatmap should be a Heatmap object if no annotations",
      "are present"
    )
  )

  hd2 <- annotate_heatmap(hd1, row_annotations = "annot")
  expect_is(
    plot_heatmap(hd2),
    "HeatmapList",
    info = paste(
      "output from plot_heatmap should be a HeatmapList when annotations are",
      "present"
    )
  )
})

###############################################################################
