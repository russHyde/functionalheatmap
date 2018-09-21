###############################################################################

context("Tests for classes in `functionalheatmap`")

###############################################################################

test_that("as_heatmap_data", {
  my_list <- list()

  expect_equal(
    class(as_heatmap_data(my_list)),
    "heatmap_data"
  )
})

###############################################################################
