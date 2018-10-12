###############################################################################

context("Ensure ComplexHeatmap objects can be made from the heatmap_data")

###############################################################################

get_hmd1 <- function() {
  as_heatmap_data(
    list(
      body_matrix = matrix(
        1:12,
        nrow = 4, dimnames = list(letters[1:4], LETTERS[1:3])
      ),
      row_data = data.frame(
        feature_id = letters[1:4],
        foo = c(TRUE, FALSE, FALSE, TRUE),
        bar = 1:4
      ),
      column_data = data.frame(
        sample_id = LETTERS[1:3],
        zig = c(TRUE, FALSE, FALSE),
        zag = c(20, 10, 50)
      )
    )
  )
}

###############################################################################

test_that("HeatmapAnnotation object can be made for the row-annotations", {
  hmd1 <- get_hmd1()
  hmd_with_foo <- annotate_heatmap(hmd1, row_annotations = "foo")

  expect_is(
    object = .get_row_annotation_object(hmd_with_foo),
    "HeatmapAnnotation"
  )
})

###############################################################################

test_that("HeatmapAnnotation object can be made for the top-annotations", {
  hmd1 <- get_hmd1()
  hmd_with_zig <- annotate_heatmap(hmd1, top_annotations = "zig")

  expect_is(
    object = .get_top_annotation_object(hmd_with_zig),
    "HeatmapAnnotation"
  )
})

###############################################################################
