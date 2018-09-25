###############################################################################

context("Tests for `annotate_heatmap`")

###############################################################################

test_that("`annotate_heatmap` fails with invalid input", {
  expect_error(
    annotate_heatmap(),
    info = "`x` must be defined in `annotate_heatmap`"
  )

  expect_error(
    annotate_heatmap("Not a heatmap_data object"),
    info = "annotate_heatmap expects a heatmap_data object"
  )
})

###############################################################################

test_that("row_annotation data-frame can be appended to heatmap_data", {
  hmd1 <- as_heatmap_data(
    list(
      body_matrix = matrix(
        1:12,
        nrow = 4, dimnames = list(letters[1:4], LETTERS[1:3])
      ),
      row_data = data.frame(
        sample_id = letters[1:4],
        foo = c(TRUE, FALSE, FALSE, TRUE)
      )
    )
  )

  expect_equal(
    object = annotate_heatmap(hmd1),
    expected = hmd1,
    info = paste(
      "unless annotations are specified, annotate_heatmap just returns the",
      "input"
    )
  )

  expect_error(
    object = annotate_heatmap(hmd1, row_annotations = "not a row_data track"),
    info = "If row_annotations are specified, they must be columns of row_data"
  )

  obj <- annotate_heatmap(hmd1, row_annotations = "foo")

  expect_is(
    obj,
    class = "heatmap_data",
    info = paste(
      "after row-annotations are added, the output should still be a",
      "`heatmap_data`` object"
    )
  )
  expect_equal(
    object = obj$row_annotation,
    expected = data.frame(foo = c(TRUE, FALSE, FALSE, TRUE)),
    info = "single column from row-data used as row_annotation data-frame"
  )
})

###############################################################################

test_that(
  paste(
    "if defined, row_annotations and contents of row_dots are added to a",
    "plotted heatmap"
  ), {
    hd1 <- as_heatmap_data(list(
      body_matrix = matrix(1:12, nrow = 4),
      row_data = data.frame(
        foo = c(FALSE, TRUE),
        bar = 1:4
      )
    ))

    m <- mockery::mock(1)
    testthat::with_mock(
      HeatmapAnnotation = m, {
        plot_heatmap(
          annotate_heatmap(
            hd1,
            row_annotations = "foo", row_dots = list(na_col = "red")
          )
        )
      },
      .env = "ComplexHeatmap"
    )
    annotation_args <- mockery::mock_args(m)[[1]]

    expect_equal(
      annotation_args[[1]],
      hd1$row_data[, "foo", drop = FALSE],
      info = paste(
        "data-frame corresponsing to the cols in the `annotate_heatmap`",
        "`row_annotations` column(s) is passed to HeatmapAnnotation"
      )
    )
    expect_true(
      "na_col" %in% names(annotation_args) && annotation_args$na_col == "red",
      info = paste(
        "additional args for formatting row-annotations in a heatmap",
        "(row_dots) are passed through to HeatmapAnnotation()"
      )
    )
  }
)

###############################################################################
