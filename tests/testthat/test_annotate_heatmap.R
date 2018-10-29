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

get_hmd1 <- function(tibble_column_data = FALSE) {
  df_func <- if (tibble_column_data) {
    tibble::tibble
  } else {
    data.frame
  }

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
      column_data = df_func(
        sample_id = LETTERS[1:3],
        zig = c(TRUE, FALSE, FALSE),
        zag = c(20, 10, 50)
      )
    )
  )
}

###############################################################################

test_that("annotate_heatmap pass-through", {
  hmd1 <- get_hmd1()

  expect_equal(
    object = annotate_heatmap(hmd1),
    expected = hmd1,
    info = paste(
      "Unless annotation tracks are specified ([row|top|bottom]_annotations),",
      "annotate_heatmap just returns the input"
    )
  )
})

###############################################################################

test_that("row_annotation data-frame can be appended to heatmap_data", {
  hmd1 <- get_hmd1()

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

test_that("top_annotation data-frame can be appended to heatmap_data", {
  hmd1 <- get_hmd1()
  hmd_no_column_data <- as_heatmap_data(
    get_hmd1()[c("body_matrix", "row_data")]
  )
  hmd_with_tibble_coldata <- as_heatmap_data(
    append(
      get_hmd1()[c("body_matrix", "row_data")],
      list(
        column_data = tibble::as_tibble(get_hmd1()$column_data)
      )
    )
  )

  expect_is(
    annotate_heatmap(hmd1, top_annotations = "zig"),
    class = "heatmap_data",
    info = paste(
      "After top-annotations are added, the output should still be a",
      "`heatmap_data` object"
    )
  )

  expect_error(
    annotate_heatmap(hmd1, top_annotations = "not_a_track_in_column_data"),
    info = paste(
      "Any tracks to be added to the top_annotations should be present in the",
      "column_data data-frame"
    )
  )

  expect_error(
    annotate_heatmap(hmd_no_column_data, top_annotations = "zig"),
    info = paste(
      "`top_annotations` can't be added to a heatmap if there is no",
      "`column_data` entry that defines the relevant data"
    )
  )

  expect_equal(
    object = annotate_heatmap(hmd1, top_annotations = "zig")$top_annotation,
    expected = hmd1$column_data["zig"],
    info = paste(
      "the top_annotation data should equal the corresponding",
      "sub-data-frame of column_data"
    )
  )

  expect_equal(
    object = annotate_heatmap(
      hmd_with_tibble_coldata, top_annotations = "zig"
      )$top_annotation,
    expected = get_hmd1()[["column_data"]]["zig"],
    info = "top_annotations can be added from a tibble"
  )
})

###############################################################################

test_that(
  paste(
    "if defined, row_annotations and contents of row_dots are added to a",
    "plotted heatmap"
  ), {
    hmd1 <- get_hmd1()

    m <- mockery::mock(1)
    testthat::with_mock(
      HeatmapAnnotation = m, {
        plot_heatmap(
          annotate_heatmap(
            hmd1,
            row_annotations = "foo", row_dots = list(na_col = "red")
          )
        )
      },
      .env = "ComplexHeatmap"
    )
    annotation_args <- mockery::mock_args(m)[[1]]

    expect_equal(
      annotation_args[[1]],
      hmd1$row_data[, "foo", drop = FALSE],
      info = paste(
        "data-frame corresponding to the cols in the `annotate_heatmap`",
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

test_that(
  paste(
    "a HeatmapAnnotation object is built from the above-the-heatmap",
    "annotation-data"
  ), {
    hmd1 <- get_hmd1()

    hmd_with_zig <- annotate_heatmap(
      hmd1,
      top_annotations = "zig",
      top_dots = list(show_legend = FALSE)
    )

    expect_is(
      .get_top_annotation_object(hmd_with_zig),
      "HeatmapAnnotation",
      "A HeatmapAnnoation can be built from a heatmap_data"
    )

    expect_is(
      plot_heatmap(hmd_with_zig),
      "Heatmap",
      info = "plotting a heatmap_data should return a Heatmap or HeatmapList"
    )

    # Values in the mock annotation object aren't used in the test
    ha <- HeatmapAnnotation(data.frame(zig = 1:3))
    m <- mockery::mock(ha)

    testthat::with_mock(
      HeatmapAnnotation = m,
      plot_heatmap(hmd_with_zig),
      .env = "ComplexHeatmap"
    )
    annotation_args <- mockery::mock_args(m)[[1]]

    expect_equal(
      annotation_args[[1]],
      hmd1$column_data[, "zig", drop = FALSE],
      info = paste(
        "data-frame corresponding to the tracks in the `annotate_heatmap`",
        "`top_annotations` argument is passed to HeatmapAnnotation"
      )
    )

    expect_true(
      "show_legend" %in% names(annotation_args)
      && !annotation_args$show_legend,
      info = paste(
        "additional args for formatting row-annotations in a heatmap",
        "(row_dots) are passed through to HeatmapAnnotation()"
      )
    )
  }
)

###############################################################################

test_that(
  paste(
    "Valid objects / plots can be made when tibble annotation data is
    provided"
  ),
  {
    annotated_hmd_from_tibble_column_data <- get_hmd1(
      tibble_column_data = TRUE
    ) %>%
    annotate_heatmap(
      top_annotations = "zig"
    )

    expect_is(
      .get_top_annotation_object(annotated_hmd_from_tibble_column_data),
      "HeatmapAnnotation",
      info = paste(
        "A HeatmapAnnotation can be built when tibble column data is provided"
      )
    )

    expect_is(
      plot_heatmap(annotated_hmd_from_tibble_column_data),
      "Heatmap",
      info = paste(
        "A Heatmap can be built when tibble `column_data` is provided"
      )
    )

    # The following  has to be ran in an interactive session since it uses the
    # graphics device.

    # A heatmap can be plotted even when annotations have been taken from a
    #  tibble (as column-data)
    # - there was originally a bug with 'tibble' use
    # - TODO: work out how to replace this test with a mocked-out graphics
    # device since the test (originally) failed after plotting had started
    skip_if_not(interactive())

    expect_silent(
      ComplexHeatmap::draw(
        plot_heatmap(annotated_hmd_from_tibble_column_data)
      )
    )
    dev.off()
})
