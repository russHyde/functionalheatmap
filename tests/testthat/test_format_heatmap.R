###############################################################################

context("Tests for `format_heatmap` in `functionalheatmap`")

###############################################################################

test_that("format_heatmap does not work on invalid input", {
  expect_error(
    format_heatmap(),
    info = "no input error to format_heatmap"
  )

  expect_error(
    format_heatmap("Not a `heatmap_data` object"),
    info = "wrong-input-type error in `format_heatmap`"
  )
})

###############################################################################

test_that("`format_heatmap` works on valid input", {
  m1 <- matrix(rnorm(12), nrow = 4)
  hd1 <- as_heatmap_data(list(body_matrix = m1))

  expect_is(
    format_heatmap(hd1),
    "heatmap_data",
    info = "output from `format_heatmap` should be a `heatmap_data` object"
  )

  expect_equal(
    format_heatmap(hd1)$body_matrix,
    m1,
    info = "Data should be unmodified by format_heatmap"
  )
})

###############################################################################

test_that("`format_heatmap` modifies formatting args for `Heatmap()`", {
  hd1 <- as_heatmap_data(list(body_matrix = matrix()))

  expect_equivalent(
    format_heatmap(hd1)$formats,
    list(cluster_columns = FALSE, show_row_names = FALSE),
    info = "`format_heatmap` defines sensible defaults for omics"
  )

  expect_equivalent(
    format_heatmap(hd1, show_row_names = TRUE, cluster_columns = TRUE)$formats,
    list(cluster_columns = TRUE, show_row_names = TRUE),
    info = "`format_heatmap` lets user define formatting flags for Heatmap()"
  )
})

###############################################################################

test_that("args set by format_heatmap pass through to a Heatmap() call", {
  hd1 <- as_heatmap_data(list(body_matrix = matrix(1:12, nrow = 4)))

  # - format_heatmap() should set show_row_names to FALSE by default (when
  # Heatmap() is subsequently called by plot_heatmap())
  m <- mockery::mock(1)
  testthat::with_mock(
    Heatmap = m, {
      plot_heatmap(format_heatmap(hd1))
    },
    .env = "ComplexHeatmap"
  )
  hm_args <- mockery::mock_args(m)[[1]]
  expect_true(
    "show_row_names" %in% names(hm_args) && !hm_args$show_row_names,
    info = paste(
      "default formatting arguments should be passed to Heatmap() when",
      "set in format_heatmap()"
    )
  )

  # set show_row_names = TRUE and check that it passes through to Heatmap()
  m <- mockery::mock(1)
  testthat::with_mock(
    Heatmap = m, {
      plot_heatmap(format_heatmap(hd1, show_row_names = TRUE))
    },
    .env = "ComplexHeatmap"
  )
  hm_args <- mockery::mock_args(m)[[1]]
  expect_true(
    "show_row_names" %in% names(hm_args) && hm_args$show_row_names,
    info = paste(
      "non-default formatting arguments should be passed to Heatmap() when",
      "set in format_heatmap()"
    )
  )
})

###############################################################################

.setup_4x3_heatmap_data <- function() {
  as_heatmap_data(
    list(
      body_matrix = matrix(
        1:12,
        nrow = 4, dimnames = list(letters[1:4], LETTERS[1:3])
      ),
      row_data = data.frame(
        feature_id = letters[1:4], my_split = rep(1:2, each = 2)
      )
    )
  )
}

###############################################################################

test_that("`split` can be defined using columns of `row_data`", {
  hd1 <- .setup_4x3_heatmap_data()

  expect_equal(
    object = format_heatmap(hd1, split = "my_split")$formats$split,
    expected = data.frame(
      my_split = rep(1:2, each = 2)
    ),
    info = "`split` defined by a single column of `row_data`"
  )

  expect_error(
    object = format_heatmap(hd1, split = "not a column"),
    info = "`split` columns should be present in the `row_data` data-frame"
  )
})

###############################################################################

test_that("format_heatmap can be used multiple times in one pipeline", {
  hd1 <- .setup_4x3_heatmap_data()

  f1 <- format_heatmap(hd1, na_col = "black", row_title = "my-rows")

  f2 <- format_heatmap(f1, na_col = "purple", column_title = "my_columns")

  expect_true(
    object = (
      !"column_title" %in% names(f1$formats) &&
        f2$formats$column_title == "my_columns"
    ),
    info = "adding a new formatting flag in a second format_heatmap call"
  )

  expect_equal(
    object = f2$formats$row_title,
    expected = f2$formats$row_title,
    info = "format_heatmap should not overwrite unstated formatting flags"
  )

  expect_true(
    object = f1$formats$na_col != f2$formats$na_col,
    info = "format_heatmap can overwrite a named formatting flag"
  )
})

###############################################################################
