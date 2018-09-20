###############################################################################

context("Tests for `setup_heatmap`")

###############################################################################

test_that("setup_heatmap: invalid input", {
  expect_error(
    setup_heatmap(),
    info = "setup_heatmap without arguments should throw an error"
  )

  expect_error(
    setup_heatmap(x = list(bodydata = matrix(), 1:10, TRUE)),
    info = "setup_heatmap should receive a list of data-frames"
  )

  expect_error(
    setup_heatmap(x = list(a = data.frame())),
    info = "`bodydata` should be present"
  )
})

test_that("setup_heatmap: valid input", {
  df1 <- data.frame(
    feature_id = letters[1:3],
    sample_id = rep(LETTERS[1:3], each = 3),
    fitted_value = rnorm(9)
  )

  expect_equal(
    setup_heatmap(list(bodydata = df1)),
    list(bodydata = df1),
    info = "If only the bodydata is provided, output data should match input"
  )
})

###############################################################################
