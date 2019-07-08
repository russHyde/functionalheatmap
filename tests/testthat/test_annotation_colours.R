###############################################################################

context("tests for making annotation colour schemes")

###############################################################################

.default_false <- "grey"
.default_true <- "black"

test_that("make a named list of colours for use in row / column annotations", {
  expect_equal(
    object = define_annotation_colours(),
    expected = list(),
    info = "Empty annotation colours"
  )

  for (true_col in c("black", "red")){
    expect_equal(
      object = define_annotation_colours(
        boolean_var_names = "some_var",
        true_col = true_col,
        false_col = "blue"
      ),
      expected = list(
        some_var = c("FALSE" = "blue", "TRUE" = true_col)
      ),
      info = "Set TRUE colour for a boolean var"
    )
  }

  for (false_col in c("black", "red")){
    expect_equal(
      object = define_annotation_colours(
        boolean_var_names = "some_var",
        true_col = "blue",
        false_col = false_col
      ),
      expected = list(
        some_var = c("FALSE" = false_col, "TRUE" = "blue")
      ),
      info = "Set FALSE colour for a boolean var"
    )
  }

  expect_equal(
    define_annotation_colours(
      boolean_var_names = "some_var"
    ),
    list(
      some_var = c("FALSE" = .default_false, "TRUE" = .default_true)
    ),
    info = "default values exist for boolean colours"
  )

  .custom_colours <- list(
    some_var = c("FALSE" = "yellow", "TRUE" = "green"),
    some_other_var = function(x) {
      "red"
    },
    some_discrete_var = c(
      "[0]" = "#fde0dd", "[1, 3]" = "#fa9fb5", "[4, 7]" = "purple"
    )
  )
  expect_equal(
    # only custom colours were specified (this is a trivial case)
    define_annotation_colours(
      custom_colour_list = .custom_colours
    ),
    expected = .custom_colours,
    info = "custom_colour_list should be unmodified"
  )

  expect_equal(
    define_annotation_colours(
      boolean_var_names = c("abc"),
      custom_colour_list = .custom_colours
    ),
    expected = append(
      .custom_colours,
      list(abc = c("FALSE" = .default_false, "TRUE" = .default_true))
    ),
    info = "combine list of custom colours with newly-defined boolean colours"
  )

  # the annotation variables defined in boolean_var_names and custom_colour_list
  # should not overlap
  expect_error(
    define_annotation_colours(
      boolean_var_names = "some_var", # present in .custom_colours
      custom_colour_list = .custom_colours
    ),
    info = paste(
      "there should be no overlap between varnames in boolean_var_names and",
      "custom_colour_list"
    )
  )

  expect_error(
    define_annotation_colours(
      boolean_var_names = c("the_same_var", "the_same_var")
    ),
    info = "variable names should not be duplicated in `boolean_var_names`"
  )

  .duplicated_colour_list <- list(
    some_var = "red", some_var = "green"
  )
  expect_error(
    define_annotation_colours(
      custom_colour_list = .duplicated_colour_list
    ),
    info = "variable names should not be duplicated in `custom_colour_list`"
  )
})

###############################################################################

test_that("make a named vector for colouring booleans", {
  # boolean_colour returns a named vector c("FALSE" = "some_colour", "TRUE" =
  # "some_colour")

  expect_equal(
    object = boolean_colour(true_col = "black", false_col = "white"),
    expected = c("FALSE" = "white", "TRUE" = "black"),
    info = "user can set true and false colour"
  )

  expect_equal(
    object = boolean_colour(true_col = "black"),
    expected = c("FALSE" = .default_false, "TRUE" = "black"),
    info = "FALSE colour has a default value"
  )

  expect_equal(
    object = boolean_colour(true_col = "red", false_col = "blue"),
    expected = c("FALSE" = "blue", "TRUE" = "red"),
    info = "alternative change to both colours"
  )

  expect_error(
    object = boolean_colour(false_col = "red"),
    info = "true_col should be provided"
  )
})
