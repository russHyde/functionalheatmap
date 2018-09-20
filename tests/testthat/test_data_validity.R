###############################################################################

context("Data validity tests for `functionalheatmap`")

###############################################################################

test_that(".is_nonempty_list_of_data_frames", {
  expect_true(
    .is_nonempty_list_of_data_frames(list(a = data.frame(), b = data.frame()))
  )
  expect_false(
    .is_nonempty_list_of_data_frames(list(matrix()))
  )
  expect_false(
    .is_nonempty_list_of_data_frames(1)
  )
  expect_false(
    .is_nonempty_list_of_data_frames(list())
  )
  expect_false(
    .is_nonempty_list_of_data_frames(list(data.frame(), matrix()))
  )
})
