###############################################################################

context("Data validity tests for `functionalheatmap`")

###############################################################################

test_that(".is_nonempty_list_of_data_frames", {
  expect_true(
    .is_nonempty_list_of_data_frames(list(a = data.frame(), b = data.frame()))
  )
})
