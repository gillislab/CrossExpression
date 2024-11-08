test_that("scale_01 works correctly", {
  vec <- c(-1, 0, 1, 2)
  
  # Basic functionality test
  result <- scale_01(vec)
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_equal(length(result), length(vec))
  
  # Test with all same values
  same_vals <- c(1,1,1,1)
  result_same <- scale_01(same_vals)
  expect_true(all(is.na(result_same)))
  
  # Test with NA values
  vec_na <- c(-1, NA, 1, 2)
  result_na <- scale_01(vec_na)
  expect_true(any(is.na(result_na)))
  
  # Test with matrix input
  mat <- matrix(1:4, nrow=2)
  result_mat <- scale_01(mat)
  expect_equal(length(result_mat), 4)
}) 