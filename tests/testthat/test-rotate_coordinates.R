test_that("rotate_coordinates works correctly", {
  x <- c(1, 0)
  y <- c(0, 1)
  
  # Test 90 degree rotation
  result <- rotate_coordinates(x, y, n_degrees=90)
  expect_equal(dim(result), c(2,2))
  expect_equal(colnames(result), c("pos_x", "pos_y"))
  
  # Test with centering and scaling
  result2 <- rotate_coordinates(x, y, n_degrees=90, center=TRUE, scale=TRUE)
  expect_equal(dim(result2), c(2,2))
  
  # Test with flipping
  result3 <- rotate_coordinates(x, y, flip_x=TRUE)
  expect_equal(dim(result3), c(2,2))
  
  result4 <- rotate_coordinates(x, y, flip_y=TRUE)
  expect_equal(dim(result4), c(2,2))
}) 