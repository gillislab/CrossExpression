test_that("correlation function works correctly", {
  mat1 <- matrix(c(1,2,3,4), nrow=2)
  mat2 <- matrix(c(2,4,6,8), nrow=2)
  
  # Test single matrix correlation
  result1 <- correlation(mat1)
  expect_equal(dim(result1), c(2,2))
  expect_equal(diag(result1), c(1,1))
  
  # Test two matrix correlation
  result2 <- correlation(mat1, mat2)
  expect_equal(dim(result2), c(2,2))
  expect_equal(result2[1,1], 1)
}) 