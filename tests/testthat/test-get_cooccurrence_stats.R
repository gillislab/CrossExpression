test_that("get_cooccurrence_stats works correctly", {
  mat1 <- matrix(c(1,0,1,0), nrow=2)
  mat2 <- matrix(c(0,1,0,1), nrow=2)
  
  # Basic functionality test
  result <- get_cooccurrence_stats(mat1, mat2)
  expect_type(result, "list")
  expect_equal(names(result), c("cooccur", "cells"))
  expect_equal(dim(result$cooccur), c(2,2))
  expect_equal(dim(result$cells), c(2,2))
  
  # Test with sparse=FALSE
  result_dense <- get_cooccurrence_stats(mat1, mat2, sparse=FALSE)
  expect_type(result_dense, "list")
  
  # Test with binarize=FALSE
  mat3 <- matrix(c(1,2,3,4), nrow=2)
  mat4 <- matrix(c(5,6,7,8), nrow=2)
  result_nonbinary <- get_cooccurrence_stats(mat3, mat4, binarize=FALSE)
  expect_type(result_nonbinary, "list")
  
  # Test with incompatible dimensions
  mat5 <- matrix(1:6, nrow=3)
  expect_error(get_cooccurrence_stats(mat1, mat5))
  
  # Test with empty matrices
  expect_error(get_cooccurrence_stats(matrix(nrow=0, ncol=0), matrix(nrow=0, ncol=0)))
}) 