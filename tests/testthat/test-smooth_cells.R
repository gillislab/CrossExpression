test_that("smooth_cells works with synthetic data", {
  # Create a small synthetic dataset
  set.seed(42)
  n_cells <- 50
  n_genes <- 5
  data <- matrix(runif(n_cells * n_genes), nrow = n_cells, ncol = n_genes)
  colnames(data) <- paste0("gene", 1:n_genes)
  rownames(data) <- paste0("cell", 1:n_cells)
  locations <- matrix(rnorm(n_cells * 2), nrow = n_cells, ncol = 2)
  
  # Test with correlation computation (default parameters)
  result_with_corr <- smooth_cells(data, locations, neighbors_smoothed = 1, corr = TRUE)
  
  # Test output structure with correlation
  expect_type(result_with_corr, "list")
  expect_equal(length(result_with_corr), 2)
  expect_equal(names(result_with_corr), c("smooth_expression", "smooth_correlation"))
  
  # Check dimensions of smoothed expression matrix
  expect_equal(dim(result_with_corr$smooth_expression), dim(data))
  
  # Check dimensions of correlation matrix
  expect_equal(dim(result_with_corr$smooth_correlation), c(n_genes, n_genes))
  
  # Check correlation matrix properties
  expect_true(all(result_with_corr$smooth_correlation >= -1 & result_with_corr$smooth_correlation <= 1))
  expect_true(all(diag(result_with_corr$smooth_correlation) <= 1 + 1e-10)) # Allow floating point error
}) 