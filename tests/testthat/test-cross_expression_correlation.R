# Create a helper function to generate test data
create_test_data <- function() {
  set.seed(42)
  n_cells <- 50
  n_genes <- 5
  data <- matrix(runif(n_cells * n_genes), nrow = n_cells, ncol = n_genes)
  colnames(data) <- paste0("gene", 1:n_genes)
  rownames(data) <- paste0("cell", 1:n_cells)
  locations <- matrix(rnorm(n_cells * 2), nrow = n_cells, ncol = 2)
  
  list(
    data = data,
    locations = locations,
    n_cells = n_cells,
    n_genes = n_genes
  )
}

test_that("cross_expression_correlation produces correct output", {
  test_data <- create_test_data()
  
  # Test with neighbor = 1
  result <- cross_expression_correlation(test_data$data, test_data$locations, 
                                        neighbor = 1)
  
  # Test output structure
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), choose(test_data$n_genes, 2))
  
  # Check column names
  expected_cols <- c("gene1", "gene2", "correlation")
  expect_equal(colnames(result), expected_cols)
  
  # Check data types
  expect_type(result$gene1, "character")
  expect_type(result$gene2, "character")
  expect_type(result$correlation, "double")
})

test_that("cross_expression_correlation produces correct matrix output", {
  test_data <- create_test_data()
  
  # Test with matrix output
  result_matrix <- cross_expression_correlation(test_data$data, test_data$locations, 
                                              neighbor = 1, output_matrix = TRUE)
  
  # Check output structure for matrix output
  expect_true(is.matrix(result_matrix))
  expect_equal(dim(result_matrix), c(test_data$n_genes, test_data$n_genes))
  
  # Check gene names are correctly set
  expect_equal(rownames(result_matrix), colnames(test_data$data))
  expect_equal(colnames(result_matrix), colnames(test_data$data))
})
