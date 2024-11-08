test_that("cross_expression returns correct data frame output", {
  # Create a small synthetic dataset
  set.seed(42)
  n_cells <- 50
  n_genes <- 5
  data <- matrix(runif(n_cells * n_genes), nrow = n_cells, ncol = n_genes)
  colnames(data) <- paste0("gene", 1:n_genes)
  rownames(data) <- paste0("cell", 1:n_cells)
  locations <- matrix(rnorm(n_cells * 2), nrow = n_cells, ncol = 2)
  
  # Run cross_expression function
  result <- cross_expression(data, locations, neighbor = 1, alpha_cross = 0.05, alpha_co = 0)
  
  # Test output structure
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 8)
  expect_equal(nrow(result), choose(n_genes, 2))
  
  # Check column names
  expected_cols <- c("row", "col", "gene1", "gene2", "co_pvalue", "cross_pvalue", "cross_padj", "cross_sig")
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Check data types
  expect_type(result$co_pvalue, "double")
  expect_type(result$cross_pvalue, "double")
  expect_type(result$cross_padj, "double")
  expect_type(result$cross_sig, "integer")
})

test_that("cross_expression returns correct matrix output", {
  # Create a small synthetic dataset
  set.seed(42)
  n_cells <- 50
  n_genes <- 5
  data <- matrix(runif(n_cells * n_genes), nrow = n_cells, ncol = n_genes)
  colnames(data) <- paste0("gene", 1:n_genes)
  rownames(data) <- paste0("cell", 1:n_cells)
  locations <- matrix(rnorm(n_cells * 2), nrow = n_cells, ncol = 2)
  
  # Test with output_matrix = TRUE
  result_matrix <- cross_expression(data, locations, neighbor = 1, output_matrix = TRUE)
  
  # Check output structure for matrix output
  expect_type(result_matrix, "list")
  expect_equal(length(result_matrix), 3)
  expect_equal(names(result_matrix), c("Cross_without_FDR", "Cross_with_FDR", "Co_with_FDR"))
  
  # Check matrix dimensions
  expect_equal(dim(result_matrix[[1]]), c(n_genes, n_genes))
  expect_equal(dim(result_matrix[[2]]), c(n_genes, n_genes))
  expect_equal(dim(result_matrix[[3]]), c(n_genes, n_genes))
})
