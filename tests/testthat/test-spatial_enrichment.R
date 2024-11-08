test_that("spatial_enrichment works with synthetic data", {
  # Create a small synthetic dataset
  set.seed(42)
  n_cells <- 50
  n_genes <- 5
  
  # Create binary expression matrix (cells x genes)
  data <- matrix(rbinom(n_cells * n_genes, 1, 0.3), nrow = n_cells, ncol = n_genes)
  colnames(data) <- paste0("gene", 1:n_genes)
  rownames(data) <- paste0("cell", 1:n_cells)
  
  # Create 2D spatial coordinates for cells
  # Use a more structured pattern to ensure some spatial correlation
  x_coords <- rep(1:sqrt(n_cells), times = sqrt(n_cells)) + rnorm(n_cells, sd = 0.1)
  y_coords <- rep(1:sqrt(n_cells), each = sqrt(n_cells)) + rnorm(n_cells, sd = 0.1)
  locations <- cbind(x_coords, y_coords)
  
  # Test with default parameters
  result <- spatial_enrichment(data, locations, gene1 = "gene1", gene2 = "gene2")
  
  # Test output structure
  expect_type(result, "list")
  expect_equal(length(result), 4)
  expect_true(all(c("pvalue", "target", "null", "plot") %in% names(result)))
  
  # Check p-value properties
  expect_type(result$pvalue, "double")
  expect_true(result$pvalue >= 0 && result$pvalue <= 1)
  
  # Check distribution vectors
  expect_type(result$target, "double")
  expect_type(result$null, "double")
  expect_true(length(result$target) > 0)
  expect_true(length(result$null) > 0)
  
  # Check plot object
  expect_s3_class(result$plot, "ggplot")
  
  # Test with different neighbor parameter
  result_neighbor <- spatial_enrichment(data, locations, 
                                       gene1 = "gene1", 
                                       gene2 = "gene3", 
                                       neighbor = 2)
  
  expect_type(result_neighbor, "list")
  expect_equal(length(result_neighbor), 4)
  
  # Test with max_pairs parameter
  result_pairs <- spatial_enrichment(data, locations,
                                    gene1 = "gene2",
                                    gene2 = "gene4",
                                    max_pairs = 10)
  
  expect_type(result_pairs, "list")
  expect_equal(length(result_pairs), 4)
  
  # Test with different gene combinations
  all_combinations <- combn(colnames(data), 2)
  for (i in 1:min(2, ncol(all_combinations))) {  # Test first few combinations to save time
    gene1 <- all_combinations[1, i]
    gene2 <- all_combinations[2, i]
    
    result_combo <- spatial_enrichment(data, locations, gene1, gene2)
    
    # Basic structure checks
    expect_type(result_combo, "list")
    expect_equal(length(result_combo), 4)
    expect_true(result_combo$pvalue >= 0 && result_combo$pvalue <= 1)
  }
}) 