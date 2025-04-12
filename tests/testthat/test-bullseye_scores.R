# Create a helper function to generate test data
create_test_data <- function() {
  set.seed(42)
  n_cells <- 50
  n_genes <- 5
  data <- matrix(rbinom(n_cells * n_genes, 1, 0.3), nrow = n_cells, ncol = n_genes)
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

test_that("bullseye_scores returns correct structure with default parameters", {
  test_data <- create_test_data()
  result_default <- bullseye_scores(test_data$data, test_data$locations)
  
  # Test output structure
  expect_s3_class(result_default, "data.frame")
  
  # Check number of rows
  expected_rows <- choose(test_data$n_genes, 2) * 2
  expect_equal(nrow(result_default), expected_rows)
  
  # Check columns
  expect_equal(ncol(result_default), 13)
  expect_true(all(c("gene1", "gene2", "Cell") %in% colnames(result_default)))
})

test_that("bullseye_scores works with custom window sizes", {
  test_data <- create_test_data()
  custom_windows <- c(1, 7)
  
  result_custom <- bullseye_scores(test_data$data, test_data$locations, window_sizes = custom_windows)
  
  # Check columns for custom window sizes
  expect_equal(ncol(result_custom), 5)
  expect_true(all(paste0("Neig = ", custom_windows) %in% colnames(result_custom)))
})

test_that("bullseye_scores correctly returns matrix output", {
  test_data <- create_test_data()
  
  # Test with output_matrix = TRUE
  result_matrix <- bullseye_scores(test_data$data, test_data$locations, output_matrix = TRUE)
  
  # Check output structure with matrices
  expect_type(result_matrix, "list")
  
  # Check matrix dimensions
  for (i in 1:length(result_matrix)) {
    expect_equal(dim(result_matrix[[i]]), c(test_data$n_genes, test_data$n_genes))
    expect_true(all(rownames(result_matrix[[i]]) == colnames(test_data$data)))
    expect_true(all(colnames(result_matrix[[i]]) == colnames(test_data$data)))
  }
}) 