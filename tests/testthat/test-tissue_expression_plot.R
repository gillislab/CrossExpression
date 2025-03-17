test_that("tissue_expression_plot works correctly", {
  # Create test data
  data <- matrix(sample(0:1, 100, replace=TRUE), nrow=50, ncol=2)
  colnames(data) <- c("gene1", "gene2")
  
  # Create test locations as a data frame instead of matrix
  locations <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )
  
  # Basic functionality test
  p <- tissue_expression_plot(data, locations, "gene1", "gene2", point_size=1)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "x coordinates")
  expect_equal(p$labels$y, "y coordinates")

  # Test with cross_expression=TRUE
  p2 <- tissue_expression_plot(data, locations, "gene1", "gene2", 
                             cross_expression=TRUE, neighbor=1)
  expect_s3_class(p2, "ggplot")
  
  # Test with scale bar
  p3 <- tissue_expression_plot(data, locations, "gene1", "gene2", 
                             scale_bar=10)
  expect_s3_class(p3, "ggplot")
  
  # Test with invalid gene names
  expect_error(tissue_expression_plot(data, locations, "invalid1", "invalid2"))
  
  # Test with incompatible dimensions
  locations_bad <- matrix(c(0,0,1), nrow=3)
  expect_error(tissue_expression_plot(data, locations_bad, "gene1", "gene2"))
  
  # Test with empty matrices
  expect_error(tissue_expression_plot(matrix(nrow=0, ncol=0), 
                                    matrix(nrow=0, ncol=0), 
                                    "gene1", "gene2"))
}) 