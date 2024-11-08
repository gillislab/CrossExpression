test_that("tissue_expression_plot works correctly", {
  # Create test data
  data <- matrix(sample(0:1, 100, replace=TRUE), nrow=50, ncol=2)
  colnames(data) <- c("gene1", "gene2")
  locations <- data.frame(
    x = rnorm(50),
    y = rnorm(50)
  )

  # tissue_expression_plot is created when cross_expression=TRUE
  p <- tissue_expression_plot(data, locations, "gene1", "gene2", point_size=1)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "x coordinates")
  expect_equal(p$labels$y, "y coordinates")
  legend_data <- ggplot2::ggplot_build(p)$plot$scales$scales
  expect_equal(length(legend_data), 3)
  
  # tissue_expression_plot is created when cross_expression=FALSE
  p2 <- tissue_expression_plot(data, locations, "gene1", "gene2", cross_expression=FALSE, point_size=1)
  expect_s3_class(p2, "ggplot")
  expect_equal(p2$labels$x, "x coordinates")
  expect_equal(p2$labels$y, "y coordinates")
  legend_data2 <- ggplot2::ggplot_build(p2)$plot$scales$scales
  expect_equal(length(legend_data2), 3)
})