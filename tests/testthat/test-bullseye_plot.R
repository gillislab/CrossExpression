test_that("bullseye_plot has correct visual properties", {
  # Use mock data
  scores <- c(0.8, 0.6, 0.4, 0.2)

  # Check return type
  p <- bullseye_plot(scores)
  expect_s3_class(p, "ggplot")

  # Check labels
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$fill, "")
  
  # Check color scale
  fill_scale <- p$scales$scales[[which(sapply(p$scales$scales, function(x) "fill" %in% x$aesthetics))]]
  expect_equal(fill_scale$palette(0), "#ADD8E6")
  expect_equal(fill_scale$palette(1), "#08306B")
  
  # Check that polygon count matches input length
  expect_equal(length(unique(p$data$window)), length(scores))
})