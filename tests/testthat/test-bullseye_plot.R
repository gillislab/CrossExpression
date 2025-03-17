test_that("bullseye_plot generates correct plot", {
  # Test with all numerical input
  scores <- c(0.5, 0.3, 0.2, 0.1)
  p <- bullseye_plot(scores)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$fill, "")
  
  # Test with empty vector
  p <- bullseye_plot(numeric(0))
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$fill, "")
  
  # Test with NA values
  scores_na <- c(0.5, NA, 0.2, 0.1)
  p <- bullseye_plot(scores_na)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$y, "")
  expect_equal(p$labels$fill, "")
}) 