testthat::test_that('FigTraktplott returns a qic object and preserves counts', {
  result <- FigTraktplott()
  n <- c(100, 150, 200)
  y <- c(0.8, 0.7, 0.9)
  testthat::expect_s3_class(result, 'qic')
  testthat::expect_equal(result@data$n, n)
  testthat::expect_equal(result@data$y.mean, round(y * n))
})
