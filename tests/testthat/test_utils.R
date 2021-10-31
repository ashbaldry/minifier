testthat::context("Utility Functions")

testthat::test_that("getSearchFileExtension provides the correct extension for each minifier", {
  testthat::expect_equal(getSearchFileExtension("js"), "*.js$")
  testthat::expect_equal(getSearchFileExtension("css"), "*.css$")
  testthat::expect_equal(getSearchFileExtension("html"), "*.html$")
})
