test_that("Check if columns are moved", {
  first <- move_to_first(iris, Species)
  last <- move_to_last(iris, Sepal.Length)

  expect_equal(names(first)[1], "Species")
  expect_equal(names(last)[5], "Sepal.Length")
})
