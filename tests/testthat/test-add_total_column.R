test_that("column summing works", {
  df <- data.frame(id = LETTERS[1:3], x1 = 1:3, x2 = 4:6, x3 = 7:9)
  df <- add_total_column(df)

  expect_identical(df$Total, c(12, 15, 18))
})

test_that("column renaming works", {
  df <- data.frame(id = LETTERS[1:3], x1 = 1:3, x2 = 4:6, x3 = 7:9)
  df <- add_total_column(df, name = "New Column")

  expect_identical(df$`New Column`, c(12, 15, 18))
})
