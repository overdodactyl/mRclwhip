test_that("pct_change on dataframe columns", {
  df <- data.frame(id = LETTERS[1:4], x1 = c(1, 5, 1.2, 9), x2 = c(2, 5.2, 1, 10))
  df <- dplyr::mutate(df, pct_chng = pct_change(x1, x2) %>% round(2))

  expect_identical(df$pct_chng, c(1.00, 0.04, -0.17, 0.11))

})

test_that("pct_change on single values", {
  res <- pct_change(1, 1.1, perc = TRUE)

  expect_identical(res, "10%")
})
