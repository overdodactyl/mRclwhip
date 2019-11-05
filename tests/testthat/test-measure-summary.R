test_that("n range", {
  df <- tibble::tribble(
    ~Group, ~n, ~low, ~high,
    "a", 1.323453, 1.154, 1.44,
    "b", 1000.801, 987.741, 1010.9891
  )

  expect_identical(nrange(df, n, low, high, accuracy = .1)$Estimate[2], "1,000.8 (987.7, 1,011.0)")
  expect_identical(nrange(df, n, low, high, accuracy = .1) %>% names(), c("Group", "Estimate"))
  expect_identical(nrange(df, n, low, high, name = "Result") %>% names(), c("Group", "Result"))
  expect_identical(
    nrange(df, n, low, high, remove = F) %>% names(),
    c("Group", "n", "low", "high", "Estimate")
  )
})
