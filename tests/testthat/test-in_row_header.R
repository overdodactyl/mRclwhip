test_that("Correct Row Addition", {
  df <- tibble::tribble(
    ~Label, ~value,
    "A", 1.2,
    "A", 1.3,
    "B", 5.0,
    "C", 4.3,
    "C", 4.1,
    "C", 5.2,
    "D", 6
  )

  df <- df %>% in_row_headers(Label)

  expect_identical(df$Label, c("A", "A", "A", "B", "C", "C", "C", "C", "D"))
})
