test_that("No errors produced from table formatting", {
  data <- move_to_first(iris, Species)

  header1 <- list(values = c("Length", "Width", "Length", "Width", "Species"))
  header2 <- list(values = c("", "Sepal", "Petal"), colwidths = c(1, 2, 2))

  expect_error(format_flextable(data, header1 = header1, header2 = header2), NA)
  expect_error(flextable::flextable(data) %>% format_flextable(), NA)
  expect_error(format_flextable(data) %>% add_footer("Test Footer"), NA)
})
