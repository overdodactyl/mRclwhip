test_that("Expected Results", {
  expect_equal(str_to_firstup("age at death"), "Age at death")
  expect_equal(str_to_firstup("baseline BMI"), "Baseline BMI")
})

test_that("Multiple inputs", {
  expect_identical(c("age at death", "baseline BMI") %>% str_to_firstup(), c("Age at death", "Baseline BMI"))
})
