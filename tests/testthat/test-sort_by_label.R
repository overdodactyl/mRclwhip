test_that("Re-order variables", {
  df <- data.frame(gender = c("M", "F"), study_age = c(56, 74), "bmi" = c(60, 40))
  Hmisc::label(df$gender) <- "Sex"
  Hmisc::label(df$study_age) <- "Age at Study"
  Hmisc::label(df$bmi) <- "BMI"

  expect_identical(df %>% sort_by_label() %>% names(), c("study_age", "bmi", "gender"))
})
