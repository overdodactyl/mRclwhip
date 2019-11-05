test_that("All Identical", {
  expect_true(all_identical(1, 1, 1))
  expect_false(all_identical(1, 1, 1, "1"))
  expect_false(all_identical(1, 1, 2))
  expect_false(all_identical(1, 1, NA))
})

test_that("Identity tests fail if less than 2 arguments", {

  output <- capture_output(
    expect_warning( # Gets a warning: "generated a condition with class packageNotFoundError/error/condition. It is less fragile to test custom conditions with `class`"
      expect_error(all_identical(1), "Input length")
    )
  )
})


test_that("Any Identical", {
  expect_true(any_identical(1, 1, 1))
  expect_false(any_identical(1, "1", 2))
  expect_true(any_identical(1, 1, 2))
  expect_true(any_identical(1, 1, NA))
  expect_false(any_identical(1, 2, 3, 4))
  expect_true(any_identical(NA, NA, 1))
})
