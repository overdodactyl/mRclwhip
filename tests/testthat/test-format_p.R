test_that("numeric values", {
  expect_equal(format_p(.0000001), "<0.001")
  expect_equal(format_p(.0346), "0.035")
  expect_equal(format_p(.34567), "0.35")
})

test_that("p values close to 1", {
  expect_equal(suppressWarnings(format_p(.991)), "1.00")
  expect_equal(format_p(1), "1.00")
  expect_warning(format_p(.998), "rounded")
  # expect_equal(format_p(1), "0.35")
})

test_that("characters", {
  expect_equal(format_p("<0.001"), "<0.001")
  expect_equal(format_p("< 0.001"), "<0.001")
  output <- capture_output(
    expect_warning(
      expect_error(format_p("< 0.02342"), "Couldn't format")
    )
  )

})

test_that("no p-values", {
  expect_equal(format_p(""), "-")
  expect_equal(format_p(NA), "-")
  expect_equal(format_p("", empty_string = NA_character_), NA_character_)
  expect_equal(format_p(NA, na_string = NA_character_), NA_character_)
})
