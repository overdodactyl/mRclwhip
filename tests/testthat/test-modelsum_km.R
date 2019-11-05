test_that("no errors", {
  df <- arsenal::mockstudy
  df_selected <- df %>%
    dplyr::select(fu.time, fu.stat, mdquality.s, sex) %>%
    dplyr::mutate_at(dplyr::vars(mdquality.s), as.factor) %>%
    dplyr::mutate(fu.time = fu.time / 365)

  Hmisc::label(df_selected$sex) <- "Sex"
  Hmisc::label(df_selected$mdquality.s) <- "Quality"
  expect_error(modelsum_km(event = fu.stat, time = fu.time, times = 1:3, data = df_selected, freedom.from = "death"), NA)
})
