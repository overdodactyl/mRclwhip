test_that("no errors", {
  df <- arsenal::mockstudy
  df_selected <- df %>%
    dplyr::select(fu.time, fu.stat, mdquality.s, ps, sex) %>%
    dplyr::mutate_at(dplyr::vars(mdquality.s, ps), as.factor)

  Hmisc::label(df_selected$sex) <- "Sex"
  Hmisc::label(df_selected$mdquality.s) <- "Quality"
  Hmisc::label(df_selected$ps) <- "PS"

  expect_error(modelsum_cox(event = fu.stat, time = fu.time, data = df_selected), NA)
})
