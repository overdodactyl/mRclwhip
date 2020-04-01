test_that("No errors produced from using polish", {
  coxph_res <- survival::coxph(survival::Surv(fu.time, fu.stat) ~ race + sex + bmi, data = mockstudy)

  Hmisc::label(mockstudy$sex) <- "Gender"



  expect_error(
    coxph_res %>%
      polish(
        .header1 = list(values = c("", "HR (95% CI)")),
        exponentiate = T
      ),
    NA
    )

  mtcars <- mtcars %>%
    dplyr::mutate_at(dplyr::vars(gear, am, carb), as.character)

  labs <- c(hp = "HP",
            gear = "# of gears",
            am = "Automatic",
            wt = "Weight",
            carb = "Carb"
  )

  lm_res <- lm(mpg ~ hp + gear + am + wt + carb, data = mtcars)

  expect_error(
    lm_res %>%
      polish(
        .header1 = list(values = c("", "Estimate (95% CI)")),
        .labels = labs
      ),
    NA
  )
})
