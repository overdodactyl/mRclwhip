#' Calculate age
#'
#' This function is just a convenient wrapper to calculate age using lubridate.
#'
#' @param dob Starting date or datetime (usually date of birth).
#' @param ref_date Ending date or datetime
#' @export
#' @examples
#' calc_age(dob = as.Date("1974-03-14"))
#'
#' birth_days <- sample(seq(as.Date("1999/01/01"), as.Date("2000/01/01"), by = "day"), 5)
#' follow_ups <- data.frame(dob = birth_days, last_checkup = Sys.Date())
#'
#' follow_ups %>%
#'   dplyr::mutate(age_last_fu = calc_age(dob, last_checkup))
calc_age <- function(dob, ref_date = Sys.Date()) {
  lubridate::interval(dob, ref_date) / lubridate::dyears()
}
