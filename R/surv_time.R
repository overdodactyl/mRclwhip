#' Calculate time variable for survival analysis
#'
#' Clinical data often has separate columns for event dates and last follow-up dates.
#' This function calculates the time variable to be used in time-to-event analysis (i.e. time to event or time to censor).
#'
#' @param .data A tbl
#' @param start_time A column of dates or datetimes to use as the starting point.  This will often be date of diagnosis.
#' @param event A column of numeric indicators (0,1) where 1 represents occurrence of the event.
#' @param event_date A column of dates or datetimes corresponding to the event.
#' @param fu_date A column of dates or datetimes corresponding to the last known follow-up date.
#' @param surv_end_time The name of the output column representing the last date to be used in survival.
#' This will either be event_date or fu_date.
#' @param surv_time The name of the output column representing survival time
#' @export
#' @examples
#' # Create some test data
#' set.seed(12215)
#'
#' surv_data <- tibble::tibble(
#'   diagnosis_dt = sample(seq(as.Date("1985/01/01"), as.Date("1990/01/01"), by = "day"), 10),
#'   death = sample(c(0, 1), 10, replace = TRUE),
#'   fu_dt = as.Date("2009/11/13")
#' )
#'
#' surv_data <- surv_data %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(death_dt = dplyr::if_else(death == 1,
#'        sample(seq(as.Date("2010/01/01"), as.Date("2019/01/01"), by = "day"), 1),
#'        as.Date(NA))) %>%
#'   dplyr::ungroup()
#'
#' surv_data %>%
#'   surv_time(diagnosis_dt, death, death_dt, fu_dt)
surv_time <- function(.data, start_time, event, event_date, fu_date, surv_time = "surv_time", surv_end_time = "surv_end_time") {
  if (!all(.data[[dplyr::enexpr(event)]] %in% c(0, 1))) {
    stop("event column should only consist of c(0,1)")
  }

  event <- enquo(event)
  start_time <- enquo(start_time)
  event_date <- enquo(event_date)
  fu_date <- enquo(fu_date)

  .data %>%
    dplyr::mutate(
      !!surv_end_time := dplyr::if_else(!!event == 1, !!event_date, !!fu_date),
      !!surv_time := calc_age(!!start_time, !!dplyr::quo(surv_end_time))
    )
}
