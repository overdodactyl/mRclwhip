#' Summarize two columns as N (\%)
#'
#' Provided two columns, create a new column representing a number and percentage
#'
#' @param .data A tbl.
#' @param n Column of values.
#' @param perc Column of percentages (as decimals).
#' @param name Name of new column.
#' @param accuracy Number to round to, NULL for automatic guess.
#' @param remove If TRUE, remove input columns from output data frame.
#' @examples
#' iris %>%
#'   dplyr::group_by(Species) %>%
#'   dplyr::summarise(n = dplyr::n()) %>%
#'   dplyr::ungroup() %>%
#'   dplyr::mutate(freq = prop.table(n)) %>%
#'   countpct(n, freq)
#' @export

countpct <- function(.data, n, perc, name = "N", accuracy = 0.01, remove = TRUE) {
  res <- dplyr::mutate(.data, !!name := paste0(!!enquo(n), " (", scales::percent(!!enquo(perc), accuracy), ")"))

  if (remove) {
    res <- dplyr::select(res, -!!enquo(perc), -!!enquo(n))
  }

  res
}
