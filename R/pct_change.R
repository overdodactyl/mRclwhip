#' Calculate the percent change between two values or vectors of values
#'
#' @param start A value or vector of values
#' @param final A value or vector of values
#' @param perc A logical indicating whether or not to format the value with a percent symbol
#' @export
#' @examples
#' tibble::tibble(
#'  start = seq(0.1, 1.1, 0.1),
#'  final = sample(seq(0.1, 1, 0.1), 11, replace = T)
#'  ) %>%
#' dplyr::mutate(pct_diff = pct_change(start, final))


pct_change <- function(start, final, perc = FALSE) {

  x <- (final - start) / (abs(start))

  if (perc) {
    x <- scales::percent(x)
  }

  x

}
