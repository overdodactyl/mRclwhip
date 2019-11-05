#' Summarize three columns as N (low, high)
#'
#' Often useful for summarizing an association metric and it's confidence interval.
#'
#' @param .data A tbl.
#' @param n,low,high Column names to summarize
#' @param accuracy Number to round to, NULL for automatic guess.
#' @param name Name of new column.
#' @param remove If TRUE, remove input columns from output data frame.
#' @param big.mark Character used between every 3 digits to separate thousands.
#' @return An object of the same class as .data.
#' @examples
#' df <- tibble::tribble(
#'   ~Group, ~n, ~low, ~high,
#'   "a", 1.323453, 1.154, 1.44,
#'   "b", .801, .741, .9891
#' )
#'
#' df %>% nrange(n, low, high)
#' @export

nrange <- function(.data, n, low, high, accuracy = 0.01, name = "Estimate", remove = TRUE, big.mark = ",") {
  n <- enquo(n)
  low <- enquo(low)
  high <- enquo(high)

  res <- .data %>%
    mutate_at(vars(!!n, !!low, !!high), list(~ scales::comma(., accuracy, big.mark = big.mark))) %>%
    mutate(!!name := paste0(!!n, " (", !!low, ", ", !!high, ")"))

  if (remove) {
    res <- select(res, -!!n, -!!low, -!!high)
  }

  res
}
