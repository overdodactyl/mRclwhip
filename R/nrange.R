#' Summarize three columns as N (low, high)
#'
#' Often useful for summarizing an association metric and it's confidence interval.
#'
#' @param .data A tbl.
#' @param n,low,high Column names to summarize
#' @param accuracy Number to round to, NULL for automatic guess.
#' @param name Name of new column.
#' @param remove If TRUE, remove input columns from output data frame.
#' @param formatter scales function used to format text
#' @param big.mark Character used between every 3 digits to separate thousands.

#' @return An object of the same class as .data.
#' @examples
#' df <- tibble::tribble(
#'   ~Group, ~n, ~low, ~high,
#'   "a", 1.323453, 1.154, 1.44,
#'   "b", .801, .741, .9891
#' )
#'
#' df %>% nrange(n, low, high, formatter = "comma")
#' @export

nrange <- function(.data, n, low, high, accuracy = 0.01, name = "Estimate",
                   formatter = c("comma", "percent", "number") , remove = TRUE, big.mark = ",") {

  # if (!"scales" %in% loadedNamespaces())
  #   attachNamespace("scales")
  #
  # comma <- function(...) {scales::comma(...)}
  # percent <- function(...) {scales::percent(...)}
  # number <- function(...) {scales::number(...)}

  formatter <- match.arg(formatter)

  if (formatter == "comma") {
    res <- .data %>%
      mutate_at(vars({{n}}, {{low}}, {{high}}), list(~ scales::comma(., accuracy = accuracy, big.mark = big.mark)))
  } else if (formatter == "percent") {
    res <- .data %>%
      mutate_at(vars({{n}}, {{low}}, {{high}}), list(~ scales::percent(., accuracy = accuracy, big.mark = big.mark)))
  } else if (formatter == "number") {
    res <- .data %>%
      mutate_at(vars({{n}}, {{low}}, {{high}}), list(~ scales::number(., accuracy = accuracy, big.mark = big.mark)))
  }

  res <- res %>%
    mutate({{name}} := paste0({{n}}, " (", {{low}}, ", ", {{high}}, ")"))

  if (remove) {
    res <- select(res, -{{n}}, -{{low}}, -{{high}})
  }

  res
}

##' @importFrom scales comma
##' @export
NULL
