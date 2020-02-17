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

nrange <- function(.data, n, low, high, accuracy = 0.01, name = "Estimate",
                   formatter = c("comma", "percent", "number") , remove = TRUE) {

  if (!"scales" %in% loadedNamespaces())
    attachNamespace("scales")

  formatter <- match.arg(formatter)
  format_func <- match.fun(formatter)

  res <- .data %>%
    mutate_at(vars({{n}}, {{low}}, {{high}}), list(~ format_func(., accuracy = accuracy))) %>%
    mutate({{name}} := paste0({{n}}, " (", {{low}}, ", ", {{high}}, ")"))

  if (remove) {
    res <- select(res, -{{n}}, -{{low}}, -{{high}})
  }

  res
}

##' @importFrom scales comma
##' @export
NULL
