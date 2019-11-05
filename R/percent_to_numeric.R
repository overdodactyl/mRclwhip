#' Convert a formatted percent to a numeric
#'
#' Convert a vector of percents (strings) to a numeric vector.
#' @param perc A vector of strings
#' @export
#' @examples
#' percent_to_numeric(c("32.4%", "1.2%"))
percent_to_numeric <- function(perc) {
  as.numeric(sub("%", "", perc))
}
