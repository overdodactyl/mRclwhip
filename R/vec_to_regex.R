#' Convert a vector to a regular expression
#'
#' Often helpful when constructing a regular expression based on a vector of values
#'
#' @param ... Vectors to collapse into the regex.  Duplicated elements are removed.
#' @param sep Character used to separate elements of ...
#' @param wrap 2-element vector of strings to wrap elements of ... in.
#' @export
#'
#' @examples
#' vec_to_regex(month.abb)
#'
#' vec_to_regex(letters[1:5], sep = "", wrap = c("[", "]+"))
vec_to_regex <- function(..., sep = "|", wrap = c("(", ")")) {
  elements <- unique(as.character(c(...)))

  pattern <- paste0(wrap[1], paste(elements, collapse = sep), wrap[2])

  pattern
}
