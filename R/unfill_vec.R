#' Unfill a vector
#'
#' The inverse of dplyr::fill.
#' Useful for formatting data frames for final output, where one often wants to avoid repeating values.
#' Often used in conjunction with \code{in_row_headers()}.
#' @param x a vector of values
#' @seealso \code{in_row_headers()}
#' @export
#' @examples
#' move_to_first(iris, Species) %>%
#'   in_row_headers(Species) %>%
#'   dplyr::mutate(Species = unfill_vec(Species))
unfill_vec <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)
}
