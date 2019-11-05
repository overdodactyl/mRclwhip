#' Return indexes of values that are (or are not) NA
#'
#' Numeric indexes of non-NA values in a vector
#' @param x A vector.
#' @param negate If `TRUE`, return indexes of NA values.
#' @examples
#' non_na(c(1, 2, NA, 3, NA))
#' non_na(c(1, 2, NA, 3, NA), negate = TRUE)
#' @export

non_na <- function(x, negate = FALSE) {
  rows <- unfill_vec(x)

  res <- which(is.na(rows))

  if (!negate) {
    all <- 1:length(x)

    res <- all[!all %in% res]
  }

  res
}
