#' Split character vector of N (low, high) into three columns
#'
#' @param .data A tbl.
#' @param col Column to separate
#' @param remove If TRUE, remove input columns from output data frame.
#' @return An object of the same class as .data.
#' @examples
#' tmp <- data.frame(
#'   obs = "A",
#'   val = "1224.11 (119.3214, 134.21)",
#'   stringsAsFactors = FALSE
#' )
#'
#' tmp %>%
#'   split_nrange(val) %>%
#'   nrange(n, low, high)
#' @export

split_nrange <- function(.data, col, remove = TRUE) {
  pattern <- "(.*) \\((.*),(.*)\\)"

  col <- enquo(col)

  res <- .data %>%
    mutate(
      n = gsub(pattern, "\\1", !!col),
      low = gsub(pattern, "\\2", !!col),
      high = gsub(pattern, "\\3", !!col)
    ) %>%
    mutate_at(vars(.data$n, .data$low, .data$high), as.numeric)

  if (remove) {
    res <- res %>% select(-!!col)
  }

  res
}
