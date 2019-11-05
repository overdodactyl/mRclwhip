#' Add header rows within a column for values that repeat.
#'
#' Often helpful when wanting to create sub-groupings within a column in a final output.
#' @param .data A tbl.
#' @param col Column to add subheaders to.
#' @examples
#' df <- tibble::tribble(
#'   ~Label, ~value,
#'   "A", 1.2,
#'   "A", 1.3,
#'   "B", 5.0,
#'   "C", 4.3,
#'   "C", 4.1,
#'   "C", 5.2,
#'   "D", 6
#' )
#' df %>% in_row_headers(Label)
#' @export

in_row_headers <- function(.data, col) {
  unique_vals <- unique(.data[[dplyr::enexpr(col)]])

  for (val in unique_vals) {
    if (sum(.data[[dplyr::enexpr(col)]] == val) > 1) {
      insert <- match(val, .data[[dplyr::enexpr(col)]])
      .data <- .data %>% tibble::add_row(!!enquo(col) := val, .before = insert)
    }
  }

  .data
}
