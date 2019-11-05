#' Move a column to the beginning or end of a data frame
#'
#' Move a column to the beginning or end of a data frame
#'
#' @param .data A tbl.
#' @param col Name of column to move (unquoted)
#' @return An object of the same class as .data
#' @import dplyr
#' @examples
#' head(iris)
#' head(iris) %>% move_to_first(Species)
#' head(iris) %>% move_to_last(Sepal.Length)
#' @export
move_to_last <- function(.data, col) {
  col <- dplyr::enquo(col)
  .data %>%
    dplyr::select(-!!col, !!col)
}

#' @export
#' @rdname move_to_last
move_to_first <- function(.data, col) {
  col <- dplyr::enquo(col)
  .data %>%
    dplyr::select(!!col, everything())
}
