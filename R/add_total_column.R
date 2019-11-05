#' @title Add a total column to a tbl
#' @description Sum up all numeric columns in a data frame
#' @export
#' @param .data A tbl.
#' @param name The output column name. If omitted, it will be Total.
#' @return An object of the same class as .data
#' @examples
#' df <- data.frame(id = LETTERS[1:3], x1 = 1:3, x2 = 4:6, x3 = 7:9)
#' df
#' df %>% add_total_column()
add_total_column <- function(.data, name = "Total") {
  .data %>% dplyr::mutate(!!as.name(name) := rowSums(dplyr::select_if(., is.numeric)))
}
