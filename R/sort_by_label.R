#' @title Sort the columns of a data frame by it's labels (alphabetically)
#' @description Helpful when using labels in the final output
#' @export
#' @param .data A tbl
#' @return An object of the same class as .data

sort_by_label <- function(.data) {
  tmp <- data.frame(name = names(.data), label = Hmisc::label(.data))

  tmp <- arrange(tmp, .data$label)

  .data %>% select(one_of(as.character(tmp$name)))
}
