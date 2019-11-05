#' Create a data frame with columns for variables and variable labels
#'
#' Often helpful when wanting to replace variable names with labels in final output.
#'
#' @param .data A tbl.
#' @export

label_df <- function(.data) {
  data.frame(
    variable = names(.data),
    label = Hmisc::label(.data),
    stringsAsFactors = FALSE
  )
}
