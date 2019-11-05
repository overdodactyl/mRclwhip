#' Coalesce columns of a data frame
#'
#' Often helpful when data has duplicate rows, but column data is split between entries.
#'
#' @param .data A tbl.
#' @export
#' @examples
#' df <- tibble::tribble(
#'   ~V1, ~V2, ~V3, ~V4,
#'   "A", 1,NA,2,
#'   "B", 3,4,3,
#'   "C", 5,6,3,
#'   "A", 2,3,NA
#' )
#'
#' df %>%
#'   dplyr::group_by(V1) %>%
#'   coalesce_rows()

coalesce_rows <- function(.data) {
.data %>% dplyr::summarise_all(coalesce_vec)
}

coalesce_vec <- function(vec) dplyr::coalesce(!!! vec)

