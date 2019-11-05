#' Check if all values are unique
#'
#' Checking if all values are unique can be a good step in QC to ensure entries are not duplicated.  If passed a
#' list, the input is first flattened (which may result in type casting).
#'
#' @param vec A vector or list.
#' @export
#' @examples
#' all_unique(c(2,1,3))
#' all_unique(c(1,1,2))
#' all_unique(list(1, "1", 2))

all_unique <- function(vec) {
  if (is.list(vec)) vec <- purrr::flatten(vec)
  length(vec) == dplyr::n_distinct(vec)
}
