#' Check if any number of values are identical
#'
#' Returns TRUE if all, or any, arguments are equal to each other using the test base::identical
#' @param ...  Two or more values to compare
#' @return Logical
#' @examples
#' all_identical(1, 1, 1)
#' all_identical(1, 1, "1")
#' all_identical(1, 1, 2)
#' all_identical(1, 1, NA)
#' @export
#'
all_identical <- function(...) {
  .check_length(...)
  all(.identical(...))
}

#' @export
#' @rdname all_identical
#' @examples
#' any_identical(1, 1, 2)
#' any_identical(1, "1", 2)
any_identical <- function(...) {
  .check_length(...)
  any(.identical(...))
}

.check_length <- function(...) {

  input_num <- length(c(...))

  if (input_num < 2) {
    usethis::ui_stop(
      "Input length must be {usethis::ui_value('>= 2')}, \\
      not {usethis::ui_value(input_num)}."
    )
  }

}

.identical <-function(...)  {

  input <- purrr::flatten(list(...))

  if (length(input) > 2L) {
    out <- c(identical(input[[1]], input[[2]]), .identical(input[-1]))
  } else {
    out <- identical(input[[1]], input[[2]])
  }

  out

}
