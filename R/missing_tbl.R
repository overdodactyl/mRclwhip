#' Generate a table of "missingness"
#'
#' Create a table of missing values along with percentages.
#' It may be undesirable to count a variable if it is dependent on another one.
#' This can be accounted for here by using a named vector where the name is the dependent variable and the
#' value is the indpendent variable.  c("B" = "A") can be thought of here as "B depends on A".
#' In other words, each entry in B will only be evaluated if A is not missing.
#' If the input is a labeled data frame, the final output will include a column of labels.
#'
#' @param data A tbl.
#' @param depends  A named vector representing variable dependencies.  For example, c("B" = "A", "C" = "B")
#' can be read as "B depends on A and C depends on B".  If a value for B is missing, it will only be counted if A is
#' not missing, and similarly for C and B.
#'
#' @export
#'
#' @importFrom dplyr mutate filter
#'
#' @examples
#' data <- tibble::tribble(
#'   ~A, ~B, ~C,
#'   1, 2, 3,
#'   NA, NA, 6,
#'   7, NA, 9
#' )
#'
#' data
#'
#' missing_tbl(data)
#'
#' missing_tbl(data, depends = c("B" = "A"))
#'
missing_tbl <- function(data, depends = NA) {

  depends <- suppressWarnings({depends_from_vector(depends)})
  # print(depends)

  res <- data.frame(
    Variable = names(data),
    stringsAsFactors = FALSE)

  res$Label <- Hmisc::label(data)

  res <- res %>% dplyr::left_join(depends, by = "Variable")

  res <- res %>%
    mutate(m = purrr::map2(.data$Variable, .data$Depends, .missing_var, .data = data),
           N = purrr::map_dbl(.data$m, purrr::pluck, "n"),
           `Non-Missing` = purrr::map_dbl(.data$m, purrr::pluck, "count"),
           Missing = purrr::map_dbl(.data$m, purrr::pluck, "missing"),
           `Non-Missing` = paste0(scales::comma(.data$`Non-Missing`), " (", scales::percent(.data$`Non-Missing`/.data$N), ")"),
           Missing = paste0(scales::comma(.data$Missing), " (", scales::percent(.data$Missing/.data$N), ")"),
           N = scales::comma(.data$N)) %>%
    select(-.data$m)

  if (all_identical(res$Label, "")) res$Label <- NULL
  if (all_identical(res$Depends, NA_character_)) res$Depends <- NULL

  res

}

.missing_var <- function(var, depends, .data) {
  tmp <- .data

  if (!is.na(depends)) {
    tmp <- tmp %>% dplyr::filter(!is.na(!!sym(depends)))
  }

  possible <- nrow(tmp)

  present <- tmp %>%
    filter(!is.na(!!sym(var))) %>%
    nrow()


  missing = possible - present

  list(n = possible, count = present, missing = missing)

}


"%||%" <- function(a, b) {
  if (!is.null(a) & !is.na(a)) a else b
}

depends_from_vector <- function(by) {

  if (identical(by, NA)) return(data.frame(Variable = "", Depends = "", stringsAsFactors = FALSE))

  # by <- by[!duplicated(by)]
  by_x <- names(by) %||% by
  by_y <- unname(by)

  # If x partially named, assume unnamed are the same in both tables
  by_x[by_x == ""] <- by_y[by_x == ""]

  data.frame(Variable = by_x, Depends = by_y, stringsAsFactors = FALSE)
}
