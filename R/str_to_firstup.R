#' Capitalize the first character of a string
#'
#' Capitalize the first character of a string without altering case of any other character.
#' @export
#' @param string String to modify
#' @examples
#' str_to_firstup("age at death")
#' str_to_firstup("baseline BMI")
#' c("age at death", "baseline BMI") %>% str_to_firstup()
str_to_firstup <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}
