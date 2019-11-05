#' Remove all data from the global environment
#'
#' Cleans the global environment.  Effectively the same as clicking the broom button in RStudio.
#'
#' @export

rm_all <- function() {
  rm(list=ls(envir = globalenv()), envir = globalenv())
}
