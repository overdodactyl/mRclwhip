#' Knit the Rmd file that is currently open
#'
#' This is a convenient alternative to using the knit button
#' when you want to specify additional arguments such as output directory.
#'
#' @param output_dir The output directory, as used in \code{rmarkdown::render}
#' @param ... Additional arguments to pass to \code{rmarkdown::render}
#'
#' @export
#' @examples
#' \dontrun{
#' # This command will knit the Rmd file that is currently open into the directory R.
#' knitthis("R")
#'
#' # Assuming the open file is data_prep.Rmd, this would be equivalent to running:
#' rmarkdown::render(".../data_prep.Rmd", output_dir = ".../R")
#'
#' }
#'
knitthis <- function(output_dir = "documents", ...) {
  path <- get_active_r_file()

  rmarkdown::render(path, output_dir = output_dir, ...)

  # if (open) {
  #   # rstudioapi::sendToConsole("browseURL('docs/index.html')")
  #   # fs::file_show(path)
  # }
}

## Code adapted from https://github.com/r-lib/usethis/blob/2d9d73456940d07cba15b3accd01dc38d4d3def4/R/r.R

get_active_r_file <- function(path = "R") {
  if (!rstudioapi::isAvailable()) {
    usethis::ui_stop("Argument {ui_code('name')} must be specified.")
  }
  ## rstudioapi can return a path like '~/path/to/file' where '~' means
  ## R's notion of user's home directory
  active_file <- proj_path_prep(rstudioapi::getSourceEditorContext()$path)

  ext <- fs::path_ext(active_file)
  if (ext != "Rmd") {
    usethis::ui_stop(
      "Open file must have {ui_value('.Rmd')} as extension, \\
      not {ui_value(ext)}."
    )
  }

  fs::path_file(active_file)
}


## usethis policy re: preparation of the path to active project
proj_path_prep <- function(path) {
  if (is.null(path)) {
    return(path)
  }
  fs::path_real(path)
}
