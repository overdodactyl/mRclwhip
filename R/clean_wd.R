#' @title Move all html, pdf, and docx files to an output directory
#' @export
#' @param input_dir Path of directory to clean
#' @param output_dir Path to output directory
#' @param file_ext Extension of file to move

clean_wd <- function (input_dir = ".", output_dir = "./documents/", file_ext = c("docx", "html", "pdf")) {

  fs::dir_create(output_dir)

  file_ext <- stringr::str_remove(file_ext, "\\.")

  regex <- paste0(file_ext, collapse = "|")
  regex <- paste0("(", regex, ")$")

  fs::dir_ls(input_dir) %>%
    fs::path_filter(regexp = regex) %>%
    fs::file_move(output_dir)

}
