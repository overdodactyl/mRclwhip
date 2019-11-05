#' @title Add a formatted footer to a flextable
#' @export
#' @param ft A flextable object
#' @param text The footer text
#' @param fontsize The fontsize
#' @param fontname The fontname
#' @return A flextable object

add_footer <- function(ft, text = "Sample footnote", fontsize = 10, fontname = "Times New Roman") {
  ft %>%
    flextable::add_footer_lines(values = text) %>%
    flextable::font(fontname = fontname, part = "footer") %>%
    flextable::fontsize(size = fontsize, part = "footer") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "footer") %>%
    flextable::padding(padding.left = 6, j = 1, part = "footer") %>%
    flextable::valign(valign = "center", part = "footer")
}
