#' Add tables and titles to a word document
#'
#' Extends the functionality of the officer package by easing the creation of word documents.  Tables
#' will be added sequentually.  A page break will be inserted after every table except the last.
#'
#' @export
#' @param .docx An officer object
#' @param tables A list of tables.
#' @param titles A list of titles.  Must be either NA or the length of tables.  If NA, tables will be
#'   titles 'Table 1', 'Table 2', ...

add_table <- function(.docx, tables, titles = NA) {

  if (identical(titles, NA)) {
    l <- length(tables)
    titles <- paste0("Table ", 1:l)
  }

  if (length(tables) != length(titles)) {
    usethis::ui_stop("Titles must either be NA or the same length as tables.")
  }

  for (i in seq_along(titles)) {
    page_break = ifelse(i == length(titles), FALSE, TRUE)
    .docx <- .add_table(.docx, titles[i], tables[[i]], page_break)
  }

  .docx

}

# Define style for table title
tbl_head <- officer::fp_text(color = "black", font.size = 12, bold = TRUE,
                    font.family = "Times New Roman",
                    vertical.align = "baseline", shading.color = "transparent")


# Generate title
table_header <- function(title) {
  officer::fpar(officer::ftext(title, prop = tbl_head), fp_p = officer::fp_par(text.align = "center"))
}


.add_table <- function(.docx, title, table, page_break = TRUE) {

  title <- table_header(title)

  res <- .docx %>%
    officer::body_add_fpar(title) %>%
    officer::body_add_par("") %>%
    flextable::body_add_flextable(table)

  if (page_break) {
    res <- officer::body_add_break(res)
  }

  res
}
