#' @title Standard options used to format a flextable
#' @export
#' @param data A `data frame` or `flextable` object
#' @param center_cols,left_cols,right_cols numeric vector of columns to center, left or right align
#' @param fontsize, fontsize
#' @param fontname fontname
#' @param center_num Center numeric columns?
#' @param header1,header2 Named list of values and colwidths.  header1 will replace the original header.  header2 will add a
#' second header on top.
#' @param bold_header Bold the header column(s)?
#' @param pbold (String) Name of column to bold as p-values
#' @param psig A numeric threshold to bold p-values#' @param psig A numeric threshold to bold p-values

format_flextable <- function(data, center_cols = NULL, left_cols = NULL, right_cols = NULL,
                             header1 = NULL, header2 = NULL, bold_header = FALSE,
                             center_num = TRUE, fontsize = 10, fontname = "Times New Roman",
                             pbold = NA, psig = 0.05) {

  if (is.data.frame(data)) {
    ft <- flextable::flextable(data)
  } else {
    ft <- data
  }

  border <- officer::fp_border(width = 1.5)

  if (!is.null(header1)) {
    ft <- ft %>% flextable::delete_part()

    if (is.null(header1$colwidths)) header1$colwidths <- rep(1, length(header1$values))

    ft <- ft %>% flextable::add_header_row(values = header1$values, colwidths = header1$colwidths)
  }

  if (!is.null(header2)) {
    if (is.null(header2$colwidths)) header2$colwidths <- rep(1, length(header2$values))

    ft <- ft %>% flextable::add_header_row(values = header2$values, colwidths = header2$colwidths)
    ft <- ft %>% flextable::border(border.top = border, part = "header")
  }

  last_col <- ncol(ft$body$dataset)

  ft <- ft %>%
    flextable::font(fontname = fontname, part = "all") %>%
    flextable::fontsize(size = fontsize, part = "all") %>%
    # flextable::autofit(add_w = 0, add_h = 0) %>%
    flextable::hline_top(border = border, part = "header") %>%
    flextable::hline_bottom(border = border, part = "all") %>%
    flextable::padding(padding.top = 0, part = "all") %>%
    flextable::padding(padding.bottom = 0, part = "all") %>%
    flextable::padding(padding.left = 5, j = 1, part = "header") %>%
    flextable::valign(part = "header") %>%
    flextable::align(j = 1, part = "all") %>%
    flextable::align(j = 2:last_col, part = "all", align = "center") %>%
    flextable::height_all(height = 0)

  ft <- ft %>% flextable::bold(part = "header", bold = bold_header)

  numeric <- ft$body$dataset %>%
    dplyr::select_if(is.numeric) %>%
    names()

  numeric <- match(numeric, names(ft$body$dataset))

  if (center_num & length(numeric) > 0) ft <- flextable::align(ft, j = numeric, align = "center", part = "all")

  if (!is.null(center_cols)) ft <- flextable::align(ft, j = center_cols, align = "center", part = "all")

  if (!is.null(left_cols)) ft <- flextable::align(ft, j = left_cols, align = "left", part = "all")

  if (!is.null(right_cols)) ft <- flextable::align(ft, j = right_cols, align = "right", part = "all")

  if (!identical(pbold, NA)) {
    for (p in pbold) {
      j = match(p, names(ft$body$dataset))
      ft <- format_pbold(ft, pval = p, psig = psig, j = j)
    }
  }


  flextable::width(ft, j = 1:ncol(ft$body$dataset), width = flextable::dim_pretty(ft)$widths)

}
