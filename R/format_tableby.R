#' @title Generate a formatted flextable from a tableby object
#' @export
#' @param tbl A tableby object
#' @param bold_var Bold variable names in the first column?
#' @param ... Additional arguments to pass to format_flextable
#' @param pvals A character vector of p values to format
#' @param bold_p (Logical) If TRUE, p values less than 0.05 will be bolded.
#'
#' @examples
#' \dontrun{
#' tableby(arm ~ ., data = mockstudy, digits = 2) %>% format_tableby()
#' }
#'
format_tableby <- function(tbl, bold_var = T, bold_p = T, pvals = "p value", ...) {
  if (!is.data.frame(tbl)) {
    tmp <- tbl %>%
      summary(text = T) %>%
      as.data.frame()
  } else {
    tmp <- tbl
  }

  if (pvals == "p value" & (!pvals %in% names(tmp))) pvals = NA

  names(tmp)[1] <- "Label"


  if (!identical(pvals, NA)) {
    for (p in pvals) {
      j = match(p, names(tmp))
      tmp <- mutate(tmp, {{p}} := format_p(tmp[[p]], na_string = "", empty_string = ""))
    }
  }

  if ("p value" %in% names(tmp)) {
    tmp <- mutate(tmp, `p value` = format_p(.data$`p value`, na_string = "", empty_string = ""))
  }

  indent_cols <- which(startsWith(tmp$Label, "- "))

  tmp <- tmp %>% mutate(Label = stringr::str_remove_all(.data$Label, "- "))

  center_cols <- 2:ncol(tmp)

  ft <- format_flextable(tmp, ...) %>%
    flextable::padding(i = indent_cols, j = 1, padding.left = 15, part = "body") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::bold(~ !startsWith(Label, " "), ~Label) %>%
    flextable::set_header_labels(Label = "")

  if (!identical(pvals, NA) & bold_p) {
    center_cols <- center_cols[-length(center_cols)]
    for (p in pvals) {
      j = match(p, names(ft$body$dataset))
      ft <- format_pbold(ft, pval = p, psig = 0.05, j = j)
    }
  }

  if (!is.na(pvals)) {
    # Name the P-Value columns
    pnames <- stats::setNames(rep("P value", length(pvals)), pvals)
    ft <- flextable::set_header_labels(ft, values = pnames)
  }


  if (!bold_var) ft <- flextable::bold(ft, j = 1, bold = F)

  ft <- flextable::align(ft, j = center_cols, align = "center", part = "all")


  return(ft)
}

