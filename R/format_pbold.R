#' Format a column
#'
#' Format a column of pvalues for a flextable
#'
#' @param ft A flextable object
#' @param pval (String) Name of pvalue colum
#' @param j A numeric column index.  If NA, it will be the last column in the table.
#' @param psig A numeric threshold to bold p-values
#' @export


format_pbold <- function(ft, pval, psig = 0.05, j = NA) {

  if (is.na(j)) {
    j = ncol(ft$body$dataset)
  }

  pval <- glue::backtick(pval)

  form <- stats::as.formula(paste0("~ ", pval, "== '<0.001' | as.numeric(", pval, ")< ", psig))

  suppressWarnings({
    ft <- ft %>%
      flextable::bold(form, j = j)
  })
}


