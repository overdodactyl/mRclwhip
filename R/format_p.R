#' @title Format a column of p-values
#' @param pval A vector of p values
#' @param na_string Value to return if pval is NA
#' @param empty_string Value to return if pval == ""
#' @param sig Level of signficance.  If formatted p-value = significance level, an extra digit will be added
#' @export
#'
format_p <- function(pval, na_string = "-", empty_string = "-", sig = NA) {

  if (is.data.frame(pval)) {
    usethis::ui_stop("pval must be a vector of characters or numbers.")
  }

  res <- c()

  for (p in pval) {
    res <- c(res, .internal_pformater(p, na_string, empty_string, sig))
  }

  res

}


.internal_pformater <- function(p, na_string = "-", empty_string = "-", sig) {
  if (is.na(p)) return(na_string)
  if (is.character(p)) {
    if (gsub(" ", "", p, fixed = TRUE) == "<0.001") p = 0
    else if (p == "") p = return(empty_string)
    else {
      p <- tryCatch({
        as.numeric(p)
      }, warning = function(w) {
        usethis::ui_stop("Couldn't format p value: {usethis:::ui_value(p)}")
      })
    }
  }

  if (p > 1) {
    usethis::ui_stop("P-value greater than one.")
  }

  if (p < 0.001) res <- "<0.001"
  else if (p > .99) {
    res <- "1.00"
    usethis::ui_warn("P-value of {usethis::ui_value(p)} rounded to 1.00")
  }
  else if (p < 0.1) res <- scales::pvalue(p, accuracy = .001)
  else res <- scales::pvalue(p, accuracy = .01)

  if (!is.na(sig)) {
    if (res == scales::number(sig, .001)) res <- scales::pvalue(p, .0001)
  }

  res
}

