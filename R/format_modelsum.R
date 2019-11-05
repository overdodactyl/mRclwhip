#' @title Generate a formatted flextable from a modelsum object
#' @export
#' @param tbl A modlesum object
#' @return A flextable object

format_modelsum <- function(tbl) {
  tmp <- as.data.frame(tbl)

  if ("estimate" %in% names(tmp)) {
    metric <- "estimate"
  } else if ("OR" %in% names(tmp)) metric <- "OR"

  if (any(stringr::str_detect(names(tmp), "wald"))) {
    suffix <- ".wald"
  } else {
    suffix <- ""
  }

  ci_vars <- c(metric, paste0("CI.lower.", metric, suffix), paste0("CI.upper.", metric, suffix))

  tmp <- tmp %>% dplyr::mutate_at(dplyr::vars(ci_vars), list(~ scales::comma(., .01)))


  tmp <- tmp %>% dplyr::mutate(ci = paste0(!!as.name(ci_vars[1]), " (", !!as.name(ci_vars[2]), ", ", !!as.name(ci_vars[3]), ")"))


  tmp <- tmp %>% dplyr::select(.data$label, .data$ci, .data$p.value)

  tmp <- tmp %>% dplyr::mutate(p.value = format_p(.data$p.value))

  # tmp <- format_modelsum_p(tmp)

  ci_name <- paste0(metric, " (95% CI)")

  ft <- format_flextable(tmp, center_cols = 2, left_cols = 1) %>%
    flextable::set_header_labels(label = "", ci = ci_name, p.value = "P value") %>%
    format_pbold(pval = "p.value")

  return(ft)
}

