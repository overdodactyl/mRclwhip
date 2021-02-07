#' @title Univariable Cox regression Table
#' @param event Name of event column
#' @param time Name of time column
#' @param data A tbl.
#' @param pval Include p-value?
#' @export
#'
modelsum_cox <- function(event, time, data = NA, pval = TRUE) {
  vars <- data %>% dplyr::select(-!!dplyr::enquo(event), -!!dplyr::enquo(time))

  arguments <- as.list(match.call())
  time <- eval(arguments$time, data)
  event <- eval(arguments$event, data)

  perm_fit <- function(var, lab) {
    num <- data %>%
      filter(!is.na((!!event))) %>%
      nrow()

    surv.form <- survival::Surv(time, event) ~ eval(as.name(var))

    broom::tidy(survival::coxph(surv.form, data), exponentiate = T, conf.int = TRUE) %>%
      dplyr::mutate(N = num) %>%
      dplyr::mutate(
        Label = lab,
        N = num,
        term = stringr::str_replace_all(.data$term, "eval\\(as\\.name\\(var\\)\\)", "")
      ) %>%
      nrange(.data$estimate, .data$conf.low, .data$conf.high, name = "HR", accuracy = .01) %>%
      dplyr::select(.data$Label, .data$term, .data$HR, .data$p.value, .data$N)
  }


  res <- purrr::map2_df(names(vars), as.character(Hmisc::label(vars)), perm_fit) %>%
    mutate(p.value = format_p(.data$p.value)) %>%
    dplyr::select(.data$Label, Level = .data$term, .data$N, .data$HR, .data$p.value)

  if (!pval) {
    res <- res %>% dplyr::select(-.data$p.value)
  }

  res <- res %>%
    format_flextable(header1 = list(values = c("", "Level", "N", "HR (95% CI)", "P-value")))

  if (pval) {
    suppressWarnings({
      res <- res %>% flextable::bold(~ p.value == "<0.001" | as.numeric(p.value) < 0.05, ~p.value)
    })
  }

  res
}
