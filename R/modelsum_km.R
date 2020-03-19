#' @title Kaplan-Meier Table
#' @param event Name of event column
#' @param time Name of time column
#' @param data A tbl.
#' @param times Numeric vector of time points to analyze
#' @param freedom.from Character string to be used in table header
#' @param pval Include p value?
#' @export


modelsum_km <- function(event, time, data = NA, times = 1:3, freedom.from = "death", pval = T) {
  vars <- data %>% dplyr::select(-!!enquo(event), -!!enquo(time))

  arguments <- as.list(match.call())
  time <- eval(arguments$time, data)
  event <- eval(arguments$event, data)

  perm_fit <- function(var, lab) {
    if (is.null(var)) {
      surv.form <- survival::Surv(time, event) ~ NULL
    } else {
      surv.form <- survival::Surv(time, event) ~ eval(as.name(var))
    }

    surv.fit <- survival::survfit(surv.form, data = data)

    if (is.null(var)) {
      sum <- summary(surv.fit)
      events <- as.numeric(sum$table[4])
    } else {
      sum <- summary(surv.fit)
      events <- as.numeric(sum$table[,4])
    }

    p.val <- tryCatch({
      surv.diff <- survival::survdiff(surv.form, data = data)
      1 - stats::pchisq(surv.diff$chisq, length(surv.diff$n) - 1)
    },
    error = function(e) {
      NA
    }
    )

    res <- .get_timepoints_survsummary(fit = surv.fit, data = data, times = times)

    res <- res %>%
      dplyr::mutate_at(dplyr::vars(.data$surv, .data$ci.lower, .data$ci.upper), scales::percent) %>%
      dplyr::mutate(
        HR = paste0(.data$surv, " (", .data$ci.lower, ", ", .data$ci.upper, ")"),
        time_n = time,
        time = paste0(.data$time, "-year freedom from ", freedom.from),
        strata = stringr::str_replace_all(.data$strata, "eval\\(as.name\\(var\\)\\)=", ""),
        strata = stringr::str_to_title(.data$strata),
        strata_size = scales::comma(.data$strata_size, accuracy = 1)
      ) %>%
      dplyr::group_by(.data$strata) %>%
      dplyr::mutate(
        # event = events %>% scales::comma(), #sum(.data$n.event) %>% scales::comma(),
        time = factor(.data$time),
        time = forcats::fct_reorder(.data$time, .data$time_n)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        Level = .data$strata,
        Total = .data$strata_size,
        # Event = .data$event,
        .data$time,
        .data$HR
      ) %>%
      tidyr::spread(.data$time, .data$HR) %>%
      mutate(Event = events %>% scales::comma(accuracy = 1)) %>%
      dplyr::select(1, 2, `Total Events` = .data$Event, dplyr::everything()
      )

    res$Level <- unfill_vec(res$Level)

    res <- dplyr::bind_rows(data.frame(Level = lab, p.value = p.val, stringsAsFactors = FALSE), res) %>%
      dplyr::select(-.data$p.value, .data$p.value) %>%
      mutate(`P value` = format_p(.data$p.value)) %>%
      dplyr::select(-.data$p.value)
      # format_p(., p.value)

    if (nrow(res) == 2) {
      res <- res %>% dplyr::filter(dplyr::row_number() != 1)
    }

    res
  }

  all <- perm_fit(NULL, "All")

  if (length(names(vars)) > 0) {
    tbl <- purrr::map2_df(names(vars), as.character(Hmisc::label(vars)), perm_fit)
  } else {
    tbl <- data.frame()
  }

  tbl <- dplyr::bind_rows(all, tbl)

  indent_rows <- tbl %>%
    dplyr::mutate(col = 1:dplyr::n()) %>%
    dplyr::filter(!is.na(.data$Total), col != 1) %>%
    .$col

  if (!pval) {
    tbl <- tbl %>% dplyr::select(-.data$`P value`)
  }

  tbl %>%
    dplyr::rename(" " = "Level") %>%
    format_flextable() %>%
    flextable::padding(j = 1, i = indent_rows, padding.left = 30, part = "body")
}

## Most helper functions re-purposed from
## https://github.com/kassambara/survminer/blob/e52f64d35e28016326af7331bf3e686d0c329ed7/R/utilities.R


.clean_strata <- function(strata, fit) {
  is_dollar_sign <- grepl("$", as.character(strata)[1], fixed = TRUE)
  if (is_dollar_sign) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed = TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed = TRUE)
    strata <- as.factor(strata)
  }
  else if (!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}

.get_variables <- function(strata, fit, data = NULL) {
  variables <- sapply(
    as.vector(strata),
    function(x) {
      x <- unlist(strsplit(x, "=|,\\s+", perl = TRUE))
      x[seq(1, length(x), 2)]
    }
  )
  variables <- unique(as.vector(variables))
  variables <- intersect(variables, colnames(.get_data(fit, data)))
  variables
}

# remove white space at the head and the tail of a string
.trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

# extract dataset if not provided
.get_data <- function(fit, data = NULL, complain = TRUE) {
  if (is.null(data)) {
    if (complain) {
      warning("The `data` argument is not provided. Data will be extracted from model fit.")
    }
    data <- eval(fit$call$data)
    if (is.null(data)) {
      stop("The `data` argument should be provided either to ggsurvfit or survfit.")
    }
  }
  data
}

# levels of a given variable used in survfit formula
# ----------------------------
# variable: variable name
.get_variable_value <- function(variable, strata, fit, data = NULL) {
  res <- sapply(as.vector(strata), function(x) {
    x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl = TRUE))
    index <- grep(paste0("^", variable, "$"), x)
    .trim(x[index + 1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if (!is.null(var_levels)) {
    res <- factor(res, levels = var_levels)
  } else {
    res <- as.factor(res)
  }
  res
}


.get_timepoints_survsummary <- function(fit, data, times, decimal.place = 0) {
  survsummary <- summary(fit, times = times, extend = TRUE)

  if (is.null(fit$strata)) {
    .strata <- factor(rep("All", length(survsummary$time)))
    strata_names <- "All"
    strata_size <- rep(fit$n, length(.strata))
  }
  else {
    .strata <- factor(survsummary$strata)
    strata_names <- names(fit$strata)
    nstrata <- length(strata_names)
    strata_size <- rep(fit$n, each = length(.strata) / nstrata)
  }

  strata <- .clean_strata(.strata)
  res <- data.frame(
    strata = strata,
    time = survsummary$time,
    n.risk = round(survsummary$n.risk, digits = decimal.place),
    pct.risk = round(survsummary$n.risk * 100 / strata_size),
    n.event = round(survsummary$n.event, digits = decimal.place),
    cum.n.event = unlist(by(survsummary$n.event, strata, cumsum)),
    n.censor = round(survsummary$n.censor, digits = decimal.place),
    cum.n.censor = unlist(by(survsummary$n.censor, strata, cumsum)),
    surv = survsummary$surv,
    ci.lower = survsummary$lower,
    ci.upper = survsummary$upper,
    strata_size = strata_size
  )

  if (!is.null(fit$strata)) {
    variables <- .get_variables(res$strata, fit, data)
    for (variable in variables) res[[variable]] <- .get_variable_value(variable, res$strata, fit, data)
  }
  rownames(res) <- 1:nrow(res)
  res
}
