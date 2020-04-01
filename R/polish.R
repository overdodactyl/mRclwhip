#' Tidy and put the finishing touches on a model output
#'
#' Wrapper around broom::tidy() with some additional finishing touches!
#'
#' @param x A model object to be converted into a tibble::tibble()
#' @param .labels A named list of variable labels. If NULL, current labels will be used
#' @param .conf_int Logical to return a 95% confidence interval
#' @param .flextable Logical to return a formatted flextable object if TRUE or a list of a dataframe and row numbers to use in flextable::padding()
#' @param .header1 See ??mRclwhip::format_flextable for details
#' @param .header2 See ??mRclwhip::format_flextable for details
#' @param ... additional arguments passed to broom::tidy()

#' @return A flextable::flextable() or a list with a tidy tibble::tibble() a vector of row numbers
#' @examples
#' df <- tibble::tribble(
#'   ~id, ~age, ~mile_time,
#'   "a",  23,   7,
#'   "b",  33,   6.5,
#'   "c",  18,   7.25,
#'   "d",  27,   6.8,
#' )
#'
#' lm_res <- lm(mile_time ~ age, data = df)
#'
#' lm_res %>% polish(.header1 = list(values = c("", "Estimate (95% CI)")))
#' @export

polish <- function(x, .labels = NULL, .conf_int = TRUE, .flextable = TRUE, .header1 = NULL, .header2 = NULL, ...) {

  if(is.null(.labels)) {

    tryCatch({
      df1 <- eval(x$call$data)
    },
    error = function(e) {
      usethis::ui_stop(
        "The data frame {x$call$data} does not exist in the environment anymore. Please pass in labels or recreate {x$call$data}."
        )
      }
    )


    labs <- mRclwhip::label_df(eval(x$call$data)) %>%
      dplyr::mutate(
        label = dplyr::case_when(
          label == "" ~ variable,
          label != "" ~ label,
          TRUE        ~ NA_character_
        )
      )

  } else {

    labs <- tibble::tibble(
      variable = names(.labels),
      label = .labels %>% unname()
    )

    if(nrow(labs) != length(attr(x$terms, "term.labels"))) {
      usethis::ui_stop(
        glue::glue("You provided {nrow(labs)} labels but your model has {length(attr(x$terms, 'term.labels'))} covariate(s). ",
        "Please provide the same number of labels as covariates.")
      )
    }

  }

  ## Create a data frame with the # of levels for each factor var
  factor_vars <- eval(x$call$data) %>%
    dplyr::select_if(function(x) is.factor(x) | is.character(x)) %>%
    names()
  numlevs <- purrr::map_dbl(factor_vars, ~length(levels(as.factor(eval(x$call$data)[[.x]]))))
  levs_df <- tibble::tibble(
    variable = factor_vars,
    numlevs = numlevs
  )

  res <- x %>%
    broom::tidy(conf.int = .conf_int, ...) %>%
    dplyr::filter(stringr::str_detect(term, "(Intercept)", negate = T)) %>%
    dplyr::mutate(
      variable = stringr::str_extract(term, mRclwhip::vec_to_regex(labs[["variable"]])),
      level = stringr::str_remove(term, variable),
      p.value = mRclwhip::format_p(p.value)
      ) %>%
    dplyr::left_join(labs, "variable") %>%
    dplyr::left_join(levs_df, "variable") %>%
    dplyr::mutate(
      label_lev = case_when(
        numlevs > 2                 ~ level,
        !is.na(level) & level != "" ~ as.character(glue::glue("{label} ({level})")),
        TRUE                        ~ label
      ),
      estimate = glue::glue("{scales::comma(estimate, accuracy = 0.01)}"),
      conf.low = glue::glue("{scales::comma(conf.low, accuracy = 0.01)}"),
      conf.high = glue::glue("{scales::comma(conf.high, accuracy = 0.01)}"),
      Estimate = glue::glue("{estimate} ({conf.low}, {conf.high})"),
      numlevs = mRclwhip::unfill_vec(numlevs)
    ) %>%
    dplyr::select(label_lev, Estimate, label, numlevs)

  newrow_ind <- res %>% dplyr::mutate(rn = dplyr::row_number()) %>% dplyr::filter(numlevs > 2) %>% pull(rn)
  rowlab <- res %>% dplyr::filter(dplyr::row_number() %in% newrow_ind) %>% dplyr::pull(label)

  addrow <- function(.before, lab) {

    res <<- res %>%
      dplyr::add_row(label_lev = lab, .before = .before)

  }

  purrr::walk2(newrow_ind, rowlab, addrow)

  ## get row numbers for padding the factor levels in the flextable
  padding_rn <- res %>%
    tidyr::fill(numlevs) %>%
    dplyr::mutate(
      rn = dplyr::row_number(),
      numlevs = ifelse(numlevs > 2, numlevs, NA)
    ) %>%
    dplyr::filter(!is.na(numlevs)) %>%
    dplyr::pull(rn)

  ## return either a flextable or a list with a dataframe and row numbers
  if(.flextable) {

    res %>%
      dplyr::select(-c(label, numlevs)) %>%
      mRclwhip::format_flextable(
        header1 = .header1,
        header2 = .header2
      ) %>%
      flextable::padding(i = padding_rn, j = 1, padding.left = 25)

  } else {

    res <- res %>% dplyr::select(-c(label, numlevs))

    list(res, padding_rn)

  }


}
