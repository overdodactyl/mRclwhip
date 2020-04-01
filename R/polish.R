#' Tidy and put the finishing touches on a model output
#'
#' Wrapper around \code{broom::tidy()} with some additional finishing touches!
#'
#' @param x A model object to be converted into a \code{tibble::tibble()}
#' @param .labels A named list of variable labels. If NULL, current labels will be used
#' @param .conf_int Logical to return a 95 percent confidence interval
#' @param .flextable Logical to return a formatted flextable object if TRUE or a list of a dataframe and row numbers to use in \code{flextable::padding()}
#' @param .header1 See \code{??mRclwhip::format_flextable} for details
#' @param .header2 See \code{??mRclwhip::format_flextable} for details
#' @param ... additional arguments passed to \code{broom::tidy()}
#' @examples
#' df1 <- tibble::tibble(
#'   id = letters,
#'   age = sample(seq(18, 39, 1), 26, replace = T),
#'   mile_time = sample(seq(6, 7.5, 0.1), 26, replace = T),
#'   gender = sample(c("Male", "Female"), 26, replace = T),
#'   country = sample(c("USA", "Canada", "Africa", "England"), 26, replace = T)
#' )
#'
#' lm_res <- lm(mile_time ~ age + gender + country, data = df1)
#'
#' lm_res %>% polish(.labels = c(age = "Age", gender = "Gender", country = "Country of origin"), .header1 = list(values = c("", "Estimate (95% CI)")))
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

  # find rows where the >2 level factors start to determine where to add the new rows with labels
  # Keep in mind, once you add one label as a new row, you must increase the index for the next
  # one by 1 (for each row added prior)
  newrow_ind <- res %>% dplyr::mutate(rn = dplyr::row_number()) %>% dplyr::filter(numlevs > 2) %>% pull(rn)
  rowlab <- res %>% dplyr::filter(dplyr::row_number() %in% newrow_ind) %>% dplyr::pull(label)

  for(i in 1:length(newrow_ind)) {

    newrow_ind[i] <- newrow_ind[i] + (i - 1)

  }

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
