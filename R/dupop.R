#' Generate code to create duplicate population
#'
#' This
#'
#' @param data A dataframe of observations to mimic
#' @param id_col Column name for individual observation id's
#' @param group_cols Columns to cluster by (ordered hierarchically)
#' @param ... other arguments passed by name to each variable
#'
#' @export
#'
#' @examples
#'
#' df <- tibble::tibble(
#'   id = 1:100,
#'   group = rep(1:4, each = 25),
#'   gender = sample(c("m", "f"), 100, replace = TRUE),
#'   age = sample(18:45, 100, replace = TRUE, prob = dnorm(18:45, 24, 6)),
#'   height = rnorm(100, 160, 20)
#' )
#'
#' df |>
#'   dupop(id_col = id, group_cols = group, n = 1000)

dupop <- function(data, id_col, group_cols, n, ...) {

  require(rlang)
  require(purrr)

  id_col <- enquo(id_col)
  group_cols <- enquo(group_cols)

  col_types <- lapply(data, class)

  statements <- lapply(data, code_single_variable)

  statements |>
    imap(\(statement, varname) {

      glue::glue("{varname} = {statement}") |>
        glue::glue(n = n)

    })
}

