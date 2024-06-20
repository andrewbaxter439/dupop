#' Generate code for a single variable
#'
#' @param variable Vector to generate code for
#' @param distribution Distribution of variable
#'
code_single_variable <- function(variable, distribution = "normal") {

  var_type <- class(variable)

  if (var_type == "integer") {

    min_var <- min(variable)
    max_var <- max(variable)

    if (distribution == "normal") {
      probs <- dnorm(min_var:max_var, mean(variable), sd(variable)) |> signif(4)
    }

    glue::glue("sample({min(variable)}:{max(variable)}, {{n}}, replace = TRUE, ",
               "prob = dnorm({min_var}:{max_var}, {mean(variable)}, {sd(variable) |> signif(4)}))")

  }

}
