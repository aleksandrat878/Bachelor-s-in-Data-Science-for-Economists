# R function to generate all formulas up to size n
# Erik-Jan van Kesteren
# October 2018

#' Generate Formulas for Modeling
#'
#' This function creates all possible formula strings for a response variable 
#' given a set of predictor variables and a specified number of predictors per formula.
#'
#' @param p Integer. The number of predictors to include in each formula.
#' @param x_vars Character vector. The names of available predictor variables.
#' @param y_var Character string. The name of the response variable.
#'
#' @return A character vector of formula strings.
#' @examples
#' x_vars <- c("age", "income", "education")
#' generate_formulas(2, x_vars, "outcome")
#' @export
generate_formulas <- function(p, x_vars, y_var) {
  # Input checking
  if (p %% 1 != 0) {
    stop("p must be an integer.")
  }
  if (p > length(x_vars)) {
    stop("p should not exceed the number of available predictor variables.")
  }
  if (!is.character(x_vars)) {
    stop("x_vars must be a character vector.")
  }
  if (!is.character(y_var) || length(y_var) != 1) {
    stop("y_var must be a single character string.")
  }
  
  # Generate all combinations and return formula strings
  combn(x_vars, p, FUN = function(vars) {
    paste0(y_var, " ~ ", paste(vars, collapse = " + "))
  })
}
