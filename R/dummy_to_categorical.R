#' Convert Dummy Variables to Categorical Variable
#'
#' This function converts a set of dummy (one-hot encoded) variables into a categorical variable.
#' It assigns a category based on the column with the highest value in each row.
#' If all values in a row are zero, it assigns a default value.
#'
#' @param ... A set of logical or binary numeric vectors (or a data frame) representing dummy variables.
#' @param levels A character vector specifying the category levels. Defaults to column names.
#' @param default The value to assign when all dummy variables in a row are zero. Defaults to `NA`.
#'
#' @return A factor variable with levels corresponding to the input categories and the default value.
#'
#' @examples
#' dummy_data <- data.frame(A = c(TRUE, FALSE, FALSE),
#'                          B = c(FALSE, TRUE, FALSE),
#'                          C = c(FALSE, FALSE, TRUE))
#' dummy_to_categorical(dummy_data)
#'
#' dummy_data2 <- data.frame(A = c(1, 0, 0, 0),
#'                           B = c(0, 1, 0, 0),
#'                           C = c(0, 0, 1, 0))
#' dummy_to_categorical(dummy_data2, default = "None")
#'
#' @export
dummy_to_categorical <- function(..., levels = NULL, default = NA) {
  df <- data.frame(...)

  # Handle empty input
  if (ncol(df) == 0) {
    return(factor(character(0), levels = c(levels, default)))
  }

  # Ensure all columns are logical or binary (0/1)
  if (!all(sapply(df, function(x) is.logical(x) || all(x %in% c(0, 1))))) {
    stop("All input variables must be logical (TRUE/FALSE) or binary numeric (0/1).")
  }

  # Ensure that each row has at most one TRUE/1
  if (any(rowSums(df) > 1)) {
    stop("Each row must have at most one TRUE/1 value.")
  }

  if (is.null(levels)) {
    levels <- colnames(df)
  }

  max_indices <- max.col(df, ties.method = "first")
  max_values <- apply(df, 1, max)

  # Assign default where all values are 0, otherwise use levels[max.col()]
  result <- ifelse(max_values == 0, default, levels[max_indices])

  factor(result, levels = c(levels, default))
}
