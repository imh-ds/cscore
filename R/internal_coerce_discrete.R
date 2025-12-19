#' Coerce Numeric Vector to Discrete If Integer-Like (Internal)
#'
#' If the input is numeric and contains a sufficiently small number of unique
#' values (less than or equal to \code{threshold}), this function rounds values
#' to the nearest integer—effectively treating the vector as discrete.
#' Non-numeric inputs are returned unchanged.
#'
#' @param x A vector, typically numeric. Non-numeric vectors are returned as-is.
#' @param threshold An integer specifying the maximum number of unique values to
#'   consider the input as discrete. Defaults to 10.
#'
#' @return A vector of the same length as \code{x}, possibly rounded if numeric
#'   and considered discrete.
#'
#' @keywords internal
coerce_to_discrete_if_integer_like <- function(x, threshold = 10) {
  if (!is.numeric(x)) return(x)
  
  x_no_na <- x[!is.na(x)]
  unique_vals <- unique(x_no_na)
  
  if (length(unique_vals) <= threshold) {
    if (any(abs(unique_vals - round(unique_vals)) > .Machine$double.eps^0.5)) {
      # Contains decimals — round them
      x <- round(x)
    }
  }
  return(x)
}