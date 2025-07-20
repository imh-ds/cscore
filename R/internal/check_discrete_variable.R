#' Heuristically Determine Whether a Variable is Discrete (Internal)
#'
#' Determines whether a vector should be treated as a discrete variable based on
#' its type and number of unique values. Factors, ordered factors, and integers
#' are treated as discrete. Numeric vectors are considered discrete if they have
#' no more than \code{threshold} unique values and all unique values are
#' integer-like (within floating-point tolerance).
#'
#' @param x A vector to be evaluated for discreteness. Typically numeric or
#'   factor-like.
#' @param threshold An integer specifying the maximum number of unique values a
#'   numeric vector can have to still be considered discrete. Defaults to 10.
#'
#' @return Logical scalar indicating whether \code{x} is treated as a discrete
#'   variable.
#'
#' @keywords internal
#' @noRd
is_discrete_variable <- function(x, threshold = 10) {
  
  # If factor or integer, return TRUE for discrete variable
  if (is.factor(x) || is.ordered(x)) return(TRUE)
  if (is.integer(x)) return(TRUE)
  
  # If numeric, check if it's a float or not
  if (is.numeric(x)) {
    x_no_na <- x[!is.na(x)]
    unique_vals <- unique(x_no_na)
    
    if (length(unique_vals) <= threshold) {
      is_integer_like <- all(abs(unique_vals - round(unique_vals)) < .Machine$double.eps^0.5)
      return(is_integer_like)
    }
  }
  return(FALSE)
}