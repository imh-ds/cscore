#' Detect Higher Order Constructs
#'
#' @description Takes a composite list as input and returns a character vector
#'   containing the names of all composite variable vectors that are considered
#'   'higher order' constructs/variables. A construct is considered at the
#'   'higher order' if it is a composite of other composite scores that
#'   represent different dimensions. This function automatically detects higher
#'   order constructs as any vector containing one or more of the names of other
#'   composite vectors in the list.
#'
#' @param composite_list A required named list of vectors. Each name in the list
#'   represents a composite variable, and the corresponding vector contains the
#'   column names that are associated with the indicators comprising said
#'   composite variable.
#'
#' @return A vector of higher order construct/variable names. The function uses
#'   a for loop to iterate over each named vector in the list and checks if any
#'   of the values in the vector match the names of the vectors in the list. If
#'   a match is found, the name of that vector is added to the
#'   \code{higher_order_variables} vector. The function then returns
#'   \code{higher_order_variables.}
#'
#' @seealso [remove_higher_order_variable(), keep_higher_order_variable()]
#'
#' @keywords internal
#' @noRd
detect_higher_order_variable <- function(composite_list){
  
  # Initialize an empty character vector to store the names of higher order variables
  higher_order_variables <- character(0)
  
  # Loop over each named vector in the list
  for (name in names(composite_list)) {
    
    # Check if any of the values in the current vector match the names of the vectors in the list
    if (any(names(composite_list) %in% composite_list[[name]])) {
      
      # If a match is found, add the name of the current vector to the higher_order_variables vector
      higher_order_variables <- c(higher_order_variables, name)
      
    }
    
  }
  
  # Return the names of all vectors that are considered "higher order variables"
  return(higher_order_variables)
  
}

