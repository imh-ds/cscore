#' Remove Higher Order Constructs from Composite List
#'
#' @description Takes a composite list as input and removes vectors that match
#'   higher order constructs/variables.
#'
#' @param composite_list A required named list of vectors. Each name in the list
#'   represents a composite variable, and the corresponding vector contains the
#'   column names that are associated with the indicators comprising said
#'   composite variable.
#' @param higher_order_variables A vector containing the names of higher order
#'   variables.
#'
#' @return The function returns a new list that is a subset of
#'   \code{composite_list}, excluding any vectors whose names are in
#'   \code{higher_order_variables.}
#'
#' @seealso [detect_higher_order_variable(), keep_higher_order_variable()]
#'
#' @noRd
remove_higher_order_variable <- function(
    composite_list,
    higher_order_variables
) {
  
  # Subset the list to include only those elements whose names are not in
  # the higher_order_variables vector. This effectively removes the higher
  # order variables from the list.
  composite_list[!(names(composite_list) %in% higher_order_variables)]
  
}