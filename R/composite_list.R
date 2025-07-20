#' Composite List
#'
#' @description Constructs a structured list of named composite variables and 
#' their corresponding indicator items. Composite variables that reference other 
#' composites in their definitions are automatically classified as higher-order 
#' composites and separated accordingly. The function returns a list containing 
#' lower-order composites, higher-order composites, and the declaration order.
#'
#' @param ... Named vectors specifying the indicators for each composite 
#' variable. Higher-order composites should be defined using the names of 
#' previously defined composites.
#'
#' @return A named list with three elements:
#' \itemize{
#'   \item{\code{lower}}{ — a list of lower-order composite variables}
#'   \item{\code{higher}}{ — a list of higher-order composite variables}
#'   \item{\code{order}}{ — a character vector specifying the order of declaration}
#' }
#'
#' @examples
#' data(grit)
#'
#' # Specify the named list with composite names and their respective indicators
#' composite_list <- composite_list(
#'
#'   # Lower-order composites
#'   extraversion          = sprintf("e%01d", seq(10)),
#'   neuroticism           = sprintf("n%01d", seq(10)),
#'   agreeableness         = sprintf("a%01d", seq(10)),
#'   conscientiousness     = sprintf("c%01d", seq(10)),
#'   openness              = sprintf("o%01d", seq(10)),
#'   consistency_interest  = sprintf("gs%01d", c(2,3,5,7,8,11)),
#'   perseverance_effort   = sprintf("gs%01d", c(1,4,6,9,10,12)),
#'
#'   # Higher-order composites
#'   grit                  = c("consistency_interest", "perseverance_effort")
#'
#'  )
#'
#' @export
composite_list <- function(...){
  
  # Create a list from the arguments
  composites <- list(...)
  
  # Detect higher order variables in the composite list
  higher_order_variables <- detect_higher_order_variable(composites)
  
  # Get only lower order variables
  lower_order_varlist <- remove_higher_order_variable(composites,
                                                      higher_order_variables)
  
  # Get only higher order variables
  higher_order_varlist <- keep_higher_order_variable(composites,
                                                     higher_order_variables)
  
  # Check the length of higher order vectors
  higher_order_length_check <- any(sapply(higher_order_varlist, length) == 1)
  
  # Flag and provide error if length == 1
  if(isTRUE(higher_order_length_check)) {
    stop("Error: Higher order variables cannot be composed of a single indicator.")
  }
  
  # Composite variable order
  composite_order <- names(composites)
  
  # Return
  order_list <- list(lower = lower_order_varlist,
                     higher = higher_order_varlist,
                     order = composite_order)
  
  return(order_list)
  
}

#' @rdname composite_list
#' @export
clist <- composite_list

#' @rdname composite_list
#' @export
cl <- composite_list