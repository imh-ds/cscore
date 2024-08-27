#' Composite List
#'
#' @description A duplicate of base R's \code{list} function. Create a list of
#'   composite variable named vectors by specifying their corresponding
#'   indicator items. Composite variables with indicators comprising the names
#'   of other composite variables are automatically categorized as higher-order
#'   composites. These are separated into its own \code{higher} list of
#'   composite vectors. The remaining composite variables are allocated into the
#'   \code{lower} list of composite vectors.
#'
#' @param ... Required named objects. Each vector should include the columns
#'   representing the indicator variables.
#'
#' @return Two lists of lower and higher-order named composite vectors.
#'
#' @examples
#'
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