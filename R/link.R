#' Create Link Paths Between Node Sets
#'
#' Generates a set of directed paths representing all combinations of links from
#' nodes in `from` to nodes in `to`. The result is a data frame that can be used
#' as input to `composite_model()` and related functions.
#'
#' @param from A character vector of source node names.
#' @param to A character vector of target node names.
#'
#' @return A data frame with two columns, `from` and `to`, containing all
#'   directed edges from each element in `from` to each element in `to`.
#'
#' @examples
#' link(from = c("x1", "x2"), to = c("y"))
#'
#' @export
link <- function(from, to) {
  tidyr::crossing(from = from, to = to)
}