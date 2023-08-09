#' Repeatedly applies function .f on .data using args consecutively
#'
#' @param .data An object to which the function will be applied
#' @param .f The function that will be applied
#' @param args An iterable of arguments that will be passed to the function
#' @return The data after applying the function repeatedly
#' @examples
#' # adds 1 and then 2 to the vector
#' repeat_over_args(1:5, `+`, c(1,2))
#' @export
repeat_over_args <- function(.data, .f, args) {

  data_copy <- .data
  for (arg in args) data_copy <- .f(data_copy, arg)
  data_copy

}


#' Selects the top-left corner of a 2d data object
#'
#' An extension of the head() function, which also selects the n leftmost
#' columns after applying head().
#'
#' @param x A 2-dimensional data object such as data frame, tibble, or matrix
#' @param n Top n rows and columns to select
#' @return The subsetted object
#' @examples
#' ear(mtcars, 5)
#' @export
ear <- function(x, n=10) {

  x[1:n,1:n]

}


