#' @importFrom tibble tibble
NULL

#' Baby names.
#'
#' Full baby name data provided by the SSA. This includes all names with at
#' least 5 uses per year per state. If a state has less than 5 but more than 0
#' uses in a given year, the total across all states for that year will be
#' less than the national total given in the \code{babynames} package.
#'
#' @format A data frame with five variables: \code{state}, \code{sex},
#'   \code{year}, \code{name} and \code{n}.
"statebabynames"
