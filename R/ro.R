#' Neat rounding
#'
#' This function neatly rounds any number to given number of digits after the decimal point.
#' @examples
#' ro()
#' @export
ro = function(value, round_to = 2) {
    value = as.numeric( value )
    return(format(round(value, round_to), nsmall = round_to))
}