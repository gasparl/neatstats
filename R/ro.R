#' @title Neat rounding
#'
#' @description Rounds a given number to given number of digits after the
#'   decimal point, returning it as string, with trailing zeros when applicable.
#' @param num Number to be rounded.
#' @param round_to Number of fractional digits (i.e., digits after the decimal
#'   point), to round to.
#' @return Number as string: \code{num} rounded to \code{round_to} digits, with
#'   trailing zeros when applicable.
#' @examples
#' ro( 1.2345 ) # returns "1.23"
#'
#' ro( 0.12345, 1 ) # returns "0.1"
#'
#' ro( 12.3, 4 ) # returns "12.3000"
#' @export
ro = function(num, round_to = 2) {
    if (is.numeric(num)) {
        value = num
    } else {
        value = as.numeric(as.character(num))
    }
    return(format(round(value, round_to), nsmall = round_to))
}
