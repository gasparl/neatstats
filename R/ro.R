#' @title Neat rounding
#'
#' @description Rounds a given number to given number of digits after the
#'   decimal point, returning it as string, with trailing zeros when applicable.
#' @param num Number to be rounded.
#' @param round_to Number of fractional digits (i.e., digits after the decimal
#'   point), to round to.
#' @param leading_zero Logical, \code{TRUE} by default. If \code{FALSE}, omits
#'   leadig zero (e.g. returns ".17" instead of "0.17").
#' @return Number as string: \code{num} rounded to \code{round_to} digits, with
#'   trailing zeros when applicable.
#' @examples
#' ro( 1.2345 ) # returns "1.23"
#'
#' ro( 0.12345, 1 ) # returns "0.1"
#'
#' ro( 12.3, 4 ) # returns "12.3000"
#' @export
ro = function(num,
              round_to = 2,
              leading_zero = TRUE) {
    validate_args(match.call(),
                  list(val_arg(leading_zero, c('bool'), 1)))
    if (is.numeric(num)) {
        value = num
    } else {
        value = as.numeric(as.character(num))
    }
    formtd = format(round(value, round_to), nsmall = round_to)
    if (leading_zero == FALSE) {
        formtd = sub("0.", ".", formtd, fixed = TRUE)
    }
    return(formtd)
}
