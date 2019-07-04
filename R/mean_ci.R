#'@title Confidence Interval of Mean
#'
#'@description Calculates confidence interval of a vector of numbers.
#'@param x Numeric vector.
#'@param ci Numeric; confidence level for returned CI.
#'@return Confidence interval (as named vector).
#' @seealso \code{\link{se}}
#' @examples
#' mean_ci( c(11, 15, 19, 43, 53, -4, 34, 8, 33, -1, 54 ) )
#' mean_ci( c(11, 15, 19, 43, 53, -4, 34, 8, 33, -1, 54 ), ci = .80 )
#'
#' @export
mean_ci = function(x, ci = 0.95) {
    m_c = mean(x)
    se_c = se(x)
    z_c = stats::qnorm(1 - (1 - ci) / 2)
    low = (m_c - (z_c * se_c))
    upp = (m_c + (z_c * se_c))
    return( c(lower = low, upper = upp) )
}