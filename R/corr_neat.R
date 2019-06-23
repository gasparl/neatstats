#'@title Correlation Statistics
#'
#'@description \code{\link[stats:cor.test]{Pearson correlation}} results
#'  including confidence interval (CI) and correlation
#'  \code{\link[BayesFactor:correlationBF]{Bayes factor}} (BF).
#'@param var1 Numeric vector; numbers of the first variable.
#'@param var2 Numeric vector; numbers of the second variable.
#'@param ci Numeric; confidence level for the returned CI, as implemented in
#'  \code{\link[stats]{cor.test}}.
#'@param bf_added Logical. If \code{TRUE} (default), Bayes factor is calculated
#'  and displayed.
#'@param direction String; optionally specifies one-sided test: either
#'  "negative" (negative correlation expected) or "positive" (positive
#'  correlation expected). (Short forms also work, e.g. "p", "pos", "neg", etc.)
#'  If left empty, the test is two-sided.
#'@param round_r Number \code{\link[=ro]{to round}} to the correlation and its
#'  CI.
#'@param for_table Logical. If \code{TRUE}, omits the confidence level display
#'  from the printed text.
#'@details
#' The Bayes factor (BF) is always calculated with the default r-scale of
#' \code{0.707}. BF supporting null hypothesis is denoted as BF01, while that
#' supporting alternative hypothesis is denoted as BF10. When the BF is smaller
#' than 1 (i.e., supports null hypothesis), the reciprocal is calculated (hence,
#' BF10 = BF, but BF01 = 1/BF). When the BF is greater than or equal to 10000,
#' scientific (exponential) form is reported for readability. (The original full
#' BF number is available in the returned named vector as \code{bf}.)
#'
#'@return Prints correlation statistics (including CI and BF) in APA style.
#'  Furthermore, when assigned, returns a named vector with the following
#'  elements: \code{r} (Pearson correlation), \code{p} (p value), \code{bf}
#'  (Bayes factor).
#'@note The correlation and CI is calculated via
#'\code{\link[stats:cor.test]{stats::cor.test}}.
#'
#'The Bayes factor is calculated via
#'\code{\link[BayesFactor:correlationBF]{BayesFactor::correlationBF}}.
#'
#' @seealso \code{\link{t_neat}}
#' @examples
#' # assign two variables
#' v1 = c(11, 15, 19, 43, 53, -4, 34, 8, 33, -1, 54 )
#' v2 = c(4, -2, 23, 13, 32, 16, 3, 29, 37, -4, 65 )
#'
#' corr_neat(v1, v2) # prints statistics
#'
#' # one-sided, and omitting the "95% CI" part
#' corr_neat(v1, v2, direction = 'pos', for_table = TRUE)
#'
#' # print statistics and assign main results
#' results = corr_neat(v1, v2, direction = 'pos')
#'
#' results['p'] # get precise p value
#' @export
corr_neat = function(var1,
                     var2,
                     ci = .95,
                     bf_added = TRUE,
                     direction = "",
                     round_r = 3,
                     for_table = FALSE) {
    if (direction != "" &&
        substr("negative", 1, nchar(direction)) == direction) {
        message("One-sided test! Negative correlation expected.")
        the_cor = stats::cor.test(var1,
                           var2,
                           alternative = "l",
                           conf.level = ci)
        if (bf_added == TRUE) {
            bf = as.vector(BayesFactor::correlationBF(var1, var2, nullInterval = c(-1, 0))[1])
        }
    } else if (direction != "" &
               substr("positive", 1, nchar(direction)) == direction) {
        message("One-sided test! Positive correlation expected.")
        the_cor = stats::cor.test(var1,
                           var2,
                           alternative = "g",
                           conf.level = ci)
        if (bf_added == TRUE) {
            bf = as.vector(BayesFactor::correlationBF(var1, var2, nullInterval = c(0, 1))[1])
        }
    } else {
        the_cor = stats::cor.test(var1, var2, conf.level = ci)
        if (bf_added == TRUE) {
            bf = as.vector(BayesFactor::correlationBF(var1, var2))
        }
    }
    if (bf_added == TRUE) {
        bf_out = bf_neat(bf)
    } else {
        bf_out = "."
        bf = NA
    }
    if (for_table == TRUE) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
    }
    r = edges(the_cor$estimate, round_r, no_null = TRUE)
    lower = edges(the_cor$conf.int[1], round_r, no_null = TRUE)
    upper = edges(the_cor$conf.int[2], round_r, no_null = TRUE)
    p_value = the_cor$p.value
    df = the_cor$parameter
    out = paste0("r(",
                 df,
                 ") = ",
                 r,
                 ci_disp,
                 " [",
                 lower,
                 ", ",
                 upper,
                 "]",
                 ", p = ",
                 ro(p_value, 3),
                 bf_out)
    prnt(out)
    invisible(c(
        r = as.numeric(the_cor$estimate),
        p = p_value,
        bf = as.numeric(bf)
    ))
}
