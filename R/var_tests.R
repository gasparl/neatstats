#'@title Variance Equality Tests and Plots
#'
#'@description Performs variance equality tests and creates related plots
#'  (histogram, density, boxplots). This is primarily a subfunction of
#'  \code{\link{anova_neat}}, but here it is available separately for other
#'  potential purposes.
#'@param var Numeric vector; numbers of any given variable.
#'@param group Vector of factors with which to group the \code{var} values.
#'@param median Logical, \code{FALSE} by default. If \code{TRUE}, Levene's test
#'  uses medians instead of means (giving a Brown-Forsythe test). This is a
#'  robust alternative that works better with non-normal (skewed) data.
#'@param plots String: \code{"none"} for no plots, \code{"hist"} for histrogram
#'  and density, \code{"box"} for box plot, and \code{"both"} for both at the
#'  same time.
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'
#'@return Prints test results, and displays plots (and returns them as
#'  \code{\link[ggplot2]{ggplot}} object), if so specified.
#'
#'@note
#'
#'Calculated via...
#'
#'@references
#'
#'Brown, M. B. and Forsythe, A. B. (1974), Journal of the American Statistical
#'Association, 69, pp. 364-367.
#'
#'Levene, H. (1960). Robust tests for equality of variances. In I. Olkin, H.
#'Hotelling, et al. (eds.). Contributions to Probability and Statistics: Essays
#'in Honor of Harold Hotelling. Stanford University Press. pp. 278–292.
#'
#'Fox, J. and Weisberg, S. (2019) An R Companion to Applied Regression, Third
#'Edition, Sage.
#'
#' @seealso \code{\link{t_neat}}
#' @examples
#'
#' # var_tests( ### )
#' # should be equal...
#'
#' @export
var_tests = function(var1,
                      var2 = NULL,
                      pair = FALSE,
                      norm_tests = 'all',
                      alpha = 0.05,
                      hush = FALSE,
                      plots = 'none') {
    validate_args(
        match.call(),
        list(
            val_arg(var1, c('num'), 0),
            val_arg(var2, c('null', 'num')),
            val_arg(pair, c('bool'), 1),
            val_arg(norm_tests, c('char')),
            val_arg(alpha, c('num'), 1),
            val_arg(hush, c('bool'), 1),
            val_arg(plots, c('char'), 1, c('none', 'hist', 'qq', 'both'))
        )
    )
    norm_tests_in(
        var1 = var1,
        var2 = var2,
        pair = pair,
        norm_tests = norm_tests,
        alpha = alpha,
        hush = hush,
        plots = plots,
        tneet = FALSE,
        nonparametric = FALSE
    )
}
norm_tests_in = function(var1,
                         var2,
                         pair,
                         norm_tests,
                         alpha,
                         hush,
                         plots,
                         tneet,
                         nonparametric) {
    norm_tests = tolower(norm_tests)
    norm_outs = c()
    norm_ps = c()
    norm_latent = FALSE
    if (norm_tests  == 'all') {
        norm_tests = c("w", "k2", "a2", "jb")
    } else if (norm_tests  == 'latent')  {
        norm_tests = c("w", "k2", "a2", "jb")
        norm_latent = TRUE
    } else {
        wrongnorm = norm_tests[!(norm_tests %in% c("w", "k2", "a2", "jb"))]
        if (length(wrongnorm) > 0) {
            message(
                'The following "norm_tests" inputs are not correct: "',
                paste(wrongnorm, collapse = '", "'),
                '". Switched to "all".'
            )
            norm_tests = c("w", "k2", "a2", "jb")
        }
    }
    for (norm_abbr in norm_tests) {
        statcomp_num = as.numeric(c(
            'w' = 21,
            'k2' = 6,
            'a2' = 2,
            'jb' = 7
        )[norm_abbr])
        stat_title = as.character(
            c(
                'w' = 'Shapiro-Wilk test: ',
                'k2' = "D'Agostino-Pearson test: ",
                'a2' = "Anderson-Darling test: ",
                'jb' = "Jarque-Bera test: "
            )[norm_abbr]
        )
        if (is.null(var2)) {
            normres = PoweR::statcompute(statcomp_num, var1)
            norm_ps = c(norm_ps, normres$pvalue)
            norm_outs = c(norm_outs,
                          paste0(
                              stat_title,
                              toupper(norm_abbr),
                              " = ",
                              ro(normres$statistic, 2),
                              ", p = ",
                              ro(normres$pvalue, 3)
                          ))
        } else if (pair == TRUE) {
            diff = var1 - var2
            normres = PoweR::statcompute(statcomp_num, diff)
            norm_ps = c(norm_ps, normres$pvalue)
            norm_outs = c(norm_outs,
                          paste0(
                              stat_title,
                              toupper(norm_abbr),
                              " = ",
                              ro(normres$statistic, 2),
                              ", p = ",
                              ro(normres$pvalue, 3)
                          ))
        } else {
            normres1 = PoweR::statcompute(statcomp_num, var1)
            normres2 = PoweR::statcompute(statcomp_num, var2)
            norm_ps = c(norm_ps, normres1$pvalue, normres2$pvalue)
            norm_outs = c(
                norm_outs,
                paste0(
                    stat_title,
                    toupper(norm_abbr),
                    " = ",
                    ro(normres1$statistic, 2),
                    ", p = ",
                    ro(normres1$pvalue, 3),
                    ' (1st var.); ',
                    toupper(norm_abbr),
                    " = ",
                    ro(normres2$statistic, 2),
                    ", p = ",
                    ro(normres2$pvalue, 3),
                    ' (2nd var.)'
                )
            )
        }
    }
    if (tneet == TRUE) {
        if (norm_latent == FALSE |
            (any(norm_ps[!is.na(norm_ps)] < alpha) &
             nonparametric == FALSE)) {
            prnt("--- Normality ---")
            prnt(paste(norm_outs, collapse = '\n'))
            prnt("--- t-test ---")
        }
    } else {
        if (hush == FALSE) {
            prnt(paste(norm_outs, collapse = '\n'))
        }
        if (any(norm_ps[!is.na(norm_ps)] < alpha)) {
            invisible(TRUE)
        } else {
            invisible(FALSE)
        }
    }
}
