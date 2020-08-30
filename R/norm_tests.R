#'@title Normality Tests and Plots
#'
#'@description Performs normality tests and creates related plots (histogramm,
#'  density, Q-Q). This is primarily a subfunction of \code{\link{t_neat}}, but
#'  here it is available separately for other potential purposes.
#'@param var1 Numeric vector; numbers of any given variable.
#'@param var2 Optional numeric vector (or \code{NULL}); numbers of a second
#'  variable.
#'@param pair Logical; only matters if \code{var2} is not null. In that case, if
#'  \code{TRUE} each normality test is performed for the difference values
#'  between the two variables in case of paired samples, or, if \code{FALSE},
#'  separately for each of the two variables for unpaired samples.
#'@param norm_tests Normality tests. Any or all of the following character input
#'  is accepted (as a single string or a character vector; case-insensitive):
#'  \code{"W"} (Shapiro-Wilk), \code{"K2"} (D'Agostino-Pearson), \code{"A2"}
#'  (Anderson-Darling), \code{"JB"} (Jarque-Bera); see Notes. The option
#'  \code{"all"} (default value) selects all four previous tests at the same
#'  time.
#'@param plots Logical. If \code{TRUE}, creates a density plot (i.e.,
#'  \code{\link[stats:density]{Gaussian kernel density estimates}}) from the two
#'  variables.
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'
#'@return Prints normality tests, and displays plots (and returns them as
#'  \code{\link[ggplot2]{ggplot}} object), if so specified. If no plots are
#'  created, returns \code{TRUE} if any of the specified tests has p value below
#'  the specified \code{alpha}, otherwise returns \code{FALSE}.
#'
#'@note
#'
#'#'Normality tests are all calculated via
#'\code{\link[PoweR:statcompute]{PoweR::statcompute}}, selected based on the
#'recommendation of Lakens (2015), quoting Yap and Sim (2011, p. 2153): "If the
#'distribution is symmetric with low kurtosis values (i.e. symmetric
#'short-tailed distribution), then the D'Agostino-Pearson and Shapiro-Wilkes
#'tests have good power. For symmetric distribution with high sample kurtosis
#'(symmetric long-tailed), the researcher can use the JB, Shapiro-Wilkes, or
#'Anderson-Darling test." See url{https://github.com/Lakens/perfect-t-test} for
#'more details.
#'
#'@references
#'
#'Lakens, D. (2015). The perfect t-test (version 1.0.0). Retrieved from
#'https://github.com/Lakens/perfect-t-test.
#'\doi{https://doi.org/10.5281/zenodo.17603}
#'
#'Yap, B. W., & Sim, C. H. (2011). Comparisons of various types of normality
#'tests. Journal of Statistical Computation and Simulation, 81(12), 2141–2155.
#'\doi{https://doi.org/10.1080/00949655.2010.520163}
#'
#' @seealso \code{\link{t_neat}}
#' @examples
#'
#' norm_tests(stats::rnorm(100))
#' # should be normal...
#'
#' @export
norm_tests = function(var1,
                      var2 = NULL,
                      pair = TRUE,
                      norm_tests = 'all',
                      alpha = 0.05,
                      hush = FALSE,
                      plots = 'none') {
    validate_args(
        match.call(),
        list(
            val_arg(var1, c('num'), 0),
            val_arg(var2, c('num'), 0),
            val_arg(pair, c('bool'), 1),
            val_arg(norm_tests, c('char')),
            val_arg(alpha, c('num'), 1),
            val_arg(hush, c('bool'), 1),
            val_arg(plots, c('char'), 1, c('none', 'hist', 'qq', 'both')),
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
        if (pair == TRUE) {
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
