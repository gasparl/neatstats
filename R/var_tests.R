#'@title Variance Equality Tests and Plots
#'
#'@description Performs variance Brown-Forsythe and Fligner-Killeen equality
#'  tests (tests of homogeneity of variances) and creates related plots
#'  (histogram, density, boxplots). This is primarily a subfunction of
#'  \code{\link{anova_neat}}, but here it is available separately for other
#'  potential purposes.
#'@param xvar Either a numeric vector (numbers of any given variable), or, if
#'  \code{dat} is given, a column name specifying the variable in the given data
#'  frame.
#'@param group_by Either a vector of factors with which to group the \code{xvar}
#'  values, or, if \code{dat} is given, one or more column names specifying the
#'  columns in the given data frame.
#'@param dat Either \code{NULL} or a data frame from which the respective column
#'  names should be selected for \code{xvar} and \code{group}.
#'@param plots String: \code{"none"} for no plots, \code{"hist"} for histrogram
#'  and density, \code{"box"} for box plot, and \code{"both"} for both at the
#'  same time.
#'@param sep String (underscore \code{"_"} by default) for separating group
#'  names, for plot display.
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'
#'@return Prints test results, and displays plots (and returns them as
#'  \code{\link[ggplot2]{ggplot}} object), if so specified.
#'
#'@note
#'
#' Brown-Forsythe test (i.e., Levene's test using medians) is calculated via
#' \code{\link[car:leveneTest]{car::leveneTest}}. Fligner-Killeen test, which
#' may be more robust (i.e., less affected by non-normal distribution), is
#' calculated via \code{\link[stats:fligner.test]{stats::fligner.test}}. (See
#' also Conover et al., 1981, p. 360.)
#'
#'@references
#'
#'Brown, M. B. & Forsythe, A. B. (1974), Journal of the American Statistical
#'Association, 69, pp. 364-367.
#'
#'Conover W. J., Johnson M. E., & Johnson M. M. (1981) A comparative study of
#'tests for homogeneity of variances, with applications to the outer continental
#'shelf bidding data. Technometrics, 23, 351–361.
#'
#'Fligner, M. A. & Killeen, T. J. (1976). Distribution-free two-sample tests for
#'scale. ‘Journal of the American Statistical Association.’ 71(353), 210-213.
#'
#'Fox, J. & Weisberg, S. (2019) An R Companion to Applied Regression, Third
#'Edition, Sage.
#'
#'Levene, H. (1960). Robust tests for equality of variances. In I. Olkin, H.
#'Hotelling, et al. (eds.). Contributions to Probability and Statistics: Essays
#'in Honor of Harold Hotelling. Stanford University Press. pp. 278–292.
#'
#' @seealso \code{\link{anova_neat}}
#' @examples
#'
#' # the statistics of the four functions below should match
#' var_tests(Moore$conformity, Moore$fcategory)
#' var_tests('conformity', 'fcategory', Moore)
#' car::leveneTest(conformity ~ fcategory, data = Moore)
#' stats::fligner.test(conformity ~ fcategory, Moore)
#'
#' # again the results below should match each other
#' var_tests(Moore$conformity,
#'           interaction(Moore$fcategory, Moore$partner.status))
#' var_tests('conformity', c('fcategory', 'partner.status'), Moore)
#' car::leveneTest(conformity ~ fcategory * partner.status, data = Moore)
#' stats::fligner.test(conformity ~ interaction(fcategory, partner.status), Moore)
#'
#' @export
var_tests = function(xvar,
                     group_by,
                     dat = NULL,
                     hush = FALSE,
                     sep = ', ',
                     plots = 'none') {
    if (typeof(dat) == "character") {
        dat = eval(parse(text = dat))
    }
    validate_args(match.call(),
                  list(val_arg(xvar, c('num', 'char')),
                       val_arg(dat, c('null', 'df'), 1),
                       val_arg(
                           plots, c('char'), 1, c('none', 'hist', 'qq', 'both')
                       )))
    if (!is.null(dat)) {
        if (typeof(xvar) == 'character') {
            checkcol(names(dat), xvar)
            xvar = dat[[xvar]]
        }
        if (typeof(group_by) == 'character') {
            group_by = eval(parse(
                text = paste(
                    'with(data = dat, paste(',
                    paste(group_by, collapse = ','),
                    ", sep = '",
                    sep,
                    "'))"
                )
            ))
        }
    }
    group_by = as.factor(as.character(group_by))
    lev_med = car::leveneTest(y = xvar, group = group_by)
    fk_med = stats::fligner.test(x = xvar, g = group_by)
    prnt(
        "Brown-Forsythe: F(",
        lev_med$Df[1],
        ",",
        lev_med$Df[2],
        ")",
        " = ",
        ro(lev_med$`F value`[1], 2),
        ", p = ",
        ro(lev_med$`Pr(>F)`[1], 3),
        "; Fligner-Killeen: X2(",
        fk_med$parameter,
        ")",
        " = ",
        ro(fk_med$statistic, 3),
        ", p = ",
        ro(fk_med$p.value, 3),
        '.'
    )
}
