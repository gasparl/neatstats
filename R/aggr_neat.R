#' @title Aggregation, descriptives
#'
#' @description Returns aggregated values per group for given variable. Serves
#'   as argument in the \code{\link{table_neat}} function.
#' @param dat Data frame (or name of data frame as string).
#' @param values The vector of numbers from which the statistics are to be
#'   calculated, or the name of the column in the \code{dat} data frame, that
#'   contains the vector. (Expression or string are both accepted.)
#' @param method Function of string. If function, uses the \code{values} to
#'   calculate the returned value for the given function (e.g. means, as per
#'   default, using the \code{mean} function). Such a function may return a
#'   vector of results as well; see Examples. If string, one of two internal
#'   functions will be used. If the string end with \code{"+sd"}, e.g.,
#'   \code{"mean+sd"}, the function preceding the \code{"+"} sign will be
#'   calculated along with the standard deviation, displayed in a single column,
#'   rounded as set in the \code{round_to} argument. (This is primarily for use
#'   in the \code{\link{table_neat}} function for summary tables.) If the string
#'   does not end with \code{"+sd"}, a ratio for the occurrences of given
#'   elements will be calculated. Multiple elements can by given as a vector of
#'   strings. The number of occurrences of these elements will be the numerator
#'   (dividend), while the entire column length (i.e., number of all elements)
#'   will be the denominator (divisor). For example, if a column contains
#'   elements \code{"correct"}, \code{"incorrent"}, \code{"tooslow"}, the ratio
#'   of \code{"correct"} to all other elements (i.e., including elements
#'   \code{"correct"}, \code{"incorrent"}, and \code{"tooslow"}) can be written
#'   simply as \code{method = "correct"}. The complementary ratio, of
#'   \code{"incorrent"} and \code{"tooslow"}, can be written as \code{method =
#'   "incorrent, tooslow"}. (Hint: filter to get ratios of subgroups, e.g. to
#'   include only \code{"correct"} and \code{"incorrent"} elements, and
#'   calculate their ratio; see below.)
#' @param group_by String, or vector of strings: the name(s) of the column(s) in
#'   the \code{dat} data frame, containing the vector(s) of factors by which the
#'   statistics are grouped.
#' @param filt An expression to filter, by column values, the entire \code{dat}
#'   data frame before performing the aggregation. The expression should use
#'   column names alone; see Examples.
#' @param prefix \code{NULL} (default) or string. String specifies a prefix for
#'   each group type under the \code{group} column.
#' @param new_name \code{NULL} (default) or string. String specifies new name
#'   for the variable to be used as column title. If \code{NULL}, the name will
#'   be \code{"aggr_value"} (or, if used with \code{\link{table_neat}}, the
#'   input variable name is used).
#' @param round_to Number of digits after the decimal point to round to, when
#'   using \code{"+sd"} in \code{method}.
#' @return A data frame with the statistics per group, with a single column
#'   (\code{"aggr_group"}) indicating the grouping.
#' @seealso \code{\link{table_neat}} to create full tables using multiple
#'   variables
#' @examples
#' data("mtcars") # load base R example dataset
#'
#' # overall means and SDs for wt (Weight)
#' aggr_neat(mtcars, wt)
#'
#' # rename column
#' aggr_neat(mtcars, wt, new_name = 'weight')
#'
#' # grouped by cyl (Number of cylinders)
#' aggr_neat(mtcars, wt, group_by = 'cyl')
#'
#' # grouped by cyl and gear
#' aggr_neat(mtcars, wt, group_by = c('cyl', 'gear'))
#'
#' # prefix for group names
#' aggr_neat(mtcars, wt, group_by = 'cyl', prefix = 'cyl')
#'
#' # filter to only have cyl larger than  4
#' aggr_neat(mtcars, wt, group_by = 'cyl', filt = cyl > 4)
#'
#' # filter to only have hp (Gross horsepower) smaller than  200
#' aggr_neat(mtcars, wt, group_by = 'cyl', filt = hp < 200)
#'
#' # combine two filters above, and add prefix
#' aggr_neat(
#'     mtcars,
#'     wt,
#'     group_by = 'cyl',
#'     filt = (hp < 200 & cyl > 4),
#'     prefix = 'filtered'
#' )
#'
#' # add SD (and round output numbers to 2)
#' aggr_neat(mtcars,
#'           wt,
#'           group_by = 'cyl',
#'           method = 'mean+sd',
#'           round_to = 2)
#'
#' # now medians instead of means
#' aggr_neat(mtcars, wt, group_by = 'cyl', method = median)
#'
#' # with SD
#' aggr_neat(mtcars,
#'           wt,
#'           group_by = 'cyl',
#'           method = 'median+sd',
#'           round_to = 1)
#'
#' # overall ratio of gear 4 (Number of gears)
#' aggr_neat(mtcars, gear, method = '4')
#'
#' # overall ratio of gear 4 and 5
#' aggr_neat(mtcars, gear, method = '4, 5')
#'
#' # same ratio calculated per each cyl
#' aggr_neat(mtcars, gear, group_by = 'cyl', method = '4, 5')
#'
#' # per each cyl and per vs (engine type)
#' aggr_neat(mtcars,
#'           gear,
#'           group_by = c('cyl', 'vs'),
#'           method = '4, 5')
#'
#' # ratio of gear 3 per gear 3 and 5
#' aggr_neat(
#'     mtcars,
#'     gear,
#'     group_by = 'cyl',
#'     method = '3',
#'     filt = gear %in% c(3, 5)
#' )
#'
#' # both mean and median
#' aggr_neat(
#'     mtcars,
#'     gear,
#'     group_by = 'cyl',
#'     method = function(v) {
#'         c(my_mean = mean(v), my_median = median(v))
#'     }
#' )
#'
#' # mean, median, and count
#' aggr_neat(
#'     mtcars,
#'     gear,
#'     group_by = 'cyl',
#'     method = function(v) {
#'         c(mean = mean(v),
#'           median = median(v),
#'           count = length(v))
#'     }
#' )
#' @export

aggr_neat = function(dat,
                     values,
                     method = mean,
                     group_by = NULL,
                     filt = NULL,
                     prefix = NULL,
                     new_name = NULL,
                     round_to = 2) {
    if (typeof(dat) == "character") {
        dat = eval(parse(text = dat))
    }
    validate_args(match.call(),
                  list(
                      val_arg(dat, c('df')),
                      val_arg(method, c('function', 'char'), 1),
                      val_arg(group_by, c('null', 'char')),
                      val_arg(prefix, c('char', 'null'), 1),
                      val_arg(new_name, c('char', 'null'), 1),
                      val_arg(round_to, c('num'), 1)
                  ))
    name_taken('neat_unique_values', dat)
    values = deparse(substitute(values))
    values = trimws(values, whitespace = "['\"]")
    if (values %in% names(dat)) {
        dat$neat_unique_values = dat[[values]]
    } else {
        dat$neat_unique_values = eval(parse(text = values))
    }
    if (anyNA(dat$neat_unique_values)) {
        dat = dat[!is.na(dat$neat_unique_values),]
    }
    filt = deparse(substitute(filt))
    if (filt != "NULL") {
        filt = trimws(filt, whitespace = "['\"]")
        dat = eval(parse(text = paste0(
            'with(data = dat, dat[',
            filt,
            ',])'
        )))
    }
    if (!is.null(pkg.globals$my_unique_method)) {
        method = pkg.globals$my_unique_method
        prefix = NULL
        if (!is.null(pkg.globals$my_unique_grouping_var)) {
            group_by = pkg.globals$my_unique_grouping_var
        } else {
            group_by = NULL
        }
        if (is.null(new_name)) {
            val_name = values
        } else {
            val_name = new_name
        }
    } else {
        if (is.null(new_name)) {
            val_name = 'aggr_value'
        } else {
            val_name = new_name
        }
    }
    if (!is.null(group_by)) {
        group_by = paste(group_by, collapse = ',')
        group_by = eval(parse(text = paste0(
            'with(data = dat, list(',
            group_by,
            '))'
        )))
    } else {
        group_by = list(rep(0, nrow(dat)))
    }
    if (is.function(method) == TRUE) {
        aggred = do.call(data.frame,
                         stats::aggregate(dat$neat_unique_values, by = group_by, FUN = method))
    } else if (endsWith(method, '+sd') == TRUE) {
        func_name = strsplit(method, '+', fixed = TRUE)[[1]][1]
        method = eval(parse(text = func_name))
        aggred = stats::aggregate(dat$neat_unique_values, by = group_by, FUN = method)

        aggred = do.call(data.frame,
                         stats::aggregate(
                             dat$neat_unique_values,
                             by = group_by,
                             FUN = function(x) {
                                 stats::setNames(c(ro(method(x), round_to), ro(stats::sd(x), round_to)), c(func_name, 'sd'))
                             }
                         ))
        aggred[val_name] = paste(aggred[[paste0('x.', func_name)]], aggred$x.sd, sep =
                                     "\u00b1")
        aggred = aggred[, setdiff(names(aggred), c(paste0('x.', func_name), 'x.sd'))]
    } else {
        nume = to_c(method)
        aggred = stats::aggregate(
            dat$neat_unique_values,
            by = group_by,
            FUN = function(x) {
                sum(x %in% nume) / length(x)
            }
        )
    }
    aggred = merge_cols(aggred)
    colnames(aggred)[colnames(aggred) == 'x'] <- val_name
    if (is.null(prefix) != TRUE) {
        aggred$aggr_group = paste(prefix, aggred$aggr_group, sep = "_")
    }
    return(aggred)
}
