#' @title Neat Descriptives
#'
#' @description Returns means (or medians) and SDs per group for given variable.
#'   Primarily for use in the \code{\link{table_neat}} function.
#' @param values A vector of numbers from which the statistics are to be
#'   calculated.
#' @param round_to Number of digits after the decimal point to round to.
#' @param new_name \code{NULL} (default) or string. String specifies new name
#'   for the variable to be used as column title. If \code{NULL}, the input
#'   variable name is used.
#' @param group_by A vector of factors by which the statistics are grouped.
#' @param medians Logical. If \code{TRUE}, medians are calculated, otherwise
#'   (default: \code{FALSE}) means.
#' @return A data frame with the statistics per group. Furthermore, prints
#'   statistics per group, unless used within the \code{\link{table_neat}}
#'   function, in which case nothing is printed.
#' @seealso \code{\link{table_neat}} to create full tables using multiple
#'   variables
#' @examples
#' data("mtcars") # load base R example dataset
#'
#' res1 = m_neat(mtcars$wt ) # overall means and SDs for wt (Weight)
#'
#' res2 = m_neat(mtcars$wt, 2, new_name = 'weight') # rounded to 2 and renamed
#'
#' res3 = m_neat(mtcars$wt, 2, group_by = mtcars$cyl) # grouped by cyl (Number of cylinders)
#'
#' res4 = m_neat(mtcars$wt, 2, group_by = mtcars$cyl, medians = TRUE) # medians
#' @export

m_neat = function(values,
                  round_to = 0,
                  new_name = NULL,
                  group_by = NULL,
                  medians = FALSE) {
    validate_args(match.call(),
                  list(
                      val_arg(values, c('num'), 0),
                      val_arg(round_to, c('num'), 1),
                      val_arg(new_name, c('char', 'null'), 1),
                      val_arg(medians, c('bool'), 1)
                  ))
    if (is.null(new_name)) {
        val_name = deparse(substitute(values))
        if (grepl('\\$', val_name)) {
            val_name = unlist(strsplit(val_name, "\\$"))[2]
        }
        val_name = unlist(strsplit(val_name, "\\*"))[1]
        val_name = unlist(strsplit(val_name, "/"))[1]
    } else {
        val_name = new_name
    }
    if (!is.null(pkg.globals$my_unique_median)) {
        medians = pkg.globals$my_unique_median
    }
    if (!is.null(pkg.globals$my_unique_grouping_var)) {
        group_by = pkg.globals$my_unique_grouping_var
    }
    if (is.null(group_by)) {
        group_by = rep(0, length(values))
    }
    if (medians == TRUE) {
        per_cond = do.call(data.frame, stats::aggregate(values, by = list(group_by), function(x)
            c(
                median = ro(stats::median(x), round_to),
                sd = ro(stats::sd(x), round_to)
            )))
        per_cond[val_name] = paste(per_cond$x.median, per_cond$x.sd, sep =
                                       "\u00b1")
        per_cond = subset(per_cond, select = -c(x.median, x.sd))
    } else {
        per_cond = do.call(data.frame, stats::aggregate(values, by = list(group_by), function(x)
            c(
                mean = ro(mean(x), round_to),
                sd = ro(stats::sd(x), round_to)
            )))
        per_cond[val_name] = paste(per_cond$x.mean, per_cond$x.sd, sep =
                                       "\u00b1")
        per_cond = subset(per_cond, select = -c(x.mean, x.sd))
    }
    colnames(per_cond)[colnames(per_cond) == 'Group.1'] <- 'group'
    for (i in 1:nrow(per_cond)) {
        row = per_cond[i, ]
        prnt(row[[1]], ': MCHAR_PLUSMINSD = ', row[[2]])
    }
    invisible(per_cond)
}

aggr_neat = function(dat,
                     values,
                     method,
                     group_by = NULL,
                     filt = NULL,
                     new_name = NULL,
                     prefix = NULL) {
    if (class(dat) == "character") {
        dat = eval(parse(text = data_per_subject))
    }
    filt = deparse(substitute(filt))
    if (filt != "NULL") {
        filt = gsub(pattern = "'|\"",
                    replacement = '',
                    x = filt)
        dat = eval(parse(text = paste0('with(data = dat, dat[',
                                       filt,
                                       ',])')))
    }
    values = deparse(substitute(values))
    values = gsub(pattern = "'|\"",
                  replacement = '',
                  x = values)
    group_by = deparse(substitute(group_by))
    if (group_by != "NULL") {
        group_by = gsub(pattern = "'|\"",
                        replacement = '',
                        x = group_by)
        group_by = dat[[group_by]]
    } else {
        group_by = rep(0, nrow(dat))
    }
    if (is.function(method) == TRUE) {
        aggred = aggregate(dat[[values]], by = list(group_by), FUN = method)
    } else {
        if (grepl('/', method, fixed = TRUE) == TRUE) {
            expr = strsplit(method, '/', fixed = TRUE)[[1]]
            if (length(expr) > 2) {
                stop(
                    'The "method" argument must contain',
                    ' no more than one forward slash ("/") character.'
                )
            }
            nume = to_c(expr[1])
            denom = to_c(expr[2])
            aggred = aggregate(
                dat[[values]],
                by = list(group_by),
                FUN = function(x) {
                    sum(x %in% nume) / sum(x %in% denom)
                }
            )
        } else {
            nume = to_c(method)
            aggred = aggregate(
                dat[[values]],
                by = list(group_by),
                FUN = function(x) {
                    sum(x %in% nume) / length(x)
                }
            )
        }
    }
    colnames(aggred)[colnames(aggred) == 'Group.1'] <- 'group'
    if (is.null(prefix) != TRUE) {
        aggred$group = paste(prefix, aggred$group, sep="_")
    }
    return(aggred)
}
