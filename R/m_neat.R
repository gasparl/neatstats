#' @title Neat Descriptives
#'
#' @description Returns means (or medians) and SDs per group for given variable.
#'   Primarily for use in the \code{\link{table_neat}} function.
#' @param values A vector of numbers from which the statistics are to be
#'   calculated.
#' @param round_to Number of digits after the decimal point to round to.
#' @param new_name String. A new name for the variable to be used as column
#'   title.
#' @param group_by A vector of factors by which the statistics are grouped.
#' @param medians Logical. If \code{TRUE}, medians are calculated, otherwise
#'   means.
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
