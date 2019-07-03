#' @title Table, descriptives
#'
#' @description Creates a neat means (or similar descriptives) and standard
#'   deviations table, using \code{\link{aggr_neat}} functions as arguments.
#'   Alternatively, simply transposes data frames using first column as headers.
#' @param values_list Data frames as returned from the \code{\link{aggr_neat}}
#'   function: variables from which the statistics for the table are to be
#'   calculated. The \code{group_by}, \code{method}, and \code{prefix}
#'   parameters are ignored when they are given in the \code{\link{table_neat}}
#'   function; see Details.
#' @param group_by A vector of factors by which the statistics are grouped.
#'   (Overwrites \code{group_by} in \code{\link{aggr_neat}}; see Details.)
#' @param group_per String, "rows" or "columns". If set to "columns" (or just
#'   "c" or "col", etc.), each column contains statistics for one group.
#'   Otherwise (default), each row contains statistics for one group.
#' @param to_clipboard Logical. If \code{TRUE}, the table is copied to the
#'   clipboard (default: \code{FALSE}).
#' @param method Function or string; overwrites the \code{method} argument in
#'   \code{\link{aggr_neat}} when used within this function. See \code{method}
#'   in the \code{\link{aggr_neat}} function for details. Default value:
#'   \code{"mean+sd"} (to calculate means and standard deviations table).
#' @param transpose Logical (default: \code{FALSE}) or string. If \code{TRUE} or
#'   string, ignores all other parameters (except \code{values_list}), but
#'   merges the given list of data frames (as returned from the
#'   \code{\link{aggr_neat}}) and then transposes them using, by default, the
#'   \code{"aggr_group"} column values for new headers (corresponding to the
#'   output of \code{\link{aggr_neat}}; see Examples). However, a string given
#'   as agrument for the \code{transpose} parameter can also specify a custom
#'   column name.
#' @details The \code{values}, \code{round_to}, and \code{new_name} arguments
#'   given in the \code{\link{aggr_neat}} function are always applied. However,
#'   the \code{prefix} parameter will be overwritten as \code{NULL}. If
#'   \code{new_name} in \code{\link{aggr_neat}} is \code{NULL}, the given input
#'   variable names will be used instead of \code{"aggr_value"}. Furthermore,
#'   the \code{group_by} or \code{method} given in the \code{\link{aggr_neat}}
#'   function are only applied when no arguments are given in the
#'   \code{\link{table_neat}} function for the identical parameters
#'   (\code{group_by} or \code{medians}). If either parameter is given in the
#'   \code{\link{table_neat}} function, all separately given respective
#'   argument(s) in the \code{\link{aggr_neat}} function(s) are ignored.
#' @return Returns a data frame with means or medians and SDs per variable and
#'   per group.
#' @seealso \code{\link{aggr_neat}} for more related details
#' @examples
#' data("mtcars") # load base R example dataset
#'
#' # overall means and SDs table for disp (Displacement) and hp (Gross horsepower)
#' table_neat(list(aggr_neat(mtcars, disp),
#'                 aggr_neat(mtcars, hp)))
#'
#' # means and SDs table for mpg (Miles/(US) gallon), wt (Weight), and hp (Gross horsepower)
#' # grouped by cyl (Number of cylinders)
#' # each measure rounded to respective optimal number of digits
#' # wt renamed to weight (for the column title)
#' table_neat(list(
#'     aggr_neat(mtcars, mpg, round_to = 1),
#'     aggr_neat(mtcars, wt, new_name = 'weight', round_to = 2),
#'     aggr_neat(mtcars, hp, round_to = 0)
#' ),
#' group_by = cyl)
#'
#' # same as above, but with medians, and with groups per columns
#' table_neat(
#'     list(
#'         aggr_neat(mtcars, mpg, round_to = 1),
#'         aggr_neat(mtcars, wt, new_name = 'weight', round_to = 2),
#'         aggr_neat(mtcars, hp, round_to = 0)
#'     ),
#'     group_by = cyl,
#'     method = 'median+sd',
#'     group_per = 'columns'
#' )
#'
#' ###
#'
#' # the long example below illustrates collecting and aggregating data from
#' # participants, using the table_neat transpose == TRUE option
#'
#' # for a more complete version of this example, see the README file at the
#' # repository: https://github.com/gasparl/neatstats
#'
#' # you can completely ignore the following function
#' # it serves merely to simulate example data
#' next_subject = function(sub_num) {
#'     N = 150
#'     sub_dat = data.frame(
#'         subject_num = toString(sub_num),
#'         condition = sample(c('fullvision', 'colorblind'), 1),
#'         rt = stats::rnorm(n = N, mean = 400, sd = 150),
#'         response = sample(
#'             c(rep('correct', 9), 'incorrect', 'tooslow'),
#'             size = N,
#'             replace = TRUE
#'         ),
#'         color = sample(c('red', 'green'), size = N, replace = TRUE),
#'         valence = sample(
#'             c('positive', 'negative'),
#'             size = N,
#'             replace = TRUE
#'         )
#'     )
#'     if (sub_dat$condition[1] == 'fullvision') {
#'         green_neg = (sub_dat$color == 'green' &
#'                          sub_dat$valence == 'negative')
#'         sub_dat$rt[green_neg] = sub_dat$rt[green_neg] + stats::rnorm(
#'                                      n = length(sub_dat$rt[green_neg]),
#'                                      mean = 43,
#'                                      sd = 30)
#'         sub_dat$response[green_neg] = sample(
#'             c(rep('correct', 7), 'incorrect', 'tooslow'),
#'             size = length(sub_dat$response[green_neg]),
#'             replace = TRUE
#'         )
#'         red_pos = (sub_dat$color == 'red' &
#'                        sub_dat$valence == 'positive')
#'         sub_dat$rt[red_pos] = sub_dat$rt[red_pos] + stats::rnorm(n = length(sub_dat$rt[red_pos]),
#'                                                                  mean = 37,
#'                                                                  sd = 30)
#'         sub_dat$response[red_pos] = sample(
#'             c(rep('correct', 7), 'incorrect', 'tooslow'),
#'             size = length(sub_dat$response[red_pos]),
#'             replace = TRUE
#'         )
#'     }
#'     neg = (sub_dat$valence == 'negative')
#'     sub_dat$rt[neg] = sub_dat$rt[neg] + stats::rnorm(n = length(sub_dat$rt[neg]),
#'                                                      mean = 40,
#'                                                      sd = 25)
#'     sub_dat$response[neg] = sample(
#'         c(rep('correct', 6), 'incorrect', 'tooslow'),
#'         size = length(sub_dat$response[neg]),
#'         replace = TRUE
#'     )
#'     return(sub_dat)
#' }
#' # the variable below simulates file names
#' filenames = 1:5 # (just 5 sets for this example)
#'
#' # in brief, each data file (as data frame) contains:
#' # subject_num: the number (id) of subject
#' # condition: "fullvision" vs "colorblind"
#' # rt: Response Times
#' # response: "correct", "incorrect", or "tooslow"
#' # color: "green" vs "red"
#' # valence: "positive" vs "negative"
#' # see e.g.
#' head(next_subject(1)) # (here the function gives the data)
#'
#' # we want to aggregate this, per subject, to get the following RT values:
#' # rt_green_negative rt_green_positive rt_red_negative rt_red_positive
#' # and also Error Rates (correct/(correct+incorrect)):
#' # er_green_negative er_green_positive er_red_negative er_red_positive
#'
#' # for this, loop through all data files
#' for (file_name in filenames) {
#'     subject_data = next_subject(file_name)
#'     # with real data, this would be e.g.:
#'     # subject_data = read.table(file_name, stringsAsFactors=F, fill=T, header=T)
#'
#'     # print current file name - just to monitor the process
#'     cat(file_name, ' ')
#'
#'     # now aggregate rt data per type
#'     rts = aggr_neat(
#'         subject_data,
#'         rt,
#'         group_by = 'color, valence',
#'         method = mean,
#'         prefix = 'rt'
#'     )
#'
#'     # same with error rates
#'     ers = aggr_neat(
#'         subject_data,
#'         response,
#'         group_by = 'color, valence',
#'         method = 'incorrect',
#'         prefix = 'er',
#'         filt = (response %in% c('correct', 'incorrect'))
#'     )
#'
#'     # merge and transpose here to get the subject's data in one line
#'     subject_line = table_neat(list(rts, ers), transpose = TRUE)
#'
#'     # add the subject_id and condition to the beginning
#'     subject_line = data.frame(
#'         subject_id = subject_data$subject_num[1],
#'         condition = subject_data$condition[1],
#'         subject_line
#'     )
#'
#'     # merge aggregated subject data
#'     if (!exists("subjects_merged")) {
#'         # if doesn't yet exist, create first line
#'         subjects_merged = subject_line
#'     } else {
#'         # if exists, add the next lines
#'         subjects_merged = rbind(subjects_merged, subject_line)
#'     }
#' }
#' # the data, with single rows per subject, is ready for analysis:
#' head(subjects_merged)
#'
#' @export

table_neat = function(values_list,
                      group_by = NULL,
                      group_per = 'rows',
                      to_clipboard = FALSE,
                      method = 'mean+sd',
                      transpose = FALSE) {
    if (transpose != FALSE) {
        validate_args(match.call(),
                      list(val_arg(values_list, c(
                          'list', 'df'
                      )),
                      val_arg(transpose, c(
                          'bool', 'char'
                      ), 1)))
        return(transp(values_list, transpose))
    }
    group_by = deparse(substitute(group_by))
    tryCatch({
        pkg.globals$my_unique_grouping_var = group_by
        pkg.globals$my_unique_method = method
        validate_args(match.call(),
                      list(
                          val_arg(values_list, c('list')),
                          val_arg(group_per, c('char'), 1),
                          val_arg(to_clipboard, c('bool'), 1),
                          val_arg(method, c('function', 'char'), 1)
                      ))
        the_table = Reduce(function(x, y)
            merge(x, y, by = "aggr_group", all = TRUE),
            values_list)
    },
    error = function(error_message) {
        message(error_message)
        pkg.globals$my_unique_grouping_var = NULL
        pkg.globals$my_unique_method = NULL
        return(NA)
    })
    pkg.globals$my_unique_grouping_var = NULL
    pkg.globals$my_unique_method = NULL
    if (group_per != "" &&
        substr("columns", 1, nchar(group_per)) == group_per) {
        the_table = t(data.frame(the_table, row.names =  1))
        the_table = data.frame(variables = row.names(the_table), the_table)
        row.names(the_table) = 1:nrow(the_table)
    }
    if (to_clipboard == TRUE) {
        utils::write.table(
            the_table,
            "clipboard",
            sep = "\t",
            quote = FALSE,
            row.names = FALSE
        )
        message('Table copied to Clipboard.')
    }
    return(the_table)
}
