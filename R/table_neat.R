#' @title Neat Table
#'
#' @description Creates a neat means (or medians) and standard deviations table, using
#' m_neat() function(s) as arguments.
#' @param values_list Values returned (as data frames) from the
#'   \code{\link{m_neat}} function: variables from which the statistics for the
#'   table are to be calculated. The \code{group_by} and \code{medians}
#'   parameters are ignored when they are given in the \code{\link{table_neat}}
#'   function; see Details.
#' @param group_by A vector of factors by which the statistics are grouped.
#'   (Overwrites \code{group_by} in \code{\link{m_neat}}; see Details.)
#' @param group_per String, "rows" or "columns". If set to "columns" (or just
#'   "c" or "col", etc.), each column contains statistics for one group.
#'   Otherwise (default), each row contains statistics for one group.
#' @param to_clipboard Logical. If TRUE, the table is copied to the clipboard.
#' @param medians Logical. If TRUE, medians are calculated, otherwise means.
#'    (Overwrites \code{medians} in \code{\link{m_neat}}; see Details.)
#' @return Returns a data frame with means or medians and SDs per variable and
#'   per group.
#' @examples
#' data("mtcars") # load base R example dataset
#' 
#' # overall means and SDs table for disp (Displacement) and hp (Gross horsepower)
#' Ms_SDs = table_neat(list(m_neat(mtcars$disp),
#'                          m_neat(mtcars$hp)))
#' 
#' # means and SDs table for mpg (Miles/(US) gallon), wt (Weight), and hp (Gross horsepower)
#' # grouped by cyl (Number of cylinders)
#' # each measure rounded to respective optimal number of digits
#' # wt renamed to weight (for the column title)
#' Ms_SDs2 = table_neat(list(m_neat(mtcars$mpg, 1),
#'                           m_neat(mtcars$wt, 2, new_name = 'weight'),
#'                           m_neat(mtcars$hp)),
#'                      group_by = mtcars$cyl)
#' 
#' # same as above, but with medians, and with groups per columns
#' Ms_SDs3 = table_neat(list(m_neat(mtcars$mpg, 1),
#'                           m_neat(mtcars$wt, 2, new_name = 'weight'),
#'                           m_neat(mtcars$hp)),
#'                      group_by = mtcars$cyl,
#'                      medians = T,
#'                      group_per = 'columns')
#' @details
#' The \code{values}, \code{round_to}, and \code{new_name} arguments given in
#' the \code{\link{m_neat}} function are always applied. However, the
#' \code{group_by} or \code{medians} given in the \code{\link{m_neat}} function
#' are only applied when no arguments are given in the \code{\link{table_neat}}
#' function for the identical parameters (\code{group_by} or \code{medians}). If
#' either parameter is given in the \code{\link{table_neat}} function, all
#' separately given respective argument(s) in the \code{\link{m_neat}}
#' function(s) are ignored.
#' @seealso \code{\link{m_neat}} for more related details
#' @export

table_neat = function( values_list, group_by = NULL, group_per = 'rows', to_clipboard = F, medians = NULL ){
    print_off()
    tryCatch(
        {
            pkg.globals$my_unique_grouping_var = group_by
            pkg.globals$my_unique_median = medians
            the_table = Reduce(function(x, y) merge(x, y, by = "group", all=TRUE), values_list )
        },
        error=function(error_message) {
            message(error_message)
            pkg.globals$my_unique_grouping_var = NULL
            pkg.globals$my_unique_median = NULL
            print_on()
            return(NA)
        }
    )
    pkg.globals$my_unique_grouping_var = NULL
    pkg.globals$my_unique_median = NULL
    print_on()
    if ( group_per != "" && substr("columns", 1, nchar(group_per) ) == group_per ) {
        the_table = t( data.frame(the_table, row.names =  1 ) )
        the_table = data.frame( variables = row.names(the_table), the_table)
        row.names(the_table) = 1:nrow(the_table)
    }
    if ( to_clipboard == T ) {
        write.table( the_table, "clipboard", sep="\t", quote = F, row.names = F )
        message('Table copied to Clipboard.')
    }
    return( the_table )
}