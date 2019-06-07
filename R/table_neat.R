#' Neat Table
#'
#' This function gives a neat M and SD table using m_neat() function(s) as input.
#' @keywords table
#' @export
#' @examples
#' table_neat()

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
    }
    return( the_table )
}