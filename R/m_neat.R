#' Neat M and SD
#'
#' This function gives means (or medians) and SDs per group for given variable. Mainly for use in the table_neat() function.
#' @keywords table
#' @export
#' @examples
#' m_neat()

m_neat = function( values, round_to = 0, new_name = NULL, group_by = NULL, medians = F ){    
    if ( is.null( new_name ) ) {
        val_name = deparse( substitute( values ) )
        if ( grepl('\\$', val_name) ) {
            val_name = unlist(strsplit( val_name, "\\$") )[2]
        }
        val_name = unlist(strsplit( val_name, "\\*") )[1]
        val_name = unlist(strsplit( val_name, "/") )[1]
    } else {
        val_name = new_name
    }    
    if ( ! is.null( pkg.globals$my_unique_median ) ) {
        medians = pkg.globals$my_unique_median
    }
    if ( ! is.null( pkg.globals$my_unique_grouping_var ) ) {
        group_by = pkg.globals$my_unique_grouping_var
    }
    if ( is.null( group_by ) ) {
        group_by = rep( 0, length(values) )
    } 
    if ( medians == T ) {
        per_cond = do.call(data.frame, aggregate( values, by = list(group_by), function(x) c(median = ro(median(x), round_to ), sd = ro(sd(x), round_to ) )) )
        per_cond[val_name] = paste(per_cond$x.median, per_cond$x.sd, sep="\u00b1")
        per_cond = subset(per_cond, select=-c(x.median,x.sd))
    } else {
        per_cond = do.call(data.frame, aggregate( values, by = list(group_by), function(x) c(mean = ro(mean(x), round_to ), sd = ro(sd(x), round_to ) )) )
        per_cond[val_name] = paste(per_cond$x.mean, per_cond$x.sd, sep="\u00b1")
        per_cond = subset(per_cond, select=-c(x.mean,x.sd))
    }        
    colnames(per_cond)[colnames(per_cond) == 'Group.1'] <- 'group'
    for(i in 1:nrow(per_cond)) {
        row = per_cond[i,]
        prnt( row[[1]], ': MCHAR_PLUSMINSD = ', row[[2]] )
    }    
    invisible( per_cond )
}