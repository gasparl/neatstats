#' Demographics
#'
#' Prints participant count, age, gender, from given dataset.
#' @examples
#' dems_neat()
#' @export
dems_neat = function( data_per_subject, group_by = NULL, percent = T, round_perc = 0 ) {
    if ( class( data_per_subject ) == "character") {
        s_dat = eval(parse(text=data_per_subject))
    } else {
        s_dat = data_per_subject
    }       
    if ( all( data_per_subject$gender == '1' | data_per_subject$gender == '2' ) == F ) {
        stop('The "gender" column must only contain the values 1 (male) or 2 (female).')
    }
    if ( is.null( group_by ) ) {
        s_dat$neat_cond = 99
    } else if ( class(group_by) == "character") {
        s_dat$neat_cond = s_dat[[ group_by ]]
    } else {
        s_dat$neat_cond = group_by
    }
    gender = as.data.frame.matrix( xtabs(~neat_cond+gender, s_dat ) )
    if (! '1' %in% colnames(gender) ) {
        gender = data.frame( '1' = 0, gender)
    } else if (! '2' %in% colnames(gender) ) {
        gender[['2']] = 0
    }
    gender$ratio = gender[[1]]/(gender[[1]]+gender[[2]])*100
    gender$neat_cond = row.names(gender)
    
    age = do.call(data.frame, aggregate( s_dat$age, by = list( s_dat$neat_cond ), function(x) c(count = length(x), mean = mean(x), sd = sd(x))) )
    names(age)[names(age) == "Group.1"] <- "neat_cond"    
    age_gend = merge( age, gender, by = 'neat_cond' )
    
    if ( percent == T ) {
        for(i in 1:nrow(age_gend)) {
            row <- age_gend[i,]
            prnt( 'Group < ', row[[1]], ' >: ', row[2], ' subjects (age = ', ro(row[3],1), 'CHAR_PLUSMIN', ro(row[4],1), ', ', ro(row[7], round_perc), "% male)" )
        }
    } else {
        for(i in 1:nrow(age_gend)) {
            row <- age_gend[i,]
            prnt( 'Group < ', row[[1]], ' >: ', row[2], ' subjects (age = ', ro(row[3],1), 'CHAR_PLUSMIN', ro(row[4],1), ', ', row[5], " male)" )
        }
    }
}