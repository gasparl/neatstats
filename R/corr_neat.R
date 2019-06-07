#' Neat correlation
#'
#' This function gives Pearson correlation results including CIs and BFs.
#' @keywords correlation
#' @export
#' @examples
#' corr_neat()

corr_neat = function( var1, var2, ci = .95, bf_added = T, direction = "", round_r = 3, for_table = F ) {
    if ( direction != "" && substr("negative", 1, nchar(direction) ) == direction ) {
        message("One-sided test! Negative correlation expected.")
        the_cor = cor.test( var1, var2, alternative = "l", conf.level = ci )
        if ( bf_added == T ) {
            bf = as.vector( BayesFactor::correlationBF( var1, var2, nullInterval = c(-1, 0) )[1] )
        }
    } else if ( direction != "" & substr("positive", 1, nchar(direction) ) == direction ) {
        message("One-sided test! Positive correlation expected.")
        the_cor = cor.test( var1, var2, alternative = "g", conf.level = ci )
        if ( bf_added == T ) {
            bf = as.vector( BayesFactor::correlationBF( var1, var2, nullInterval = c(0, 1) )[1] )
        }
    } else {
        the_cor = cor.test( var1, var2, conf.level = ci )
        if ( bf_added == T ) {
            bf = as.vector( BayesFactor::correlationBF( var1, var2 ) )
        }
    }
    if ( bf_added == T ) {
        bf_out = bf_neat( bf )
    } else {
        bf_out = "."
        bf = NA
    }
    if (for_table == T) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci*100, 0), "% CI")
    }    
    r = edges( the_cor$estimate, round_r, no_null = T )
    lower = edges( the_cor$conf.int[1], round_r, no_null = T )
    upper = edges( the_cor$conf.int[2], round_r, no_null = T )
    p_value = the_cor$p.value
    df = the_cor$parameter
    out = paste0( "r(", df, ") = ", r, ci_disp, " [", lower, ", ", upper, "]", ", p = ", ro(p_value,3), bf_out )
    prnt(out)
    invisible( c( r = as.numeric( the_cor$estimate ), p = p_value, bf = as.numeric(bf) ) ) 
}