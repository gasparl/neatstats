#' Neat ROC test
#'
#' This function gives the result of comparing two ROCs (AUCs).
#' @keywords roc
#' @export
#' @examples
#' roc_neat()

roc_neat = function( roc1, roc2, pair = F, greater = "" ) {
    if ( greater == "1" ) {
        alt = "greater"
    } else if ( greater == "2" ) {
        alt = "less"
    } else {
        alt = "two.sided"    
    }    
    roc_test = pROC::roc.test(roc1, roc2, paired = pair, alternative = alt)
    roc_stat = roc_test$statistic
    df = roc_test$parameter
    p_value = roc_test$p.value
    if ( pair == F ) {
        out = paste0( "D(", ro(df, 2), ") = ", ro(roc_stat, 2), ", p = ", ro(p_value,3) )
    } else {
        out = paste0( "Z = ", ro(roc_stat, 2), ", p = ", ro(p_value,3) )
    }
    prnt(out)
    invisible( c(stat = as.numeric(roc_stat), p = p_value) )
}