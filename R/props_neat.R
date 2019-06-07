#' Neat comparison of two proportions
#'
#' This function compares two independent proportions; including CIs and BFs.
#' @keywords proportions
#' @export
#' @examples
#' props_neat()

props_neat = function( case1, case2, n1, n2, greater = "", ci = NULL, bf_added = T, h_added = F, for_table = F ){
    # to add: McNemar for paired; corresponding BF
    prop_1 = case1/n1
    prop_2 = case2/n2
    p_diff = prop_1 - prop_2
    matr = matrix(c( case1, case2, n1-case1, n2-case2), 2, 2 )
    exact_res = Exact::exact.test( matr, to.plot = F )
    z_norm = -0.862 + sqrt( 0.743 - 2.404 * log( exact_res$p.value  ) )
    p_se = abs( p_diff / z_norm )
    if ( greater == "1" ) {
        message("One-sided exact-test (with 90% CI default)! H1: first is greater than second.")
        exact_res = Exact::exact.test( matr, to.plot = F, alternative = "greater" )
    } else if ( greater == "2" ) {
        message("One-sided exact-test (with 90% CI default)! H1: second is greater than first.")
        exact_res = Exact::exact.test( matr, to.plot = F, alternative = "less" )
    } else {
        if ( is.null(ci) ) {
            ci = 0.95
        }
    }    
    if ( is.null(ci) ) {
        ci = 0.90
    }
    z_c = qnorm(1 - (1-ci) / 2)
    p_low = p_diff - p_se*z_c
    p_upp = p_diff + p_se*z_c
    
    x1 = asin(sign(prop_1) * sqrt(abs(prop_1)))
    x2 = asin(sign(prop_2) * sqrt(abs(prop_2)))
    es = x1 - x2
    se_h = sqrt(0.25 * (1 / n1 + 1 / n2 ))
    h_low = (es-(z_c * se_h) )*2
    h_upp = (es+(z_c * se_h) )*2
    h = es*2
    z = exact_res$statistic
    pvalue = exact_res$p.value
    if ( bf_added == T ) {
        bf = contingencyTableBF(matr, sampleType = "indepMulti", fixedMargin = "rows")
        bf = as.vector( bf )
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
    if ( h_added == T ) {
        h_out = paste0(", h = ", ro(h, 2), ci_disp, " [", ro(h_low, 2), ", ", ro(h_upp, 2), "]")
    } else {
        h_out = ''
    }
    p_diff_out = edges( p_diff, 2, no_null = T )
    p_low_out = edges( p_low, 2, no_null = T )
    p_upp_out = edges( p_upp, 2, no_null = T )
    
    out = paste0( 'Z = ',  ro(z, 2), ", p = ", ro(pvalue,3), ", Pdiff = ", p_diff_out, ci_disp, " [", p_low_out, ", ", p_upp_out, "]", h_out, bf_out )
    prnt( out ) 
    invisible( c( z = as.numeric(z), p = pvalue, prop_diff = p_diff, h = h, bf = as.numeric(bf) ) )
}