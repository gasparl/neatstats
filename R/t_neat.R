#' Neat t-test
#'
#' This function gives thorough t-test results including CIs and BFs.
#' @keywords ttest
#' @export
#' @examples
#' t_neat()

t_neat = function( var1, var2, pair = F, greater = "", ci = NULL, bf_added = T, auc_added = F, r_added = F, for_table = F, test_title = "Descriptives:", round_descr = 2, round_auc = 3, auc_greater = "" ) {
    descr_1 = paste0( ro( mean(var1), round_descr ), "CHAR_PLUSMIN", ro( sd(var1), round_descr ) )
    descr_2 = paste0( ro( mean(var2), round_descr ), "CHAR_PLUSMIN", ro( sd(var2), round_descr ) )
    prnt( test_title, " MCHAR_PLUSMINSD = ", descr_1, " vs. ", descr_2 )
    if ( greater == "1" ) {
        message("One-sided t-test and BF (with 90% CI default)! H1: first is greater than second.")
        ttest = t.test( var1, var2, paired = pair, alternative = "greater" )
        if ( bf_added == T ) {
            bf = as.vector( BayesFactor::ttestBF( var1, var2, paired = pair, nullInterval = c(0, Inf) )[1] )
        }
    } else if ( greater == "2" ) {
        message("One-sided t-test and BF (with 90% CI default)! H1: second is greater than first.")
        ttest = t.test( var1, var2, paired = pair, alternative = "less" )
        if ( bf_added == T ) {
            bf = as.vector( BayesFactor::ttestBF( var1, var2, paired = pair, nullInterval = c(0, -Inf) )[1] )
        }
    } else {
        ttest = t.test( var1, var2, paired = pair )
        if ( bf_added == T ) {
            bf = as.vector( BayesFactor::ttestBF( var1, var2, paired = pair ) )
        }
        if ( is.null(ci) ) {
            ci = 0.95
        }
    }    
    if ( is.null(ci) ) {
        ci = 0.90
    }
    if ( bf_added == T ) {
        bf_out = bf_neat( bf )
    } else {
        bf_out = "."
        bf = NA
    }
    t = as.vector(ttest$statistic)
    df = as.vector(ttest$parameter)
    pvalue = ttest$p.value
    n1 = length(var1)
    n2 = length(var2)
    if ( pair == T ) {
        sm = quiet( MBESS::ci.sm( ncp = ttest$statistic, N = n1, conf.level = ci ) )
        d_orig = sm$Standardized.Mean
        d = paste0( "dwithin = ", ro( d_orig, 2 ) )
        df = ro( df, 0 )
        lower = ro( sm$Lower.Conf.Limit.Standardized.Mean, 2 )
        upper = ro( sm$Upper.Conf.Limit.Standardized.Mean, 2 )
    } else {
        the_smd = MBESS::ci.smd( ncp = t, n.1 = n1, n.2 = n2, conf.level = ci )
        d_orig = the_smd$smd
        d = paste0( "dbetween = ", ro( d_orig, 2 ) )
        df = ro( df, 1 )
        lower = ro( the_smd$Lower.Conf.Limit.smd, 2 )
        upper = ro( the_smd$Upper.Conf.Limit.smd, 2 )
    }    
    if (for_table == T) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci*100, 0), "% CI")
    }    
    if ( pair == T & r_added == T ) {
        cat( "Pearson's correlation: " )
        corr_neat( var1, var2, bf_added = F )
    }
    out = paste0( "t(", df, ") = ", ro(t, 2), ", p = ", ro(pvalue,3), ", ", d, ci_disp, " [", lower, ", ", upper, "]", bf_out )
    prnt(out)    
    if ( auc_added == T ) {
        if ( auc_greater == "2" ) {
            auc_dir = ">" # v2 expected larger
        } else {
            auc_dir = "<" # v1 expected larger
        }        
        the_roc = pROC::roc( response = c( rep( 0, length(var2) ), rep( 1, length(var1) ) ), predictor = c(var2, var1), direction =  auc_dir ) # v1 larger
        show_auc( theroc = the_roc, ci = ci, round_to = round_auc, for_table = for_table )
        max_acc = as.numeric( pROC::coords(the_roc, x = "best", ret = "accuracy" ) )[1]
        best_coords = pROC::coords(the_roc, x = "best" ) 
        invisible( list( stats = c( t = as.numeric(t), p = pvalue, d = as.numeric(d_orig), bf = as.numeric(bf), auc = pROC::auc(the_roc), accuracy = max_acc ), roc_obj = the_roc, best_thresholds = best_coords ) )
    } else {
        invisible( list( stats = c( t = as.numeric(t), p = pvalue, d = as.numeric(d_orig), bf = as.numeric(bf), auc = NULL, accuracy = NULL ), roc_obj = NULL, best_thresholds = NULL ) )
    }
}