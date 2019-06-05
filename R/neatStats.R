#' Neat path
#'
#' This function gives the path to current script's path in RStudio.
#' @keywords path
#' @export
#' @examples
#' script_path()
script_path = function() {
    return( dirname(rstudioapi::getActiveDocumentContext()$path) )
}

quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
}

prnt = function( ... ) {
    to_print = gsub('-', 'CHAR_MINUS', paste0( ... ) )
    to_print = sub("e\\+0*", " CHAR_X 10^", to_print )
    to_print = gsub("p = 0.", "p = .", to_print )
    to_print = gsub("p = .000", "p < .001", to_print ) 

    change_pairs = list( c('CHAR_MINUS', '\u2013'), 
                        c('CHAR_PLUSMIN', '\u00b1'),
                        c('CHAR_X', '\u00d7'),
                        c('CHAR_ETA', '\u03b7'),
                        c('CHAR_EPS', '\u03b5') )
     
    Encoding(to_print) = "UTF-8"
    for ( pair in change_pairs ) {
        to_print = gsub( pair[1], pair[2], to_print )
        Encoding(to_print) = "UTF-8"    
    }
    
    pkg.globals$printing( to_print )
}


pkg.globals = new.env()
pkg.globals$my_unique_grouping_var = NULL
pkg.globals$my_unique_median = NULL

pkg.globals$printing = function( to_print ) {
    cat( to_print, fill = T)
}

print_on = function() {
    pkg.globals$printing = function( to_print ) {
        cat( to_print, fill = T)
    }
}

print_off = function() {
    pkg.globals$printing = function( to_print ) {
        invisible()
    }
}

cit_d = function(probe_rts, irr_rts){
    return( (mean(probe_rts) - mean(irr_rts)) / sd(irr_rts) )
}

#' Neat rounding
#'
#' This function neatly rounds any number to given number of digits after the decimal point.
#' @keywords round
#' @export
#' @examples
#' ro()
ro = function(value, round_to = 2) {
    value = as.numeric( value )
    return(format(round(value, round_to), nsmall = round_to))
}

to_exp = function( the_num ) {
    if ( as.numeric( ro( the_num, 2 ) ) > 9999.99 ) {
        the_num = formatC( the_num, format = "e", digits = 2)
        return( the_num )
    } else {
        return( ro(the_num, 2) )
    }
}

bf_neat = function( bf ) {
    if ( is.na(bf) ) {
        message("BF = NA")
        return(".")
    } else {
        if ( bf < 1 ) {
            bf2 = 1/bf
            bf2 = to_exp( bf2 )
            bf_dir = paste0( ", BF01 = ", bf2 ) 
        } else {
            bf2 = to_exp( bf )
            bf_dir = paste0( ", BF10 = ", bf2 ) 
        }
        return( paste0( bf_dir, ". (BFplain = ", ro(bf, 4), ")" ) )
    }
}

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


bf_names = function( the_names ) {
    new_names = c()
    for ( a_name in the_names) {
        a_name = gsub( " .*","", a_name )
        a_name = sort( strsplit( a_name, ":" )[[1]] )
        a_name = paste( a_name, collapse = " CHAR_X " )
        new_names = c(new_names, a_name)
    }
    return( new_names )
}

show_auc = function(theroc, ci = 0.95, round_to = 3, for_table = F) {
    if (for_table == T) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci*100, 0), "% CI")
    }   
    auc_num = edges( pROC::auc(theroc), round_to )
    auc_ci = as.numeric( pROC::ci.auc( theroc, conf.level = ci ) )
    lower = edges( auc_ci[1], round_to )
    upper = edges( auc_ci[3], round_to )
    prnt( "AUC = ", auc_num, ci_disp, " [", lower, ", ", upper, "]" )
}
edges = function( the_num, round_to, no_null = F ) {
    if ( round(the_num, round_to) == 1 ) {
        return( "1" )
    } else if ( round(the_num, round_to) == -1 ) {
        return( "-1" )
    } else if ( round(the_num, round_to) == 0 & no_null == F ) {
        return( "0" )
    } else {
        return( sub("0\\.", "\\.", ro(  the_num, round_to ) ) )
    }
}


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


#' Neat ANOVA
#'
#' This function gives thorough ANOVA results including CIs and BFs.
#' @keywords anova
#' @export
#' @examples
#' anova_neat()

anova_neat = function( data_per_subject, values, id_col, between_vars = NULL, within_ids = NULL, ci = 0.90, bf_added = T, test_title = "--- neat ANOVA ---", welch = T, e_correction = '' ) {
    data_wide = eval(parse(text=data_per_subject))    
    if ('within_factor' %in%  names( data_wide ) ) {
        stop('Sorry, the name "within_factor" is reserved for this function. Remove or rename that column.')
    }
    if ('neat_unique_values' %in%  names( data_wide ) ) {
        stop('Sorry, the name "neat_unique_values" is reserved for this function. Remove or rename that column.')
    }    
    values = to_c( values )    
    w_anova = NULL
    if ( length( values ) > 1 ) {        
        data_reshaped = stats::reshape( data_wide, direction='long', varying= values, idvar = id_col, timevar = "within_factor", v.names = "neat_unique_values", times = values  )    
        if ( length( within_ids ) > 1 ) {
        
            for ( fact_name in names( within_ids ) ) {
                data_reshaped[[ fact_name ]] = fact_name
                for ( fact_x in within_ids[[fact_name]] ) {
                    new_type = paste0( fact_name, fact_x )
                    data_reshaped[[ fact_name ]][ grepl( fact_x, data_reshaped$within_factor ) ] = new_type
                }
                data_reshaped[[ fact_name ]] = as.factor( data_reshaped[[ fact_name ]] )
            }
            within_vars = paste( names(within_ids), collapse = ', ' )
        } else if ( is.character( within_ids ) ) {
            within_vars = within_ids            
            names(data_reshaped)[names(data_reshaped) == 'within_factor'] = within_ids
        } else {
            within_vars = 'within_factor'
        }
        value_col = "neat_unique_values"
        this_data = data_reshaped
    } else {
        value_col = values
        this_data = data_wide
        within_vars = NULL
        if ( length( to_c(between_vars) ) == 1 && welch != F ) {
            w_anova = eval(parse(text= paste0( 
                                            'stats::oneway.test(', value_col,' ~ ', between_vars, ', data = this_data, var.equal = F  )'
                                                    )
            ))   
        }
    }    
    
    this_data[,id_col] = to_fact(this_data[[id_col]])
    if ( is.null( between_vars ) ) {
        between_vars_ez = 'NULL'
        between_vars_bf = ''
    } else {
        between_vars_ez = paste0( 'c(', between_vars ,')' )
        between_vars_bf = between_vars
        for ( this_col in to_c(between_vars) ) {
            this_data[,this_col] = to_fact(this_data[[this_col]])
        }
    }
    if ( is.null( within_vars ) ) {
        within_vars_ez = 'NULL'
        within_vars_bf = ''
        id_part = ''
    } else {
        within_vars_ez = paste0( 'c(', within_vars ,')' )
        if ( is.null( between_vars ) ) {
            within_vars_bf = within_vars
        } else {
            within_vars_bf = paste0( ' * ', within_vars )
        }
        id_part = paste0( ' + ', id_col )
        for ( this_col in to_c(within_vars) ) {
            this_data[,this_col] = to_fact(this_data[[this_col]])
        }
    }        
    a_text = paste0('ez::ezANOVA(data= this_data, dv=', value_col,', wid=', id_col,', between =', between_vars_ez,', within =', within_vars_ez,', type = 2, detailed = TRUE )' )
    ez_anova_out = eval(parse(text= a_text))
    # suppressWarnings
    # suppressMessages
    
    if ( bf_added == T ) {
        indep_vars = gsub( ',', ' *', paste0( between_vars_bf, within_vars_bf ) )
        bf = eval(parse(text=
            paste0(
                'as.vector( BayesFactor::anovaBF(', value_col,' ~ ', indep_vars, id_part, ', data = this_data, whichRandom = "', id_col, '", whichModels = "bottom") )'
            )
        ))
        prnt( "--- Bayes factor ---" )
        print( bf ) # to remove
        names( bf ) = bf_names( names( bf ) )
    } else {
        bf = NULL
    }
    to_return = anova_apa( ezANOVA_out = ez_anova_out, ci = ci, bf_added = bf, test_title = test_title, welch = w_anova, e_correction = e_correction )
    invisible( to_return )
}

anova_apa = function( ezANOVA_out, ci = 0.90, bf_added = NULL, test_title = "--- neat ANOVA ---", welch = NULL, e_correction = '' ) {
    levene = ezANOVA_out$"Levene's Test for Homogeneity of Variance"
    ezANOVA_out$ANOVA$'p<.05' = NULL
    ezANOVA_out$ANOVA$ges = NULL
    if ( ( ! is.null( levene ) ) && ( is.null( welch ) ) ) {    
        if ('Levene' %in%  ezANOVA_out$ANOVA$Effect ) {
            stop('Sorry, the name "Levene" is reserved for this function. Remove or rename this factor.')
        }             
        levene_post = ''
        levene$'p<.05' = NULL
        levene = data.frame( Effect = "Levene", levene )
        if ( round( levene$p, 3 ) < 0.05 ) {
            levene_pre = "Levene's test indicates unequal variances (p < 0.05): "
        } else {
            levene_pre = "Levene's test does not indicate unequal variances (p >= 0.05): "
        }
        ezANOVA_out$ANOVA = rbind( levene, ezANOVA_out$ANOVA )
    }
    ezANOVA_out$ANOVA$pes = ezANOVA_out$ANOVA$SSn / (ezANOVA_out$ANOVA$SSn + ezANOVA_out$ANOVA$SSd) 
    ezANOVA_out$ANOVA$Effect = as.character( ezANOVA_out$ANOVA$Effect )    
    prnt( "--- ezANOVA ---" )
    print(ezANOVA_out) # to remove
    prnt( test_title )
    mauchly = ezANOVA_out$"Mauchly's Test for Sphericity"
    if ( ! is.null( mauchly ) ) {
        prnt("- Mauchly's sphericity test:")
        eps_p_corrs = get_e_corrs( mauchly, ezANOVA_out$"Sphericity Corrections", e_correction )
        prnt("- ANOVA:")
    } else {
        eps_p_corrs = NULL
    }
    stat_list = list()
    for (indx in 1:length( ezANOVA_out$ANOVA$Effect )){
        f_name = ezANOVA_out$ANOVA$Effect[indx]
        f_name = sort( strsplit( f_name, ":" )[[1]] )
        f_name = paste( f_name, collapse = " CHAR_X " )
        if ( is.null( bf_added ) | !( f_name %in% names( bf_added ) ) ) {
            bf_out = "."
            bf_val = NULL
        } else {
            bf_val = bf_added[ f_name ]
            bf_out = bf_neat( bf_val )
        }        
        F_val = ezANOVA_out$ANOVA$F[indx]
        df_n = ezANOVA_out$ANOVA$DFn[indx]
        df_d = ezANOVA_out$ANOVA$DFd[indx]
        pvalue = ezANOVA_out$ANOVA$p[indx]          
        limits = MBESS::conf.limits.ncf(F.value = F_val, conf.level = ci, df.1 = df_n, df.2 = df_d )
        lower = limits$Lower.Limit / (limits$Lower.Limit + df_n + df_d + 1)
        upper = limits$Upper.Limit / (limits$Upper.Limit + df_n + df_d + 1)        
        if ( ! is.null( welch ) ) {
            F_val = as.numeric( welch$statistic )
            df_n = as.numeric( welch$parameter['num df'] )
            df_d = ro( as.numeric( welch$parameter['denom df'] ), 1 )
            pvalue = as.numeric( welch$p.value )
        } 
        if ( ! is.null( eps_p_corrs[[f_name]] ) ) {
            pvalue = as.numeric( eps_p_corrs[[f_name]]['pval'] )
            eps_num = ro( as.numeric( eps_p_corrs[[f_name]]['eps'] ), 3 )
            eps_added = paste0( ', CHAR_EPS = ', eps_num )
        } else {
            eps_added = NULL
        }
        petas = ezANOVA_out$ANOVA$pes[indx]        
        if ( is.na(lower) ) {
            lower = "0"
        } else {
            lower = sub('.', '', ro(lower, 3) )
        }        
        if ( is.na(upper) ) {
            upper = "< .001"
        } else {
            upper = sub('.', '', ro(upper, 3) )
        }
        np2 = sub('.', '', ro(petas, 3) )
        the_ci = paste0(", ", ro(ci*100, 0), "% CI [")
        
        if ( f_name == 'Levene' ) {
            out = paste0( levene_pre, "F(", df_n, ",", df_d, ")", " = ", ro(F_val, 2), ", p = ", ro(pvalue,3), ", CHAR_ETAp2 = ", np2, the_ci, lower, ", ", upper, "]", bf_out, levene_post)
        } else {
            out = paste0( "F(", df_n, ",", df_d, ")", " = ", ro(F_val, 2), ", p = ", ro(pvalue,3), epsilon = eps_added, ", CHAR_ETAp2 = ", np2, the_ci, lower, ", ", upper, "]", bf_out, " (", f_name, ")")
        }
        prnt(out)
        s_name = gsub( " CHAR_X ", "_", f_name )
        stat_list[[ s_name ]] = c( F = as.numeric(F_val), p = pvalue, epsilon = eps_added, petas = as.numeric(petas), bf = as.numeric(bf_val) )
    }
    invisible( stat_list )
}

get_e_corrs = function( mauchly, e_corrects, e_correction ) {
    e_corrs_list = list()
    for (indx in 1:length( e_corrects$Effect )){
        s_name = e_corrects$Effect[indx]
        s_name = sort( strsplit( s_name, ":" )[[1]] )
        s_name = paste( s_name, collapse = " CHAR_X " )
        e_corrs_list[[ s_name ]] = c( gge = e_corrects$GGe[indx],
                                      ggp = e_corrects$'p[GG]'[indx],
                                      hfe = e_corrects$HFe[indx],
                                      hfp = e_corrects$'p[HF]'[indx] )
        
    }
    spher_real_corrs = list()
    for (indx in 1:length( mauchly$Effect )){
        m_name = mauchly$Effect[indx]
        m_name = sort( strsplit( m_name, ":" )[[1]] )
        m_name = paste( m_name, collapse = " CHAR_X " )
        m_w = mauchly$W[indx]
        m_pval = mauchly$p[indx]
        
        if ( e_correction == 'none' ) {
            m_corr = '.'
        } else if ( e_correction == 'gg' ) {
            m_corr = '. Correction: Greenhouse-Geisser.'
            spher_real_corrs[[ m_name ]] = c( eps = as.numeric( e_corrs_list[[m_name]]['gge'] ),
                                              pval = as.numeric( e_corrs_list[[m_name]]['ggp'] ) )
                
        } else if ( e_correction == 'hf' ) {
            m_corr = '. Correction: Huynh-Feldt.'
            spher_real_corrs[[ m_name ]] = c( eps = as.numeric( e_corrs_list[[m_name]]['hfe'] ),
                                              pval = as.numeric( e_corrs_list[[m_name]]['hfp'] ) )
        } else if ( round(m_pval, 3) < 0.05 ) {
            this_gge = as.numeric( e_corrs_list[[m_name]]['gge'] )
            if ( round( this_gge, 3 ) > 0.75 ) {
                m_corr = '. Correction: Huynh-Feldt (Greenhouse-Geisser CHAR_EPS > 0.75).'
                spher_real_corrs[[ m_name ]] = c( eps = as.numeric( e_corrs_list[[m_name]]['hfe'] ),
                                                 pval = as.numeric( e_corrs_list[[m_name]]['hfp'] ) )
                
            } else {
                m_corr = '. Correction: Greenhouse-Geisser (CHAR_EPS <= 0.75).'
                spher_real_corrs[[ m_name ]] = c( eps = as.numeric( e_corrs_list[[m_name]]['gge'] ),
                                                 pval = as.numeric( e_corrs_list[[m_name]]['ggp'] ) )
            }
        } else {
            m_corr = '.'
        }
        
        prnt( m_name, ': W = ', ro(m_w, 3), ', p = ',  ro(m_pval, 3), m_corr )
    
    }
    return(spher_real_corrs)
}

to_fact = function( var ) {
    return( as.factor( tolower( as.character( var ) ) ))
}
to_c = function( var ) {
    var = gsub("\\s", "", var) 
    return( strsplit(var, ",")[[1]] )
}

age_gender = function( all_data ) {
    # if no condition column: create it
    for_gender = all_data[,c("condition","gender")]
    gender = data.frame(prop.table(table(for_gender), 1))
    gender = head(gender, nrow(gender)/2 )

    age = do.call(data.frame, aggregate( all_data$age, by = list(all_data$condition), function(x) c(count = length(x), mean = mean(x), sd = sd(x))) )
    names(age)[names(age) == "Group.1"] <- "condition"
    age_gend = merge( age, gender, by = "condition")

    for(i in 1:nrow(age_gend)) {
        row <- age_gend[i,]
        prnt( 'condition ', row[[1]], ': count ', ro(row[2],1), ', age = ', ro(row[3],1), 'CHAR_PLUSMIN', ro(row[4],1), ', male ', ro(row[6]*100,1), "%", sep = "")
    }
}



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