# globals
pkg.globals = new.env()
pkg.globals$my_unique_grouping_var = NULL
pkg.globals$my_unique_median = NULL

# misc local functions below

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
