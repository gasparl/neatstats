# call this as source("C:/research/proj_neatstats/neatStats/R/neatStats.R")

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
    Encoding(to_print) = "UTF-8"
    to_print = gsub("CHAR_MINUS", "\u2013", to_print )
    Encoding(to_print) = "UTF-8"
    to_print = gsub("CHAR_PLUSMIN", "\u00b1", to_print )
    Encoding(to_print) = "UTF-8"
    to_print = gsub("CHAR_X", "\u00d7", to_print )
    Encoding(to_print) = "UTF-8"
    # TODO: to clipboard
    pkg.globals$printing( to_print )
}


pkg.globals = new.env()

pkg.globals$printing = function( to_print ) {
    cat( to_print, fill = T)
}


#' Printing ON
#'
#' This function switches on printing.
#' @keywords print on
#' @export
#' @examples
#' print_on()
print_on = function() {
    pkg.globals$printing = function( to_print ) {
        cat( to_print, fill = T)
    }
}

#' Printing OFF
#'
#' This function switches off printing.
#' @keywords print off
#' @export
#' @examples
#' print_off()
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

#' Neat t-test
#'
#' This function gives thorough t-test results including CIs and BFs.
#' @keywords ttest
#' @export
#' @examples
#' t_neat()

t_neat = function( var1, var2, pair = F, greater = "", ci = 0.95, bf_added = T, auc_added = F, for_table = F, test_title = "Descriptives:", round_descr = 2, round_auc = 3, auc_greater = "" ) {
    descr_1 = paste0( ro( mean(var1), round_descr ), "CHAR_PLUSMIN", ro( sd(var1), round_descr ) )
    descr_2 = paste0( ro( mean(var2), round_descr ), "CHAR_PLUSMIN", ro( sd(var2), round_descr ) )
    prnt( test_title, " MCHAR_PLUSMINSD = ", descr_1, " vs. ", descr_2 )
    if ( greater == "1" ) {
        message("One-sided t-test and BF! H1: first is greater than second.")
        ttest = t.test( var1, var2, paired = pair, alternative = "greater" )
        if ( bf_added == T ) {
            bf = as.vector( ttestBF( var1, var2, paired = pair, nullInterval = c(0, Inf) )[1] )
        }
    } else if ( greater == "2" ) {
        message("One-sided t-test and BF! H1: second is greater than first.")
        ttest = t.test( var1, var2, paired = pair, alternative = "less" )
        if ( bf_added == T ) {
            bf = as.vector( ttestBF( var1, var2, paired = pair, nullInterval = c(0, -Inf) )[1] )
        }
    } else {
        ttest = t.test( var1, var2, paired = pair )
        if ( bf_added == T ) {
            bf = as.vector( ttestBF( var1, var2, paired = pair ) )
        }
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
        sm = quiet( ci.sm( ncp = ttest$statistic, N = n1, conf.level = ci ) )
        d_orig = sm$Standardized.Mean
        d = paste0( "dwithin = ", ro( d_orig, 2 ) )
        df = ro( df, 0 )
        lower = ro( sm$Lower.Conf.Limit.Standardized.Mean, 2 )
        upper = ro( sm$Upper.Conf.Limit.Standardized.Mean, 2 )
    } else {
        the_smd = ci.smd( ncp = t, n.1 = n1, n.2 = n2, conf.level = ci )
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
    out = paste0( "t(", df, ") = ", ro(t, 2), ", p = ", ro(pvalue,3), ", ", d, ci_disp, " [", lower, ", ", upper, "]", bf_out )
    prnt(out)
    if ( auc_added == T ) {
        if ( auc_greater == "2" ) {
            auc_dir = ">" # v2 expected larger
        } else {
            auc_dir = "<" # v1 expected larger
        }        
        the_roc = roc( response = c( rep( 0, length(var2) ), rep( 1, length(var1) ) ), predictor = c(var2, var1), direction =  auc_dir ) # v1 larger
        show_auc( theroc = the_roc, ci = ci, round_to = round_auc, for_table = for_table )
        max_acc = as.numeric( coords(the_roc, x = "best", ret = "accuracy" ) )[1]
        best_coords = coords(the_roc, x = "best" ) 
        invisible( list( stats = c( t = as.numeric(t), p = pvalue, d = as.numeric(d_orig), bf = as.numeric(bf), auc = auc(the_roc), accuracy = max_acc ), roc_obj = the_roc, best_thresholds = best_coords ) )
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
    auc_num = edges( auc(theroc), round_to )
    auc_ci = as.numeric( ci.auc( theroc, conf.level = ci ) )
    lower = edges( auc_ci[1], round_to )
    upper = edges( auc_ci[3], round_to )
    prnt( "AUC = ", auc_num, ci_disp, " [", lower, ", ", upper, "]" )
}
edges = function( the_num, round_to ) {
    if ( the_num == 1 ) {
        return( "1" )
    } else if ( the_num == 0 ) {
        return( "0" )
    } else {
        return( sub('.', '', ro(  the_num, round_to ) ) )
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
    roc_test = roc.test(roc1, roc2, paired = pair, alternative = alt)
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

#' Neat ANOVA
#'
#' This function gives thorough ANOVA results including CIs and BFs.
#' @keywords anova
#' @export
#' @examples
#' anova_neat()

anova_neat = function( data_long, value_col, id_col, between_vars = NULL, within_vars = NULL, ci = 0.90, bf_added = T, test_title = "--- neat ANOVA ---" ) {
    this_data = eval(parse(text=data_long))
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
    ez_anova_out = eval(parse(text=
                                  paste0('ezANOVA(data= this_data,
                                         dv=', value_col,',
                                         wid=', id_col,',
                                         between =', between_vars_ez,',
                                         within =', within_vars_ez,',
                                         type = 2, detailed = TRUE )')
    ))    
    if ( bf_added == T ) {
        indep_vars = gsub( ',', ' *', paste0( between_vars_bf, within_vars_bf ) )
        bf = eval(parse(text=
            paste0(
                'as.vector( anovaBF(', value_col,' ~ ', indep_vars, id_part, ', data = this_data, whichRandom = "', id_col, '", whichModels = "bottom") )'
            )
        ))
        prnt( "---Bayes factor---" )
        print( bf ) # to remove
        names( bf ) = bf_names( names( bf ) )
    } else {
        bf = NULL
    }
    anova_apa( ezANOVA_out = ez_anova_out, ci = ci, bf_added = bf, test_title = test_title )
}

anova_apa = function( ezANOVA_out, ci = 0.90, bf_added = NULL, test_title = "--- neat ANOVA ---" ) {
    ezANOVA_out = aovEffectSize(ezANOVA_out, "pes")
    prnt( "---ezANOVA---" )
    print(ezANOVA_out) # to remove
    prnt( test_title )
    for (indx in 1:length( ezANOVA_out$ANOVA$Effect )){
        f_name = ezANOVA_out$ANOVA$Effect[indx]
        f_name = sort( strsplit( f_name, ":" )[[1]] )
        f_name = paste( f_name, collapse = " CHAR_X " )
        if ( is.null( bf_added ) | !( f_name %in% names( bf_added ) ) ) {
            bf_out = "."
        } else {
            bf_val = bf_added[ f_name ]
            bf_out = bf_neat( bf_val )
        }
        F_val = ezANOVA_out$ANOVA$F[indx]
        df_n = ezANOVA_out$ANOVA$DFn[indx]
        df_d = ezANOVA_out$ANOVA$DFd[indx]
        pvalue = ezANOVA_out$ANOVA$p[indx]   
        petas = ezANOVA_out$ANOVA$pes[indx]
        
        limits = conf.limits.ncf(F.value = F_val, conf.level = ci, df.1 = df_n, df.2 = df_d )
        lower = limits$Lower.Limit / (limits$Lower.Limit + df_n + df_d + 1)
        upper = limits$Upper.Limit / (limits$Upper.Limit + df_n + df_d + 1)
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
        out = paste0( "F(", df_n, ",", df_d, ")", " = ", ro(F_val, 2), ", p = ", ro(pvalue,3), ", np2 = ", np2, the_ci, lower, ", ", upper, "]", bf_out, " (", f_name, ")")
        prnt(out)        
    }
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


val_per_cond = function(values, percent = F, digits = 0){
    val_name = unlist(strsplit(deparse( substitute( values )), "\\$") )[2]
    full_data$zero = "0"
    bylist = full_data$condition # full_data$condition or full_data$zero
    if ( percent == T ) {
        per_cond <- do.call(data.frame, aggregate( values, by = list(bylist), function(x) c(mean = ro(mean(x*100),digits+1), sd = ro(sd(x*100),digits+1))) )
    } else {
        per_cond <- do.call(data.frame, aggregate( values, by = list(bylist), function(x) c(mean = ro(mean(x), digits), sd = ro(sd(x), digits ) )) )
    }
    per_cond[val_name] = paste(per_cond$x.mean, per_cond$x.sd, sep="CHAR_PLUSMIN")
    per_cond = subset(per_cond, select=-c(x.mean,x.sd))
    return(per_cond)
}
