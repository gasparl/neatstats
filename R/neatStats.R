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
prnt = function( ..., mysep = "\n") {
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
    cat( to_print, sep = mysep )
    # TODO: to clipboard
}
cit_d = function(probe_rts, irr_rts){
    return( (mean(probe_rts) - mean(irr_rts)) / sd(irr_rts) )
}
ro = function(value, round_to = 2) {
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

t_neat = function( var1, var2, pair = F, greater = "", ci = 0.95, bf_added = T, for_table = F, test_title = "Descriptives:", round_descr = 2 ) {
    descr_1 = paste0( ro( mean(var1), round_descr ), "CHAR_PLUSMIN", ro( sd(var1), round_descr ) )
    descr_2 = paste0( ro( mean(var2), round_descr ), "CHAR_PLUSMIN", ro( sd(var2), round_descr ) )
    prnt( test_title, " MCHAR_PLUSMINSD = ", descr_1, " vs. ", descr_2 )
    if ( greater == "1" ) {
        message("One-sided test! H1: first is greater than second.")
        ttest = t.test( var1, var2, paired = pair, alternative = "greater" )
        if ( bf_added == T ) {
            bf = as.vector( ttestBF( var1, var2, paired = pair, nullInterval = c(0, Inf) )[1] )
        }
    } else if ( greater == "2" ) {
        message("One-sided test! H1: second is greater than first.")
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
    }
    t = as.vector(ttest$statistic)
    df = as.vector(ttest$parameter)
    pvalue = ttest$p.value
    n1 = length(var1)
    n2 = length(var2)
    if ( pair == T ) {
        sm = quiet( ci.sm( ncp = ttest$statistic, N = n1, conf.level = ci ) )
        d = paste0( "dwithin = ", ro( sm$Standardized.Mean, 2 ) )
        df = ro( df, 0 )
        lower = ro( sm$Lower.Conf.Limit.Standardized.Mean, 2 )
        upper = ro( sm$Upper.Conf.Limit.Standardized.Mean, 2 )
    } else {
        the_smd = ci.smd( ncp = t, n.1 = n1, n.2 = n2, conf.level = ci )
        d = paste0( "dbetween = ", ro( the_smd$smd, 2 ) )
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
}


show_auc = function(theroc, for_table = T, round_to = 3) {
    if (for_table == T) {
       prnt("AUC = ", format(round(as.numeric(auc(theroc)), round_to), nsmall = round_to), " [", format(round(as.numeric(ci(theroc))[1], round_to), nsmall = round_to), ", ", format(round(as.numeric(ci(theroc))[3], round_to), nsmall = round_to), "]", sep = "")
    } else {
        prnt(" AUC = ", format(round(as.numeric(auc(theroc)), round_to), nsmall = round_to), ", 95% CI [", format(round(as.numeric(ci(theroc))[1], round_to), nsmall = round_to), ", ", format(round(as.numeric(ci(theroc))[3], round_to), nsmall = round_to), "]", sep = "")
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

#' Neat ANOVA
#'
#' This function gives thorough ANOVA results including CIs and BFs.
#' @keywords anova
#' @export
#' @examples
#' anova_neat()


anova_neat = function( data_long, value_col, id_col, between_vars = NULL, within_vars = NULL, ci = 0.90, bf_added = T, test_title = "--- neat ANOVA ---" ) {
    if ( is.null( between_vars ) ) {
        between_vars_ez = 'NULL'
        between_vars_bf = ''
    } else {
        between_vars_ez = paste0( 'c(', between_vars ,')' )
        between_vars_bf = between_vars
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
    }
    ez_anova_out = eval(parse(text=
                                  paste0('ezANOVA(data=,', data_long, ',
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
                'as.vector( anovaBF(', value_col,' ~ ', indep_vars, id_part, ', data = ', data_long, ', whichRandom = "', id_col, '", whichModels = "bottom") )'
            )
        ))
        cat( "---Bayes factor---\n" )
        print( bf ) # to remove
        names( bf ) = bf_names( names( bf ) )
    } else {
        bf = NULL
    }
    anova_apa( ezANOVA_out = ez_anova_out, ci = ci, bf_added = bf, test_title = test_title )
}

anova_apa = function( ezANOVA_out, ci = 0.90, bf_added = NULL, test_title = "--- neat ANOVA ---" ) {
    ezANOVA_out = aovEffectSize(ezANOVA_out, "pes")
    cat( "---ezANOVA---\n" )
    print(ezANOVA_out) # to remove
    cat( test_title, "\n" )
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
        prnt(out, sep = "")        
    }
}

age_gender_per_cond = function( all_data ) {
    for_gender = all_data[,c("condition","gender")]
    gender = data.frame(prop.table(table(for_gender), 1))
    gender = head(gender, nrow(gender)/2 )

    age = do.call(data.frame, aggregate( all_data$age, by = list(all_data$condition), function(x) c(count = length(x), mean = mean(x), sd = sd(x))) )
    names(age)[names(age) == "Group.1"] <- "condition"
    age_gend = merge( age, gender, by = "condition")

    for(i in 1:nrow(age_gend)) {
        row <- age_gend[i,]
        prnt( 'condition ', row[[1]], ': count ', round(row[2],1), ', age = ', format(round(row[3],1), nsmall = 1), 'CHAR_PLUSMIN', format(round(row[4],1), nsmall = 1), ', male ', format(round(row[6]*100,1), nsmall = 1), "%", sep = "")
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
