#' Neat ANOVA
#'
#' This function gives thorough ANOVA results including CIs and BFs.
#' @keywords anova
#' @export
#' @examples
#' anova_neat()

anova_neat = function(data_per_subject,
                      values,
                      between_vars = NULL,
                      within_ids = NULL,
                      ci = 0.90,
                      bf_added = T,
                      test_title = "--- neat ANOVA ---",
                      welch = T,
                      e_correction = '') {
    if (class(data_per_subject) == "character") {
        data_wide = eval(parse(text = data_per_subject))
    } else {
        data_wide = data_per_subject
    }
    if ('within_factor' %in%  names(data_wide)) {
        stop(
            'Sorry, the name "within_factor" is reserved for this function. Remove or rename that column.'
        )
    }
    if ('neat_unique_values' %in%  names(data_wide)) {
        stop(
            'Sorry, the name "neat_unique_values" is reserved for this function. Remove or rename that column.'
        )
    }
    if ('neat_unique_id' %in%  names(data_wide)) {
        stop(
            'Sorry, the name "neat_unique_values" is reserved for this function. Remove or rename that column.'
        )
    }
    id_col = 'neat_unique_id'
    data_wide[[id_col]] = as.character(seq.int(nrow(data_wide)))
    values = to_c(values)
    w_anova = NULL
    if (length(values) > 1) {
        data_reshaped = stats::reshape(
            data_wide,
            direction = 'long',
            varying = values,
            idvar = id_col,
            timevar = "within_factor",
            v.names = "neat_unique_values",
            times = values
        )
        if (length(within_ids) > 1) {
            for (fact_name in names(within_ids)) {
                data_reshaped[[fact_name]] = fact_name
                for (fact_x in within_ids[[fact_name]]) {
                    new_type = paste0(fact_name, fact_x)
                    data_reshaped[[fact_name]][grepl(fact_x, data_reshaped$within_factor)] = new_type
                }
                data_reshaped[[fact_name]] = as.factor(data_reshaped[[fact_name]])
            }
            within_vars = paste(names(within_ids), collapse = ', ')
        } else if (is.character(within_ids)) {
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
        if (length(to_c(between_vars)) == 1 && welch != F) {
            w_anova = eval(parse(
                text = paste0(
                    'stats::oneway.test(',
                    value_col,
                    ' ~ ',
                    between_vars,
                    ', data = this_data, var.equal = F  )'
                )
            ))
        }
    }

    this_data[, id_col] = to_fact(this_data[[id_col]])
    if (is.null(between_vars)) {
        between_vars_ez = 'NULL'
        between_vars_bf = ''
    } else {
        between_vars_ez = paste0('c(', between_vars , ')')
        between_vars_bf = between_vars
        for (this_col in to_c(between_vars)) {
            this_data[, this_col] = to_fact(this_data[[this_col]])
        }
    }
    if (is.null(within_vars)) {
        within_vars_ez = 'NULL'
        within_vars_bf = ''
        id_part = ''
    } else {
        within_vars_ez = paste0('c(', within_vars , ')')
        if (is.null(between_vars)) {
            within_vars_bf = within_vars
        } else {
            within_vars_bf = paste0(' * ', within_vars)
        }
        id_part = paste0(' + ', id_col)
        for (this_col in to_c(within_vars)) {
            this_data[, this_col] = to_fact(this_data[[this_col]])
        }
    }
    a_text = paste0(
        'ez::ezANOVA(data= this_data, dv=',
        value_col,
        ', wid=',
        id_col,
        ', between =',
        between_vars_ez,
        ', within =',
        within_vars_ez,
        ', type = 2, detailed = TRUE )'
    )
    ez_anova_out = eval(parse(text = a_text))
    # suppressWarnings
    # suppressMessages

    if (bf_added == T) {
        indep_vars = gsub(',', '*', paste0(between_vars_bf, within_vars_bf))
        bf = eval(parse(
            text =
                paste0(
                    'BayesFactor::anovaBF(',
                    value_col,
                    ' ~ ',
                    indep_vars,
                    id_part,
                    ', data = this_data, whichRandom = "',
                    id_col,
                    '", whichModels = "withmain")'
                )
        ))
        prnt("--- Bayes factor ---")
        if (is.null(within_vars) && length( to_c(between_vars) ) == 1 ) {
            bf_inc = as.vector(bf)
        } else {
            bf_inc = bayestestR::bayesfactor_inclusion(bf, match_models = T)
            print(bf_inc) # to remove
            bf_inc = setNames(object = bf_inc$BF, nm = rownames(bf_inc))
        }
        print(bf) # to remove
        print(bf_inc) # to remove
        bf_models = bf
        names(bf_inc) = bf_names(names(bf_inc))
    } else {
        bf_inc = NULL
        bf_models = NULL
    }
    to_return = anova_apa(
        ezANOVA_out = ez_anova_out,
        ci = ci,
        bf_added = bf_inc,
        bf_models = bf_models,
        test_title = test_title,
        welch = w_anova,
        e_correction = e_correction
    )
    invisible(to_return)
}

anova_apa = function(ezANOVA_out,
                     ci = 0.90,
                     bf_added = NULL,
                     bf_models = NULL,
                     test_title = "--- neat ANOVA ---",
                     welch = NULL,
                     e_correction = '') {
    levene = ezANOVA_out$"Levene's Test for Homogeneity of Variance"
    ezANOVA_out$ANOVA$'p<.05' = NULL
    ezANOVA_out$ANOVA$ges = NULL
    if ((!is.null(levene)) && (is.null(welch))) {
        if ('Levene' %in%  ezANOVA_out$ANOVA$Effect) {
            stop(
                'Sorry, the name "Levene" is reserved for this function. Remove or rename this factor.'
            )
        }
        levene_post = ''
        levene$'p<.05' = NULL
        levene = data.frame(Effect = "Levene", levene)
        if (round(levene$p, 3) < 0.05) {
            levene_pre = "Levene's test indicates unequal variances (p < 0.05): "
        } else {
            levene_pre = "Levene's test does not indicate unequal variances (p >= 0.05): "
        }
        ezANOVA_out$ANOVA = rbind(levene, ezANOVA_out$ANOVA)
    }
    ezANOVA_out$ANOVA$pes = ezANOVA_out$ANOVA$SSn / (ezANOVA_out$ANOVA$SSn + ezANOVA_out$ANOVA$SSd)
    ezANOVA_out$ANOVA$Effect = as.character(ezANOVA_out$ANOVA$Effect)
    prnt("--- ezANOVA ---")
    print(ezANOVA_out) # to remove
    prnt(test_title)
    mauchly = ezANOVA_out$"Mauchly's Test for Sphericity"
    if (!is.null(mauchly)) {
        prnt("- Mauchly's sphericity test:")
        eps_p_corrs = get_e_corrs(mauchly,
                                  ezANOVA_out$"Sphericity Corrections",
                                  e_correction)
        prnt("- ANOVA:")
    } else {
        eps_p_corrs = NULL
    }
    stat_list = list()
    for (indx in 1:length(ezANOVA_out$ANOVA$Effect)) {
        f_name = ezANOVA_out$ANOVA$Effect[indx]
        f_name = sort(strsplit(f_name, ":")[[1]])
        f_name = paste(f_name, collapse = " CHAR_X ")
        if (is.null(bf_added) |
            !(f_name %in% names(bf_added))) {
            bf_out = "."
            bf_val = NULL
        } else {
            bf_val = bf_added[f_name]
            bf_out = bf_neat(bf_val)
        }
        F_val = ezANOVA_out$ANOVA$F[indx]
        df_n = ezANOVA_out$ANOVA$DFn[indx]
        df_d = ezANOVA_out$ANOVA$DFd[indx]
        pvalue = ezANOVA_out$ANOVA$p[indx]
        limits = MBESS::conf.limits.ncf(
            F.value = F_val,
            conf.level = ci,
            df.1 = df_n,
            df.2 = df_d
        )
        lower = limits$Lower.Limit / (limits$Lower.Limit + df_n + df_d + 1)
        upper = limits$Upper.Limit / (limits$Upper.Limit + df_n + df_d + 1)
        if (!is.null(welch)) {
            F_val = as.numeric(welch$statistic)
            df_n = as.numeric(welch$parameter['num df'])
            df_d = ro(as.numeric(welch$parameter['denom df']), 1)
            pvalue = as.numeric(welch$p.value)
        }
        if (!is.null(eps_p_corrs[[f_name]])) {
            pvalue = as.numeric(eps_p_corrs[[f_name]]['pval'])
            eps_num = ro(as.numeric(eps_p_corrs[[f_name]]['eps']), 3)
            eps_added = paste0(', CHAR_EPS = ', eps_num)
        } else {
            eps_added = NULL
        }
        petas = ezANOVA_out$ANOVA$pes[indx]
        if (is.na(lower)) {
            lower = "0"
        } else {
            lower = sub('.', '', ro(lower, 3))
        }
        if (is.na(upper)) {
            upper = "< .001"
        } else {
            upper = sub('.', '', ro(upper, 3))
        }
        np2 = sub('.', '', ro(petas, 3))
        the_ci = paste0(", ", ro(ci * 100, 0), "% CI [")

        if (f_name == 'Levene') {
            out = paste0(
                levene_pre,
                "F(",
                df_n,
                ",",
                df_d,
                ")",
                " = ",
                ro(F_val, 2),
                ", p = ",
                ro(pvalue, 3),
                ", CHAR_ETAp2 = ",
                np2,
                the_ci,
                lower,
                ", ",
                upper,
                "]",
                bf_out,
                levene_post
            )
        } else {
            out = paste0(
                "F(",
                df_n,
                ",",
                df_d,
                ")",
                " = ",
                ro(F_val, 2),
                ", p = ",
                ro(pvalue, 3),
                epsilon = eps_added,
                ", CHAR_ETAp2 = ",
                np2,
                the_ci,
                lower,
                ", ",
                upper,
                "]",
                bf_out,
                " (",
                f_name,
                ")"
            )
        }
        prnt(out)
        s_name = gsub(" CHAR_X ", "_", f_name)
        stat_list[[s_name]] = c(
            F = as.numeric(F_val),
            p = pvalue,
            epsilon = eps_added,
            petas = as.numeric(petas),
            bf = as.numeric(bf_val)
        )
    }
    stat_list = c(stat_list, bf_models = bf_models, ez_anova = ezANOVA_out)
    invisible(stat_list)
}
