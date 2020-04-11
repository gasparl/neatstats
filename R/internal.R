# globals
pkg.globals = new.env()
pkg.globals$my_unique_grouping_var = NULL
pkg.globals$my_unique_method = NULL

# the function below is to be added later
# to_clipboard = function( printing_function ) {
#    text_for_cb = capture.output( printing_function )
#    write.table( text_for_cb, "clipboard", quote = FALSE, row.names = FALSE, col.names = FALSE )
# }

# misc local functions below

quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

prnt = function(...) {
    to_print = gsub('-', 'CHAR_MINUS', paste0(...))
    to_print = sub("e\\+0*", " CHAR_X 10^", to_print)
    to_print = gsub("p = 0.", "p = .", to_print)
    to_print = gsub(" = .000", " < .001", to_print)
    change_pairs = list(
        c('CHAR_MINUS', '\u2013'),
        c('CHAR_PLUSMIN', '\u00b1'),
        c('CHAR_X', '\u00d7'),
        c('CHAR_ETA', '\u03b7'),
        c('CHAR_EPS', '\u03b5'),
        c('CHAR_INF', '\u221e')
    )
    Encoding(to_print) = "UTF-8"
    for (pair in change_pairs) {
        to_print = gsub(pair[1], pair[2], to_print)
        Encoding(to_print) = "UTF-8"
    }
    cat(to_print, fill = TRUE)
}

to_exp = function(the_num) {
    if (as.numeric(ro(the_num, 2)) >= 10000) {
        the_num = formatC(the_num, format = "e", digits = 2)
        return(the_num)
    } else {
        return(ro(the_num, 2))
    }
}

bf_neat = function(bf) {
    if (is.na(bf)) {
        message("BF = NA")
        return(".")
    } else {
        if (bf < 1) {
            bf2 = 1 / bf
            bf2 = to_exp(bf2)
            bf_dir = paste0(", BF01 = ", bf2)
        } else {
            bf2 = to_exp(bf)
            bf_dir = paste0(", BF10 = ", bf2)
        }
        return(paste0(bf_dir, "."))
        # return(paste0(bf_dir, ". (BFplain = ", ro(bf, 4), ")"))
    }
}

bf_names = function(the_names) {
    new_names = c()
    for (a_name in the_names) {
        a_name = gsub(" .*", "", a_name)
        a_name = sort(strsplit(a_name, ":")[[1]])
        a_name = paste(a_name, collapse = " CHAR_X ")
        new_names = c(new_names, a_name)
    }
    return(new_names)
}

show_auc = function(theroc,
                    ci = 0.95,
                    round_to = 3,
                    for_table = FALSE) {
    if (for_table == TRUE) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
    }
    auc_num = edges(pROC::auc(theroc), round_to)
    auc_ci = as.numeric(pROC::ci.auc(theroc, conf.level = ci))
    lower = edges(auc_ci[1], round_to)
    upper = edges(auc_ci[3], round_to)
    prnt("AUC = ", auc_num, ci_disp, " [", lower, ", ", upper, "]")
}

edges = function(the_num, round_to, no_null = FALSE) {
    if (round(the_num, round_to) == 1) {
        return("1")
    } else if (round(the_num, round_to) == -1) {
        return("-1")
    } else if (round(the_num, round_to) == 0 & no_null == FALSE) {
        return("0")
    } else {
        return(sub("0\\.", "\\.", ro(the_num, round_to)))
    }
}

get_e_corrs = function(mauchly, e_corrects, e_correction, shush) {
    e_corrs_list = list()
    for (indx in 1:length(e_corrects$Effect)) {
        s_name = e_corrects$Effect[indx]
        s_name = sort(strsplit(s_name, ":")[[1]])
        s_name = paste(s_name, collapse = " CHAR_X ")
        e_corrs_list[[s_name]] = c(
            gge = e_corrects$GGe[indx],
            ggp = e_corrects$'p[GG]'[indx],
            hfe = e_corrects$HFe[indx],
            hfp = e_corrects$'p[HF]'[indx]
        )
    }
    spher_real_corrs = list()
    for (indx in 1:length(mauchly$Effect)) {
        m_name = mauchly$Effect[indx]
        m_name = sort(strsplit(m_name, ":")[[1]])
        m_name = paste(m_name, collapse = " CHAR_X ")
        m_w = mauchly$W[indx]
        m_pval = mauchly$p[indx]

        if (e_correction == 'none') {
            m_corr = '.'
        } else if (e_correction == 'gg') {
            m_corr = '. Correction: Greenhouse-Geisser.'
            spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['gge']),
                                           pval = as.numeric(e_corrs_list[[m_name]]['ggp']))

        } else if (e_correction == 'hf') {
            m_corr = '. Correction: Huynh-Feldt.'
            spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['hfe']),
                                           pval = as.numeric(e_corrs_list[[m_name]]['hfp']))
        } else if (round(m_pval, 3) < 0.05) {
            this_gge = as.numeric(e_corrs_list[[m_name]]['gge'])
            if (round(this_gge, 3) > 0.75) {
                m_corr = '. Correction: Huynh-Feldt (Greenhouse-Geisser CHAR_EPS > 0.75).'
                spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['hfe']),
                                               pval = as.numeric(e_corrs_list[[m_name]]['hfp']))

            } else {
                m_corr = '. Correction: Greenhouse-Geisser (CHAR_EPS <= 0.75).'
                spher_real_corrs[[m_name]] = c(eps = as.numeric(e_corrs_list[[m_name]]['gge']),
                                               pval = as.numeric(e_corrs_list[[m_name]]['ggp']))
            }
        } else {
            m_corr = '.'
        }
        if (shush == FALSE) {
            prnt(m_name, ': W = ', ro(m_w, 3), ', p = ',  ro(m_pval, 3), m_corr)
        }
    }
    return(spher_real_corrs)
}

to_fact = function(var) {
    return(as.factor(tolower(as.character(var))))
}

to_c = function(var) {
    var = gsub("\\s", "", var)
    return(strsplit(var, ",")[[1]])
}

merge_cols = function(dat_aggred) {
    g_names = names(dat_aggred)[startsWith(names(dat_aggred), 'Group.')]
    if (length(g_names) > 1) {
        dat_aggred = eval(parse(
            text = paste0(
                "within(dat_aggred, g_merged <-
                paste(",
                paste(g_names, collapse = ','),
                ", sep = '_'))"
                )
            ))
        dat_aggred[, 1] = dat_aggred$g_merged
        colnames(dat_aggred)[1] <- "aggr_group"
        dat_aggred = dat_aggred[, setdiff(names(dat_aggred), c('g_merged', g_names))]
    } else {
        colnames(dat_aggred)[1] <- "aggr_group"
    }
    return(dat_aggred)
}

transp = function(to_transpose, headers) {
    if (headers == TRUE) {
        headers = 'aggr_group'
    }
    if (class(to_transpose) == "list") {
        to_transpose = Reduce(function(x, y)
            merge(x, y, all = TRUE), to_transpose)
    }
    hnames = to_transpose[[headers]]
    tdat = as.data.frame(t(to_transpose[, -1]))
    colnames(tdat) = hnames
    return(tdat)
}

mains_ebs = function(data_long, method, eb_method, g_by) {
    fact_names = to_c(g_by)
    g_by_text = paste0('with(data = data_long, list(',
                       g_by,
                       '))')
    group_by = eval(parse(text = g_by_text))
    if (is.null(eb_method)) {
        eb_method2 = mean
    } else {
        eb_method2 = eb_method
    }
    to_plot = do.call(
        data.frame,
        stats::aggregate(
            data_long$neat_unique_values,
            by = group_by,
            FUN = function(x) {
                c(main = method(x), eb = eb_method2(x))
            }
        )
    )
    for (c_name in names(to_plot)) {
        if (!c_name %in% c('x.main', 'x.eb')) {
            to_plot[[c_name]] = as.character(to_plot[[c_name]])
        }
    }
    return(to_plot)
}
re_n = function(name, n_dict) {
    return(if (is.null(n_dict) ||
               is.na(n_dict[name]))
        name
        else
            n_dict[name])
}

name_taken = function(name, dat) {
    if (name %in%  names(dat)) {
        stop(
            'Sorry, the name "',
            name,
            '" is reserved for this function. Remove or rename that column.'
        )
    }
}

## parameter argument valudations

validate_args = function(func_used, evaled_args) {
    feedback = ''
    for (part_feed in evaled_args) {
        feedback = paste0(feedback, part_feed)
    }
    if (feedback != '') {
        func_used = gsub("\\s+", " ", paste(deparse(func_used), collapse = " "))
        feedback = paste0(
            "Arguments are not correct in the '",
            func_used,
            "' function:",
            feedback,
            '\n... Hint: enter help(',
            strsplit(func_used, "\\(")[[1]][1],
            ') for detailed function info.'
        )
        stop(feedback, call. = FALSE)
    }
}

val_arg = function(arg_val,
                   req_types,
                   req_length = 99,
                   # 0 means multiple, 1 means single, all else passes
                   opts = NULL) {
    failed = FALSE
    arg_name = deparse(substitute(arg_val))
    if (length(arg_val) > 1) {
        if (req_length == 1 &&
            !is.list(arg_val) && !is.data.frame(arg_val)) {
            failed = TRUE
        }
    } else if (req_length == 0) {
        failed = TRUE
    }
    valid_types = c('char', 'num', 'bool', 'null', 'df', 'list', 'function')
    if (!all(req_types %in% valid_types)) {
        stop(
            'invalid req_types: ',
            paste(req_types, collapse = ', '),
            '\nshould be: ',
            paste(valid_types, collapse = ', ')
        )
    }
    req_types = replace(req_types, req_types == 'char', 'character')
    req_types = replace(req_types, req_types == 'num', 'double')
    req_types = replace(req_types, req_types == 'bool', 'logical')
    req_types = replace(req_types, req_types == 'null', 'NULL')
    req_types = replace(req_types, req_types == 'df', 'data.frame')
    if ((!typeof(arg_val) %in% req_types)
        && (!('data.frame' %in% req_types &&
              is.data.frame(arg_val)))
        && (!('function' %in% req_types &&
              is.function(arg_val))) &&
        (!('list' %in% req_types &&
           is.list(arg_val)))) {
        failed = TRUE
    } else if (typeof(arg_val) == 'character' &&
               (!is.null(opts)) && (!arg_val %in% opts)) {
        failed = TRUE
    }
    if (failed == TRUE) {
        req_types = replace(req_types,
                            req_types == 'character',
                            '"character" (string)')
        req_types = replace(req_types, req_types == 'double', '"double" (numeric)')
        req_types = replace(req_types,
                            req_types == 'logical',
                            '"logical" (boolean)')
        req_types = replace(req_types, req_types == 'data.frame', '"data.frame"')
        if (!is.null(opts)) {
            if (suppressWarnings(all(!is.na(as.numeric(opts))))) {
                opts_add = paste0(
                    ' The only acceptable strings or numbers are "',
                    paste(opts, collapse = '", or "'),
                    '".'
                )
            } else {
                opts_add = paste0(
                    ' The only acceptable strings are "',
                    paste(opts, collapse = '", or "'),
                    '".'
                )
            }
        } else {
            opts_add = ''
        }
        if (req_length == 1) {
            len_add = ' must not be a vector, and'
        } else if (req_length == 0) {
            len_add = ' must be a vector, and'
        } else {
            len_add = ''
        }
        arg_msg = paste0(
            '\nThe argument "',
            arg_name,
            '"',
            len_add,
            ' must be ',
            paste(req_types, collapse = ', or '),
            '.',
            opts_add
        )
        return(arg_msg)
    } else {
        return('')
    }
}

val_wi_id = function(func_used, id_arg, val_cols) {
    if (is.list(id_arg)) {
        arg_name = deparse(substitute(arg_val))
        func_used = gsub("\\s+", " ", paste(deparse(func_used), collapse = " "))
        if (length(id_arg) < 2) {
            feedback = paste0(
                '\nIf list is given as argument for "',
                arg_name,
                '", it must contain at least two elements.'
            )
        } else {
            feedback = ''
            vals_num = length(val_cols)
            w_facts_num = length(id_arg)
            if (2 ** w_facts_num > vals_num) {
                feedback = paste0(
                    '\nYou specified ',
                    w_facts_num,
                    ' within-subject factors in "',
                    arg_name,
                    '". This means there must be at least ',
                    2 ** w_facts_num,
                    ' values columns specified, but you only specified ',
                    vals_num,
                    '.'
                )
            }
            for (val_name in val_cols) {
                for (fact_name in names(id_arg)) {
                    fact_ids = id_arg[fact_name][[1]]
                    if (length(fact_ids) <= 1) {
                        feedback = paste0(
                            feedback,
                            '\nAll within-subject factors must have at least two levels. (Check "',
                            fact_name,
                            '").'
                        )
                    } else {
                        id_count = 0
                        for (f_id in fact_ids) {
                            if (grepl(f_id, val_name, fixed = TRUE)) {
                                id_count = id_count + 1
                            }
                        }
                        if (id_count == 0) {
                            feedback = paste0(
                                feedback,
                                '\nNo matching level found for "',
                                val_name,
                                '" for factor "',
                                fact_name,
                                '".'
                            )
                        } else if (id_count > 1) {
                            feedback = paste0(
                                feedback,
                                '\nMore than one matching level found for "',
                                val_name,
                                '" for factor "',
                                fact_name,
                                '". (This means that the specified factor name text is ambiguous,',
                                ' see "within_ids" in documentation e.g. by entering ?anova_neat.',
                                ' Try different naming for level specification, ',
                                'or change column names.)'
                            )
                        }

                    }
                }
            }
        }
        if (feedback != '') {
            feedback = paste0(
                "Arguments are not correct in the '",
                func_used,
                "' function:",
                feedback,
                '\n... Hint: enter help(',
                gsub('"', '', strsplit(func_used, "\\(")[[1]][1]),
                ') for detailed function info.'
            )
            stop(feedback, call. = FALSE)
        }
    }
}
