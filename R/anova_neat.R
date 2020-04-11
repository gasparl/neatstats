#'@title Comparison of Multiple Means: ANOVA
#'
#'@description \code{\link[ez:ezANOVA]{Analysis of variance}} (ANOVA) F-test
#'  results with appropriate \code{\link[stats:oneway.test]{Welch}}'s and
#'  epsilon corrections where applicable (unless specified otherwise), including
#'  partial eta squared effect sizes with confidence intervals (CIs),
#'  generalized eta squared, and
#'  \code{\link[bayestestR:bayesfactor_inclusion]{inclusion Bayes factor based
#'  on matched models}} (BFs).
#'@param data_per_subject Data frame or name of data frame as string. Should
#'  contain all values (measurements/observations) in a single row per each
#'  subject.
#'@param values Vector of strings; column name(s) in the \code{data_per_subject}
#'  data frame. Each column should contain a single dependent variable: thus, to
#'  test repeated (within-subject) measurements, each specified column should
#'  contain one measurement.
#'@param within_ids \code{NULL} (default), string, or named list. In case of no
#'  within-subject factors, leave as \code{NULL}. In case of a single within
#'  subject factor, a single string may be given to optionally provide custom
#'  name for the within-subject factor (note: this is a programming variable
#'  name, so it should not contain spaces, etc.); otherwise (if left
#'  \code{NULL}) this one within-subject factor will always just be named
#'  \code{"within_factor"}. In case of multiple within-subject factors, each
#'  factor must be specified as a named list element, each with a vector of
#'  strings that distinguish the levels within that factors. The column names
#'  given as \code{values} should always contain one (and only one) of these
#'  strings within each within-subject factor, and thus they will be assigned
#'  the appropriate level. For example, \code{values = 'rt_s1_neg, rt_s1_pos,
#'  rt_s2_neg, rt_s2_pos'} could have \code{within_ids = list( session = c('s1',
#'  's2'), valence =  c('pos', 'neg')}. (Note: the strings for distinguishing
#'  must be unambiguous. E.g., for values \code{apple_a} and \code{apple_b}, do
#'  not set levels \code{c('a','b')}, because \code{'a'} is also found in
#'  \code{apple_b}. In this case, you could choose levels \code{c('_a','_b')} to
#'  make sure the values are correctly distinguished.) See also Examples.
#'@param between_vars \code{NULL} (default; in case of no between-subject
#'  factors) or vector of strings; column name(s) in the \code{data_per_subject}
#'  data frame. Each column should contain a single between-subject independent
#'  variable (representing between-subject factors).
#'@param ci Numeric; confidence level for returned CIs. (Default: \code{.9};
#'  Lakens, 2014; Steiger, 2004.)
#'@param bf_added Logical. If \code{TRUE} (default), inclusion Bayes factor is
#'  calculated and displayed. (Note: with multiple factors and/or larger
#'  dataset, the calculation can take considerable time.)
#'@param test_title String, \code{"--- neat ANOVA ---"} by default. Simply
#'  displayed in printing preceding the statistics.
#'@param welch If \code{TRUE} (default), calculates Welch's ANOVA via
#'  \code{\link[stats:oneway.test]{stats::oneway.test}} in case of a single
#'  factor (one-way) between-subject design. If \code{FALSE}, calculates via
#'  \code{\link[ez:ezANOVA]{ez::ezANOVA}} in such cases too (i.e., same as in
#'  case of every other design).
#'@param e_correction \code{NULL} (default) or one of the following strings:
#'  \code{'gg'}, \code{'hf'}, or \code{'none'}. If set to \code{'gg'},
#'  Greenhouse-Geisser correction is applied in case of repeated measures
#'  (regardless of violation of sphericity). If set to \code{'hf'}, Huynh-Feldt
#'  correction is applied. If set to \code{'none'}, no correction is applied. If
#'  \code{NULL}, Greenhouse-Geisser correction is applied when Mauchly's
#'  sphericity test is significant and the Greenhouse-Geisser epsilon is not
#'  larger than \code{.75}, while Huynh-Feldt correction is applied when
#'  Mauchly's sphericity test is significant and the Greenhouse-Geisser epsilon
#'  is larger than \code{.75} (see Girden, 1992).
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'
#'@details
#'
#'The Bayes factor (BF) is always calculated with the default \code{rscaleFixed}
#'of \code{0.5} (\code{"medium"}) and \code{rscaleRandom} of \code{1}
#'(\code{"nuisance"}). BF supporting null hypothesis is denoted as BF01, while
#'that supporting alternative hypothesis is denoted as BF10. When the BF is
#'smaller than 1 (i.e., supports null hypothesis), the reciprocal is calculated
#'(hence, BF10 = BF, but BF01 = 1/BF). When the BF is greater than or equal to
#'10000, scientific (exponential) form is reported for readability. (The
#'original full BF number is available in the returned named vector as
#'\code{bf}.)
#'
#'Levene's test is returned for between-subject design without Welch's
#'correction, while Mauchly's sphericity test is returned for repeated measures
#'with more than two levels. If Mauchly's test is significant, epsilon
#'correction may be applied (see the \code{e_correction} parameter). If Levene's
#'test is significant and the data is unbalanced (unequal group sample sizes),
#'you should either consider the results respectively or choose a different
#'test.
#'
#'@return Prints ANOVA statistics (including, for each model, F-test with
#'  partial eta squared and its CI, generalized eta squared, and BF, as
#'  specified via the corresponding parameters) in APA style. Furthermore, when
#'  assigned, returns a list with up to three elements. First,
#'  '\code{stat_list}', a list of named vectors per each effect (main or
#'  interaction). Each vector contains the following elements: \code{F} (F
#'  value), \code{p} (p value), \code{petas} (partial eta squared), \code{getas}
#'  (generalized eta squared), \code{epsilon} (epsilon used for correction), and
#'  \code{bf} (inclusion BF; when \code{bf_added} is not \code{FALSE}). Second,
#'  the \code{\link[ez]{ezANOVA}} object, named \code{ez_anova} (calculated even
#'  when \code{\link[stats]{oneway.test}} is printed). Third, when
#'  \code{bf_added} is not \code{FALSE}, the \code{\link[BayesFactor]{anovaBF}}
#'  object, named \code{bf_models}; including all models on which the inclusion
#'  BFs are based.
#'
#'@note All F-tests are calculated via \code{\link[ez:ezANOVA]{ez::ezANOVA}},
#'  including Levene's test and Mauchly's sphericity test. (But Welch's ANOVA is
#'  calculated in case of one-way between-subject designs via
#'  \code{\link[stats:oneway.test]{stats::oneway.test}}, unless the \code{welch}
#'  parameter is set to \code{FALSE}.)
#'
#'  Confidence intervals are calculated, using the F value, via
#'  \code{\link[MBESS:conf.limits.ncf]{MBESS::conf.limits.ncf}}, converting
#'  noncentrality parameters to partial eta squared as \code{ncp/(ncp+
#'  df_nom+df_denom+1)} (Smithson, 2003).
#'
#'  Generalized eta squared is to facilitate potential subsequent
#'  meta-analytical comparisons (see Lakens, 2013).
#'
#'  The inclusion Bayes factor based on matched models is calculated via
#'  \code{\link[bayestestR:bayesfactor_inclusion]{bayestestR::bayesfactor_inclusion}},
#'   (with \code{match_models = TRUE}, and using an
#'  \code{\link[BayesFactor:anovaBF]{BayesFactor::anovaBF}} object for
#'  \code{models} input).
#'
#'
#'@references
#'
#'Girden, E. (1992). ANOVA: Repeated measures. Newbury Park, CA: Sage.
#'
#'Kelley, K. (2007). Methods for the behavioral, educational, and social
#'sciences: An R package. Behavior Research Methods, 39(4), 979-984.
#'\doi{https://doi.org/10.3758/BF03192993}
#'
#'Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
#'cumulative science: A practical primer for t-tests and ANOVAs. Frontiers in
#'Psychology, 4. https://doi.org/10.3389/fpsyg.2013.00863
#'
#'Lakens, D. (2014). Calculating confidence intervals for Cohen's d and
#'eta-squared using SPSS, R, and Stata [Blog post]. Retrieved from
#'\url{http://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html}
#'
#'Mathot. S. (2017). Bayes like a Baws: Interpreting Bayesian Repeated Measures
#'in JASP [Blog post]. Retrieved from
#'\url{https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp}
#'
#'McDonald, J. H. 2015. Handbook of Biological Statistics (3rd ed.). Sparky House Publishing, Baltimore, Maryland. Retrieved from \url{http://www.biostathandbook.com}
#'
#'Moder, K. (2010). Alternatives to F-test in one way ANOVA in case of heterogeneity of variances (a simulation study). Psychological Test and Assessment Modeling, 52(4), 343-353.
#'
#'Navarro, D. (2013). Learning Statistics with R: A Tutorial for Psychology
#'Students and Other Beginners (Version 0.6.1). Retrieved from
#'\url{https://learningstatisticswithr.com/}
#'
#'Smithson, M. (2003). Confidence intervals. Thousand Oaks, Calif: Sage
#'Publications.
#'
#'Steiger, J. H. (2004). Beyond the F test: effect size confidence intervals and
#'tests of close fit in the analysis of variance and contrast analysis.
#'Psychological Methods, 9(2), 164-182.
#'\doi{https://doi.org/10.1037/1082-989X.9.2.164}
#'
#' @seealso \code{\link{plot_neat}}, \code{\link{t_neat}}
#' @examples
#' # assign random data in a data frame for illustration
#' # (note that the 'subject' is only for illustration; since each row contains the
#' # data of a single subject, no additional subject id is needed)
#' dat_1 = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     grouping1 = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'     grouping2 = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 1),
#'     value_1_a = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9),
#'     value_2_a = c(-14.1, 58.5, -25.5, 42.2, -13, 4.4, 55.5, -28.5, 25.6, -37.1),
#'     value_1_b = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     value_2_b = c(8.024, -14.162, 3.1, -2.1, -1.5, 0.91, 11.53, 18.37, 0.3, -0.59),
#'     value_1_c = c(27.4,-17.6,-32.7, 0.4, 37.2, 1.7, 18.2, 8.9, 1.9, 0.4),
#'     value_2_c = c(7.7, -0.8, 2.2, 14.1, 22.1, -47.7, -4.8, 8.6, 6.2, 18.2)
#' )
#' head(dat_1) # see what we have
#'
#' # For example, numbers '1' and '2' in the variable names of the values can
#' # denote sessions in an experiment, such as '_1' for first session, and '_2 for
#' # second session'. The letters '_a', '_b', '_c' could denote three different
#' # types of techniques used within each session, to be compared to each other.
#' # See further below for a more verbose but more meaningful example data.
#'\donttest{
#' # get the between-subject effect of 'grouping1'
#' anova_neat(dat_1, values = 'value_1_a', between_vars = 'grouping1')
#'
#' # main effects of 'grouping1', 'grouping2', and their interactions
#' anova_neat(dat_1,
#'            values = 'value_1_a',
#'            between_vars = c('grouping1', 'grouping2'))
#'
#' # repeated measures:
#' # get the within-subject effect for 'value_1_a' vs. 'value_1_b'
#' anova_neat(dat_1, values = c('value_1_a', 'value_1_b'))
#'
#' # same, but give the factor a custom variable name, and omit BF for speed
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b'),
#'     within_ids = 'a_vs_b',
#'     bf_added = FALSE
#' )
#' # or
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b'),
#'     within_ids = 'letters',
#'     bf_added = FALSE
#' )
#'
#' # within-subject effect for 'value_1_a' vs. 'value_1_b' vs. 'value_1_c'
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     bf_added = FALSE
#' )
#'}
#' # within-subject main effect for 'value_1_a' vs. 'value_1_b' vs. 'value_1_c',
#' # between-subject main effect 'grouping1', and the interaction of these two main
#' # effects
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_1_b', 'value_1_c'),
#'     between_vars = 'grouping1',
#'     bf_added = FALSE
#' )
#'
#' # within-subject 'number' main effect for variables with number '1' vs. number
#' # '2' ('value_1_a' and 'value_1_b' vs. 'value_2_a' and 'value_2_b'), 'letter'
#' # main effect for variables with final letterr 'a' vs. final letter 'b'
#' # ('value_1_a' and 'value_2_a' vs. 'value_1_b' and 'value_2_b'), and the
#' # 'letter' x 'number' interaction
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     ),
#'     bf_added = FALSE
#' )
#'\donttest{
#' # same as above, but now including between-subject main effect 'grouping2' and
#' # its interactions
#' anova_neat(
#'     dat_1,
#'     values = c('value_1_a', 'value_2_a', 'value_1_b', 'value_2_b'),
#'     within_ids = list(
#'         letters = c('_a', '_b'),
#'         numbers =  c('_1', '_2')
#'     ),
#'     between_vars = 'grouping2',
#'     bf_added = FALSE
#' )
#'
#' # In real datasets, these could of course be more meaningful. For example, let's
#' # say participants rated the attractiveness of pictures with low or high levels
#' # of frightening and low or high levels of disgusting qualities. So there are
#' # four types of ratings:
#' # 'low disgusting, low frightening' pictures
#' # 'low disgusting, high frightening' pictures
#' # 'high disgusting, low frightening' pictures
#' # 'high disgusting, high frightening' pictures
#'
#' # this could be meaningfully assigned e.g. as below
#' pic_ratings = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     rating_fright_low_disgust_low = c(36.2,45.2,41,24.6,30.5,28.2,40.9,45.1,31,16.9),
#'     rating_fright_high_disgust_low = c(-14.1,58.5,-25.5,42.2,-13,4.4,55.5,-28.5,25.6,-37.1),
#'     rating_fright_low_disgust_high = c(83,71,111,70,92,75,110,111,110,85),
#'     rating_fright_high_disgust_high = c(8.024,-14.162,3.1,-2.1,-1.5,0.91,11.53,18.37,0.3,-0.59)
#' )
#' head(pic_ratings) # see what we have
#'
#' # the same logic applies as for the examples above, but now the
#' # within-subject differences can be more meaningfully specified, e.g.
#' # 'disgust_low' vs. 'disgust_high' for levels of disgustingness, while
#' # 'fright_low' vs. 'fright_high' for levels of frighteningness
#' anova_neat(
#'     pic_ratings,
#'     values = c(
#'         'rating_fright_low_disgust_low',
#'         'rating_fright_high_disgust_low',
#'         'rating_fright_low_disgust_high',
#'         'rating_fright_high_disgust_high'
#'     ),
#'     within_ids = list(
#'         disgustingness = c('disgust_low', 'disgust_high'),
#'         frighteningness =  c('fright_low', 'fright_high')
#'     ),
#'     bf_added = FALSE
#' )
#'
#' # the results are the same as for the analogous test for the 'dat_1' data, only
#' # with different names
#'
#' # now let's say the ratings were done in two separate groups
#' pic_ratings = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     group_id = c(1, 2, 1, 2, 2, 1, 1, 1, 2, 1),
#'     rating_fright_low_disgust_low = c(36.2,45.2,41,24.6,30.5,28.2,40.9,45.1,31,16.9),
#'     rating_fright_high_disgust_low = c(-14.1,58.5,-25.5,42.2,-13,4.4,55.5,-28.5,25.6,-37.1),
#'     rating_fright_low_disgust_high = c(83,71,111,70,92,75,110,111,110,85),
#'     rating_fright_high_disgust_high = c(8.024,-14.162,3.1,-2.1,-1.5,0.91,11.53,18.37,0.3,-0.59)
#' )
#'
#' # now test the effect and interactions of 'group_id'
#' anova_neat(
#'     pic_ratings,
#'     values = c(
#'         'rating_fright_low_disgust_low',
#'         'rating_fright_high_disgust_low',
#'         'rating_fright_low_disgust_high',
#'         'rating_fright_high_disgust_high'
#'     ),
#'     within_ids = list(
#'         disgustingness = c('disgust_low', 'disgust_high'),
#'         frighteningness =  c('fright_low', 'fright_high')
#'     ),
#'     between_vars = 'group_id',
#'     bf_added = FALSE
#' )
#'
#' # again, same results as with 'dat_1' (using 'grouping2' as group_id)
#'}
#' @export
anova_neat = function(data_per_subject,
                      values,
                      within_ids = NULL,
                      between_vars = NULL,
                      ci = 0.90,
                      bf_added = TRUE,
                      test_title = "--- neat ANOVA ---",
                      welch = TRUE,
                      e_correction = NULL,
                      hush = FALSE) {
    if (class(data_per_subject) == "character") {
        data_wide = eval(parse(text = data_per_subject))
        data_per_subject = data_wide
    } else {
        data_wide = data_per_subject
    }
    validate_args(
        match.call(),
        list(
            val_arg(data_per_subject, c('df')),
            val_arg(values, c('char')),
            val_arg(within_ids, c('null', 'char', 'list'), 1),
            val_arg(between_vars, c('null', 'char')),
            val_arg(ci, c('num'), 1),
            val_arg(bf_added, c('bool'), 1),
            val_arg(test_title, c('char'), 1),
            val_arg(welch, c('bool'), 1),
            val_arg(e_correction, c('null', 'char'), 1, c('gg', 'hf', 'none')),
            val_arg(hush, c('bool'), 1)
        )
    )
    cols_notfound = c()
    if (!is.null(between_vars)) {
        for (colname in between_vars) {
            if (!colname %in% names(data_per_subject)) {
                cols_notfound = c(cols_notfound, colname)
            }
        }
        between_vars = paste(between_vars, collapse = ',')
    }
    for (colname in values) {
        if (!colname %in% names(data_per_subject)) {
            cols_notfound = c(cols_notfound, colname)
        }
    }
    if (length(cols_notfound) > 0) {
        if (length(cols_notfound) ==  1) {
            stop(
                'The column "',
                cols_notfound,
                '" was not found in the data frame. Perhaps check for spelling mistakes.'
            )
        } else {
            stop(
                'The following columns were not found in the data frame: "',
                paste(cols_notfound,
                      collapse = '", "'),
                '". Perhaps check for spelling mistakes.'
            )
        }
    }
    val_wi_id(match.call(), within_ids, values)
    if (is.null(e_correction)){
        e_correction = ''
    }
    name_taken('within_factor', data_wide)
    name_taken('neat_unique_values', data_wide)
    name_taken('neat_unique_id', data_wide)
    id_col = 'neat_unique_id'
    data_wide[[id_col]] = as.character(seq.int(nrow(data_wide)))
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
                    data_reshaped[[fact_name]][grepl(fact_x, data_reshaped$within_factor)] = fact_x
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
        if (length(to_c(between_vars)) == 1 && welch != FALSE) {
            w_anova = eval(parse(
                text = paste0(
                    'stats::oneway.test(',
                    value_col,
                    ' ~ ',
                    between_vars,
                    ', data = this_data, var.equal = FALSE  )'
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

    if (bf_added == TRUE) {
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
        if (is.null(within_vars) && length( to_c(between_vars) ) == 1 ) {
            bf_inc = as.vector(bf)
        } else {
            bf_inc = bayestestR::bayesfactor_inclusion(bf, match_models = TRUE)
            bf_inc = stats::setNames(object = bf_inc$BF, nm = rownames(bf_inc))
        }
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
        e_correction = e_correction,
        hush = hush
    )
    invisible(to_return)
}

anova_apa = function(ezANOVA_out,
                     ci = 0.90,
                     bf_added = NULL,
                     bf_models = NULL,
                     test_title = "--- neat ANOVA ---",
                     welch = NULL,
                     e_correction = '',
                     hush = FALSE) {
    levene = ezANOVA_out$"Levene's Test for Homogeneity of Variance"
    ezANOVA_out$ANOVA$'p<.05' = NULL
    if ((!is.null(levene)) && (is.null(welch))) {
        if ('Levene' %in%  ezANOVA_out$ANOVA$Effect) {
            stop(
                'Sorry, the name "Levene" is reserved for this function. Remove or rename this factor.'
            )
        }
        levene_post = ''
        levene$'p<.05' = NULL
        levene$ges = NA
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
    if (hush == FALSE) {
        prnt(test_title)
    }
    mauchly = ezANOVA_out$"Mauchly's Test for Sphericity"
    if (!is.null(mauchly)) {
        if (hush == FALSE) {
            prnt("- Mauchly's sphericity test:")
        }
        eps_p_corrs = get_e_corrs(mauchly,
                                  ezANOVA_out$"Sphericity Corrections",
                                  e_correction, hush)
        if (hush == FALSE) {
            prnt("- ANOVA:")
        }
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
        getas = ezANOVA_out$ANOVA$ges[indx]
        nG2 = sub('.', '', ro(getas, 3))

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
                "], CHAR_ETAG2 = ",
                nG2,
                bf_out,
                " (",
                f_name,
                ")"
            )
        }
        if (hush == FALSE) {
            prnt(out)
        }
        s_name = gsub(" CHAR_X ", "_", f_name)
        stat_list[[s_name]] = c(
            F = as.numeric(F_val),
            p = pvalue,
            epsilon = eps_added,
            petas = as.numeric(petas),
            getas = as.numeric(getas),
            bf = as.numeric(bf_val)
        )
    }
    stat_list = c(stat_list, ez_anova = ezANOVA_out, bf_models = bf_models)
    invisible(stat_list)
}
