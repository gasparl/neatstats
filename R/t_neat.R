#'@title Difference of Two Means and Area Under the Curve
#'
#'@description Welch's \code{\link[stats:t.test]{t-test}} results
#'  including Cohen's d with confidence interval (CI),
#'  \code{\link[BayesFactor:ttestBF]{Bayes factor}} (BF), and
#'  \code{\link[pROC:auc]{area under the receiver operating characteristic
#'  curve}} (AUC).
#'@param var1 Numeric vector; numbers of the first variable.
#'@param var2 Numeric vector; numbers of the second variable.
#'@param pair Logical. If \code{TRUE}, all tests (t, BF, AUC) are conducted for
#'  paired samples. If \code{FALSE} (default) for independent samples.
#'@param greater \code{NULL} or string (or number); optionally specifies
#'  one-sided tests (t and BF): either "1" (\code{var1} mean expected to be
#'  greater than \code{var2} mean) or "2" (\code{var2} mean expected to be
#'  greater than \code{var1} mean). If \code{NULL} (default), the test is
#'  two-sided.
#'@param ci Numeric; confidence level for returned CIs for Cohen's d and AUC.
#'@param bf_added Logical. If \code{TRUE} (default), Bayes factor is calculated
#'  and displayed.
#'@param auc_added Logical. If \code{TRUE}, AUC is calculated and displayed.
#'  (\code{FALSE} by default.)
#'@param r_added Logical. If \code{TRUE} (default), Pearson correlation is
#'  calculated and displayed in case of paired comparison.
#'@param for_table Logical. If \code{TRUE}, omits the confidence level display
#'  from the printed text.
#'@param test_title String, "Descriptives:" by default. Simply displayed in
#'  printing preceding the descriptive statistics. (Useful e.g. to distinguish
#'  several different comparisons inside a \code{function} or a \code{for}
#'  loop.)
#'@param round_descr Number \code{\link[=ro]{to round}} to the descriptive
#'  statistics (means and SDs).
#'@param round_auc Number \code{\link[=ro]{to round}} to the AUC and its CI.
#'@param auc_greater String (or number); specifies which variable is expected to
#'  have greater values for 'cases' as opposed to 'controls': "1" (default;
#'  \code{var1} expected to be greater for 'cases' than \code{var2} mean) or "2"
#'  (\code{var2} expected to be greater for 'cases' than \code{var1}). Not to be
#'  confused with one-sided tests; see Details.
#'@details
#' The Bayes factor (BF) is always calculated with the default r-scale of
#' \code{0.707}. BF supporting null hypothesis is denoted as BF01, while that
#' supporting alternative hypothesis is denoted as BF10. When the BF is smaller
#' than 1 (i.e., supports null hypothesis), the reciprocal is calculated (hence,
#' BF10 = BF, but BF01 = 1/BF). When the BF is greater than or equal to 10000,
#' scientific (exponential) form is reported for readability. (The original full
#' BF number is available in the returned named vector as \code{bf}.)
#'
#'The original \code{\link[pROC:auc]{pROC::auc}} function, by default, always
#'returns an AUC greater than (or equal to) .5, assuming that the prediction
#'based on values in the expected direction work correctly at least at chance
#'level. This however may be confusing. Consider an example where we measure the
#'heights of persons in a specific small sample and expect that greater height
#'predicts masculine gender. The results are, say, 169, 175, 167, 164 (cm) for
#'one gender, and 176, 182, 179, 165 for the other. If the expectation is
#'correct (the second, greater values are for males), the AUC is .812. However,
#'if in this particular population females are actually taller than males, the
#'AUC is in fact .188. To keep things clear, the \code{t_neat} function always
#'makes an assumption about which variable is expected to be greater for correct
#'classification ("1" by default; i.e., \code{var1}; to be specified as
#'\code{auc_greater = "2"} for \code{var2} to be expected as greater). For this
#'example, if the first (smaller) variables are given as \code{var1} for
#'females, and second (larger), variables are given as \code{var2} for males, we
#'have to specify \code{auc_greater = "2"} to indicate the expectation of larger
#'values for males. (Or, easier, just add the expected larger values as
#'\code{var1}.)
#'
#'@return Prints t-test statistics (including Cohen'd with CI, BF, and AUC, as
#'  specified via the corresponding parameters) in APA style. Furthermore, when
#'  assigned, returns a list, that contains a named vector '\code{stats}' with
#'  the following elements: \code{t} (t value), \code{p} (p value), \code{d}
#'  (Cohen's d), \code{bf} (Bayes factor), \code{auc} (AUC), \code{accuracy}
#'  (accuracy using the most optimal classification threshold). The latter two
#'  is \code{NULL} when \code{auc_added} is \code{FALSE}. When \code{auc_added}
#'  is \code{TRUE}, there are also two additional elements of the list. One is
#'  '\code{roc_obj}', which is a \code{\link[pROC]{roc}} object, to be used e.g.
#'  with the \code{\link{roc_neat}} function. The other is
#'  '\code{best_thresholds}', which contains the best threshold value(s) for
#'  classification, along with corresponding specificity and sensitivity.
#'
#'@note The Welch's t-test is calculated via
#'\code{\link[stats:t.test]{stats::t.test}}.
#'
#'Cohen's d and its confidence interval are calculated, using the t value, via
#'\code{\link[MBESS:ci.smd]{MBESS::ci.smd}} for independent samples (as
#'standardized mean difference) and via \code{\link[MBESS:ci.sm]{MBESS::ci.sm}}
#'for paired samples (as standardized mean).
#'
#'The Bayes factor is calculated via
#'\code{\link[BayesFactor:ttestBF]{BayesFactor::ttestBF}}.
#'
#'The correlation and its CI are calculated via
#'\code{\link[stats:cor.test]{stats::cor.test}}, and is always two-sided, always
#'with 95 percent CI. For more, use \code{\link{corr_neat}}.
#'
#'The AUC and its CI are calculated via \code{\link[pROC:auc]{pROC::auc}}, and
#'the accuracy at most optimal threshold via
#'\code{\link[pROC:coords]{pROC::coords}} (\code{x = "best"}); both using the
#'object \code{\link[pROC:roc]{pROC::roc}}.
#'
#'@references Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists
#'should by default use Welch's t-test instead of Student's t-test.
#'International Review of Social Psychology, 30(1).
#'\doi{http://doi.org/10.5334/irsp.82}
#'
#'Kelley, K. (2007). Methods for the behavioral, educational, and social
#'sciences: An R package. Behavior Research Methods, 39(4), 979-984.
#'\doi{https://doi.org/10.3758/BF03192993}
#'
#'Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J. C., &
#'Muller, M. (2011). pROC: an open-source package for R and S+ to analyze and
#'compare ROC curves. BMC bioinformatics, 12(1), 77.
#'\doi{https://doi.org/10.1186/1471-2105-12-77}
#'
#' @seealso \code{\link{corr_neat}}, \code{\link{roc_neat}}
#' @examples
#' # assign two variables (numeric vectors)
#' v1 = c(191, 115, 129, 43, 523,-4, 34, 28, 33,-1, 54)
#' v2 = c(4,-2, 23, 13, 32, 16, 3, 29, 37,-4, 65)
#'
#' t_neat(v1, v2) # prints results as independent samples
#' t_neat(v1, v2, pair = TRUE) # as paired samples (r added by default)
#' t_neat(v1, v2, pair = TRUE, greater = "1") # one-sided
#' t_neat(v1, v2, pair = TRUE, auc_added = TRUE ) # AUC included
#'
#' # print results and assign returned list
#' results = t_neat(v1, v2, pair = TRUE)
#'
#' results$stats['bf'] # get precise BF value
#'
#' @export
t_neat = function(var1,
                  var2,
                  pair = FALSE,
                  greater = NULL,
                  ci = NULL,
                  bf_added = TRUE,
                  auc_added = FALSE,
                  r_added = TRUE,
                  for_table = FALSE,
                  test_title = "Descriptives:",
                  round_descr = 2,
                  round_auc = 3,
                  auc_greater = '1') {
    validate_args(
        match.call(),
        list(
            val_arg(var1, c('num'), 0),
            val_arg(var2, c('num'), 0),
            val_arg(pair, c('bool'), 1),
            val_arg(greater, c('null', 'char'), 1, c('1', '2')),
            val_arg(ci, c('num'), 1),
            val_arg(bf_added, c('bool'), 1),
            val_arg(auc_added, c('bool'), 1),
            val_arg(r_added, c('bool'), 1),
            val_arg(for_table, c('bool'), 1),
            val_arg(test_title, c('char'), 1),
            val_arg(round_descr, c('num'), 1),
            val_arg(round_auc, c('num'), 1),
            val_arg(auc_greater, c('char'), 1, c('1', '2'))
        )
    )
    greater = toString(greater)
    descr_1 = paste0(ro(mean(var1), round_descr),
                     "CHAR_PLUSMIN",
                     ro(stats::sd(var1), round_descr))
    descr_2 = paste0(ro(mean(var2), round_descr),
                     "CHAR_PLUSMIN",
                     ro(stats::sd(var2), round_descr))
    if (pair == TRUE & r_added == TRUE) {
        cat("Correlation: ")
        corr_neat(var1, var2, ci = 0.95, bf_added = FALSE)
    }
    prnt(test_title, " MCHAR_PLUSMINSD = ", descr_1, " vs. ", descr_2)
    if (greater == "1") {
        message("One-sided t-test and BF (with 90% CI default)! H1: first is greater than second.")
        ttest = stats::t.test(var1, var2, paired = pair, alternative = "greater")
        if (bf_added == TRUE) {
            bf = as.vector(BayesFactor::ttestBF(
                var1,
                var2,
                paired = pair,
                nullInterval = c(0, Inf)
            )[1])
        }
    } else if (greater == "2") {
        message("One-sided t-test and BF (with 90% CI default)! H1: second is greater than first.")
        ttest = stats::t.test(var1, var2, paired = pair, alternative = "less")
        if (bf_added == TRUE) {
            bf = as.vector(BayesFactor::ttestBF(
                var1,
                var2,
                paired = pair,
                nullInterval = c(0,-Inf)
            )[1])
        }
    } else {
        ttest = stats::t.test(var1, var2, paired = pair)
        if (bf_added == TRUE) {
            bf = as.vector(BayesFactor::ttestBF(var1, var2, paired = pair))
        }
        if (is.null(ci)) {
            ci = 0.95
        }
    }
    if (is.null(ci)) {
        ci = 0.90
    }
    if (bf_added == TRUE) {
        bf_out = bf_neat(bf)
    } else {
        bf_out = "."
        bf = NA
    }
    t = as.vector(ttest$statistic)
    df = as.vector(ttest$parameter)
    pvalue = ttest$p.value
    n1 = length(var1)
    n2 = length(var2)
    if (pair == TRUE) {
        sm = quiet(MBESS::ci.sm(
            ncp = ttest$statistic,
            N = n1,
            conf.level = ci
        ))
        d_orig = sm$Standardized.Mean
        d = paste0("d = ", ro(d_orig, 2))
        df = ro(df, 0)
        lower = ro(sm$Lower.Conf.Limit.Standardized.Mean, 2)
        upper = ro(sm$Upper.Conf.Limit.Standardized.Mean, 2)
    } else {
        the_smd = MBESS::ci.smd(
            ncp = t,
            n.1 = n1,
            n.2 = n2,
            conf.level = ci
        )
        d_orig = the_smd$smd
        d = paste0("d = ", ro(d_orig, 2))
        df = ro(df, 1)
        lower = ro(the_smd$Lower.Conf.Limit.smd, 2)
        upper = ro(the_smd$Upper.Conf.Limit.smd, 2)
    }
    if (for_table == TRUE) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
    }
    out = paste0(
        "t(",
        df,
        ") = ",
        ro(t, 2),
        ", p = ",
        ro(pvalue, 3),
        ", ",
        d,
        ci_disp,
        " [",
        lower,
        ", ",
        upper,
        "]",
        bf_out
    )
    prnt(out)
    if (auc_added == TRUE) {
        if (auc_greater == "2") {
            auc_dir = ">" # v2 expected larger
        } else {
            auc_dir = "<" # v1 expected larger
        }
        the_roc = pROC::roc(
            response = c(rep(0, length(var2)), rep(1, length(var1))),
            predictor = c(var2, var1),
            levels = c(0, 1),
            direction =  auc_dir
        ) # v1 larger
        show_auc(
            theroc = the_roc,
            ci = ci,
            round_to = round_auc,
            for_table = for_table
        )
        max_acc = as.numeric(pROC::coords(the_roc, x = "best", ret = "accuracy"))[1]
        best_coords = pROC::coords(the_roc, x = "best")
        invisible(list(
            stats = c(
                t = as.numeric(t),
                p = pvalue,
                d = as.numeric(d_orig),
                bf = as.numeric(bf),
                auc = pROC::auc(the_roc),
                accuracy = max_acc
            ),
            roc_obj = the_roc,
            best_thresholds = best_coords
        ))
    } else {
        invisible(list(
            stats = c(
                t = as.numeric(t),
                p = pvalue,
                d = as.numeric(d_orig),
                bf = as.numeric(bf),
                auc = NULL,
                accuracy = NULL
            ),
            roc_obj = NULL,
            best_thresholds = NULL
        ))
    }
}
