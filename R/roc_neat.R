#'@title Difference of Two Areas Under the Curves
#'
#'@description Comparison of two \code{\link[=t_neat]{areas under the receiver
#'  operating characteristic curves}} (AUCs).
#'@param roc1 Receiver operating characteristic (ROC) \code{\link[pROC:roc]{
#'  object}}.
#'@param roc2 Receiver operating characteristic (ROC) \code{\link[pROC:roc]{
#'  object}}.
#'@param pair Logical. If \code{TRUE}, the test is conducted for paired samples.
#'  Otherwise (default) for independent samples.
#'@param greater \code{NULL} or string (or number); optionally specifies
#'  one-sided test: either "1" (\code{roc1} AUC expected to be greater than
#'  \code{roc2} AUC) or "2" (\code{roc2} AUC expected to be greater than
#'  \code{roc2} AUC). If \code{NULL} (default), the test is two-sided.
#'@param ci Numeric; confidence level for the returned CIs (raw difference).
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'
#'@return Prints DeLong's test results for the comparison of the two given AUCs
#'  in APA style, as well as corresponding CI for the AUC difference.
#'  Furthermore, when assigned, returns a named vector with the following two
#'  elements: \code{stat} (D value), \code{p} (p value).
#'@note
#'The test statistics are calculated via
#'\code{\link[pROC:roc.test]{pROC::roc.test}} as DeLong's test (for both paired
#'and unpaired). The \code{roc_neat} function merely prints it in APA style.
#'
#'The ROC object may be calculated via \code{\link{t_neat}}, or directly with
#'\code{\link[pROC:roc]{pROC::roc}}.
#'
#'@references
#'
#'Altman, D. G., & Bland, J. M. (2011). How to obtain the confidence interval
#'from a P value. Bmj, 343(d2090). \doi{https://doi.org/10.1136/bmj.d2090}
#'
#'DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the
#'areas under two or more correlated receiver operating characteristic curves: a
#'nonparametric approach. Biometrics, 44(3), 837-845.
#'\doi{https://doi.org/10.2307/2531595}
#'
#'Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J. C., &
#'Muller, M. (2011). pROC: an open-source package for R and S+ to analyze and
#'compare ROC curves. BMC bioinformatics, 12(1), 77.
#'\doi{https://doi.org/10.1186/1471-2105-12-77}
#'
#' @seealso \code{\link{t_neat}}
#' @examples
#'
#' # calculate first AUC (from v1 and v2)
#' v1 = c(191, 115, 129, 43, 523,-4, 34, 28, 33,-1, 54)
#' v2 = c(4,-2, 23, 13, 32, 16, 3, 29, 37,-4, 65)
#' results1 = t_neat(v1, v2, auc_added = TRUE)
#'
#' # calculate second AUC (from v3 and v4)
#' v3 = c(14.1, 58.5, 25.5, 42.2, 13, 4.4, 55.5, 28.5, 25.6, 37.1)
#' v4 = c(36.2, 45.2, 41, 24.6, 30.5, 28.2, 40.9, 45.1, 31, 16.9)
#' results2 = t_neat(v3, v4, auc_added = TRUE)
#'
#' # one-sided comparison of the two AUCs
#' roc_neat(results1$roc_obj, results2$roc_obj, greater = "1")
#' @export

roc_neat = function(roc1,
                    roc2,
                    pair = FALSE,
                    greater = NULL,
                    ci = NULL,
                    hush = FALSE) {
    validate_args(
        match.call(),
        list(
            val_arg(pair, c('bool'), 1),
            val_arg(greater, c('null', 'char'), 1, c('1', '2')),
            val_arg(ci, c('null', 'num'), 1),
            val_arg(hush, c('bool'), 1)
        )
    )
    greater = toString(greater)
    if (greater == "1") {
        if (hush == FALSE) {
            message("One-sided test (with 90% CI default)! H1: first is greater than second.")
        }
        alt = "greater"
    } else if (greater == "2") {
        if (hush == FALSE) {
            message("One-sided test (with 90% CI default)! H1: second is greater than first.")
        }
        alt = "less"
    } else {
        alt = "two.sided"
        if (is.null(ci)) {
            ci = 0.95
        }
    }
    if (is.null(ci)) {
        ci = 0.90
    }
    roc_test = pROC::roc.test(roc1, roc2, paired = pair, alternative = alt)
    roc_stat = roc_test$statistic
    df = roc_test$parameter
    p_value = roc_test$p.value

    roc_test_ts = pROC::roc.test(roc1, roc2, paired = pair)
    auc_diff = as.numeric(roc1$auc) - as.numeric(roc2$auc)
    z_norm = -0.862 + sqrt(0.743 - 2.404 * log(roc_test_ts$p.value))
    auc_se = abs(auc_diff / z_norm)
    z_c = stats::qnorm(1 - (1 - ci) / 2)
    auc_low = auc_diff - auc_se * z_c
    auc_upp = auc_diff + auc_se * z_c
    ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")

    if (pair == FALSE) {
        out = paste0(
            "D(",
            ro(df, 1),
            ") = ",
            ro(roc_stat, 2),
            ", p = ",
            ro(p_value, 3),
            " (AUC difference: ",
            ro(auc_diff, 3, leading_zero = FALSE),
            ci_disp,
            " [",
            ro(auc_low, 3, leading_zero = FALSE),
            ", ",
            ro(auc_upp, 3, leading_zero = FALSE),
            "])"
        )
    } else {
        out = paste0(
            "D = ",
            ro(roc_stat, 2),
            ", p = ",
            ro(p_value, 3),
            " (AUC difference: ",
            ro(auc_diff, 3, leading_zero = FALSE),
            ci_disp,
            " [",
            ro(auc_low, 3, leading_zero = FALSE),
            ", ",
            ro(auc_upp, 3, leading_zero = FALSE),
            "])"
        )
    }
    if (hush == FALSE) {
        prnt(out)
    }
    invisible(c(stat = as.numeric(roc_stat), p = p_value))
}
