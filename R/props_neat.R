#'@title Difference of Two Proportions
#'
#'@description \code{\link[Exact:exact.test]{ Unconditional exact test}} results
#'  for the comparison of two independent proportions, including confidence
#'  interval (CI) for the proportion difference, and corresponding
#'  \code{\link[BayesFactor:contingencyTableBF]{independent multinomial
#'  contingency table Bayes factor}} (BF). Cohen's h and its CI are also
#'  calculated.
#'@param case1 Number of 'cases' (as opposed to 'controls'; e.g. positive
#'  outcomes vs. negative outcomes) in 'group 1'.
#'@param case2 Number of 'cases' in 'group 2'.
#'@param n1 Number; sample size of 'group 1'.
#'@param n2 Number; sample size of 'group 2'.
#'@param greater \code{NULL} or string (or number); optionally specifies
#'  one-sided exact test: either "1" (\code{case1/n1} proportion expected to be
#'  greater than \code{case2/n2} proportion) or "2" (\code{case2/n2} proportion
#'  expected to be greater than \code{case1/n1} proportion). If \code{NULL}
#'  (default), the test is two-sided.
#'@param ci Numeric; confidence level for the returned CIs (proportion
#'  difference and Cohen's h).
#'@param bf_added Logical. If \code{TRUE} (default), Bayes factor is calculated
#'  and displayed. (Always two-sided.)
#'@param h_added Logical. If \code{TRUE}, Cohen's h and its CI are calculated
#'  and displayed. (\code{FALSE} by default.)
#'@param for_table Logical. If \code{TRUE}, omits the confidence level display
#'  from the printed text.
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'@details The Bayes factor (BF) is always calculated with the default r-scale
#'  of \code{0.707}. BF supporting null hypothesis is denoted as BF01, while
#'  that supporting alternative hypothesis is denoted as BF10. When the BF is
#'  smaller than 1 (i.e., supports null hypothesis), the reciprocal is
#'  calculated (hence, BF10 = BF, but BF01 = 1/BF). When the BF is greater than
#'  or equal to 10000, scientific (exponential) form is reported for
#'  readability. (The original full BF number is available in the returned named
#'  vector as \code{bf}.)
#'
#'@return Prints exact test statistics (including proportion difference with CI,
#'  and BF) in APA style. Furthermore, when assigned, returns a named vector
#'  with the following elements: \code{z} (Z), \code{p} (p value),
#'  \code{prop_diff} (raw proportion difference), \code{h} (Cohen's h),
#'  \code{bf} (Bayes factor).
#'
#'@note Barnard's unconditional exact test is calculated via
#'  \code{\link[Exact:exact.test]{Exact::exact.test}} ("z-pooled").
#'
#'  The CIs for the proportion difference is calculated based on the p value, as
#'  described by Altman and Bland (2011).
#'
#'  The Bayes factor is calculated via
#'  \code{\link[BayesFactor:contingencyTableBF]{BayesFactor::contingencyTableBF}},
#'   with \code{sampleType = "indepMulti"}, as appropriate when both sample
#'  sizes (\code{n1} and \code{n2}) are known in advance (as it normally
#'  happens). (For details, see \code{\link[BayesFactor]{contingencyTableBF}},
#'  or e.g. 'Chapter 17 Bayesian statistics' in Navarro, 2019.)
#'
#'@references
#'
#'Altman, D. G., & Bland, J. M. (2011). How to obtain the confidence interval
#'from a P value. Bmj, 343(d2090). \doi{https://doi.org/10.1136/bmj.d2090}
#'
#'Barnard, G. A. (1947). Significance tests for 2x2 tables. Biometrika, 34(1/2),
#'123-138. \doi{https://doi.org/10.1093/biomet/34.1-2.123}
#'
#'Lydersen, S., Fagerland, M. W., & Laake, P. (2009). Recommended tests for
#'association in 2x2 tables. Statistics in medicine, 28(7), 1159-1175.
#'\doi{https://doi.org/10.1002/sim.3531}
#'
#'Navarro, D. (2019). Learning statistics with R.
#'\url{https://learningstatisticswithr.com/}
#'
#'Suissa, S., & Shuster, J. J. (1985). Exact unconditional sample sizes for the
#'2 times 2 binomial trial. Journal of the Royal Statistical Society: Series A
#'(General), 148(4), 317-327. \doi{https://doi.org/10.2307/2981892}
#'
#'@examples
#' props_neat(
#'     case1 = 35,
#'     case2 = 48,
#'     n1 = 80,
#'     n2 = 77,
#'     h_added = TRUE
#' )
#'
#' props_neat(
#'     case1 = 35,
#'     case2 = 48,
#'     n1 = 80,
#'     n2 = 77,
#'     greater = "2"
#' )
#' @export
props_neat = function(case1,
                      case2,
                      n1,
                      n2,
                      greater = NULL,
                      ci = NULL,
                      bf_added = FALSE,
                      h_added = FALSE,
                      for_table = FALSE,
                      hush = FALSE) {
    validate_args(
        match.call(),
        list(
            val_arg(case1, c('num'), 1),
            val_arg(case2, c('num'), 1),
            val_arg(n1, c('num'), 1),
            val_arg(n2, c('num'), 1),
            val_arg(greater, c('null', 'char'), 1, c('1', '2')),
            val_arg(ci, c('null','num'), 1),
            val_arg(bf_added, c('bool'), 1),
            val_arg(h_added, c('bool'), 1),
            val_arg(for_table, c('bool'), 1),
            val_arg(hush, c('bool'), 1)
        )
    )
    greater = toString(greater)
    # to add: McNemar for paired; corresponding BF
    prop_1 = case1 / n1
    prop_2 = case2 / n2
    p_diff = prop_1 - prop_2
    matr = matrix(c(case1, case2, n1 - case1, n2 - case2), 2, 2)
    exact_res = Exact::exact.test(matr, to.plot = FALSE)
    z_norm = -0.862 + sqrt(0.743 - 2.404 * log(exact_res$p.value))
    p_se = abs(p_diff / z_norm)
    if (greater == "1") {
        if (hush == FALSE) {
            message("One-sided exact-test (with 90% CI default)! H1: first is greater than second.")
        }
        exact_res = Exact::exact.test(matr, to.plot = FALSE, alternative = "greater")
    } else if (greater == "2") {
        if (hush == FALSE) {
            message("One-sided exact-test (with 90% CI default)! H1: second is greater than first.")
        }
        exact_res = Exact::exact.test(matr, to.plot = FALSE, alternative = "less")
    } else {
        if (is.null(ci)) {
            ci = 0.95
        }
    }
    if (is.null(ci)) {
        ci = 0.90
    }
    z_c = stats::qnorm(1 - (1 - ci) / 2)
    p_low = p_diff - p_se * z_c
    p_upp = p_diff + p_se * z_c

    x1 = asin(sign(prop_1) * sqrt(abs(prop_1)))
    x2 = asin(sign(prop_2) * sqrt(abs(prop_2)))
    es = x1 - x2
    se_h = sqrt(0.25 * (1 / n1 + 1 / n2))
    h_low = (es - (z_c * se_h)) * 2
    h_upp = (es + (z_c * se_h)) * 2
    h = es * 2
    z = exact_res$statistic
    pvalue = exact_res$p.value
    if (bf_added == TRUE) {
        bf = BayesFactor::contingencyTableBF(matr, sampleType = "indepMulti", fixedMargin = "rows")
        bf = as.vector(bf)
        bf_out = bf_neat(bf)
    } else {
        bf_out = "."
        bf = NA
    }
    if (for_table == TRUE) {
        ci_disp = ""
    } else {
        ci_disp = paste0(", ", ro(ci * 100, 0), "% CI")
    }
    if (h_added == TRUE) {
        h_out = paste0(", h = ",
                       ro(h, 2),
                       ci_disp,
                       " [",
                       ro(h_low, 2),
                       ", ",
                       ro(h_upp, 2),
                       "]")
    } else {
        h_out = ''
    }
    p_diff_out = edges(p_diff, 2, no_null = TRUE)
    p_low_out = edges(p_low, 2, no_null = TRUE)
    p_upp_out = edges(p_upp, 2, no_null = TRUE)

    out = paste0(
        'Z = ',
        ro(z, 2),
        ", p = ",
        ro(pvalue, 3),
        ", Pdiff = ",
        p_diff_out,
        ci_disp,
        " [",
        p_low_out,
        ", ",
        p_upp_out,
        "]",
        h_out,
        bf_out
    )
    if (hush == FALSE) {
        prnt(out)
    }
    invisible(c(
        z = as.numeric(z),
        p = pvalue,
        prop_diff = p_diff,
        h = h,
        bf = as.numeric(bf)
    ))
}
