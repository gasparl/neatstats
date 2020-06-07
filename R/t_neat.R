#'@title Difference of Two Means and Area Under the Curve
#'
#'@description Welch's \code{\link[stats:t.test]{t-test}} results including
#'  Cohen's d with confidence interval (CI),
#'  \code{\link[BayesFactor:ttestBF]{Bayes factor}} (BF), and
#'  \code{\link[pROC:auc]{area under the receiver operating characteristic
#'  curve}} (AUC). For non-parametric version,
#'  \code{\link[stats:wilcox.test]{Wilcoxon test}} results (Mann–Whitney U test,
#'  aka "Wilcoxon rank-sum test", for independent samples; Wilcoxon signed-rank
#'  test for paired samples; along with corresponding rank-based BFs as per van
#'  Doorn et al., 2020).
#'@param var1 Numeric vector; numbers of the first variable.
#'@param var2 Numeric vector; numbers of the second variable.
#'@param pair Logical. If \code{TRUE}, all tests (t, BF, AUC) are conducted for
#'  paired samples. If \code{FALSE} (default) for independent samples.
#'@param nonparametric Logical (\code{FALSE} by default). If \code{TRUE}, uses
#'  nonparametric (rank-based, "Wilcoxon") t-tests (including BFs; see Details).
#'@param greater \code{NULL} or string (or number); optionally specifies
#'  one-sided tests (t and BF): either "1" (\code{var1} mean expected to be
#'  greater than \code{var2} mean) or "2" (\code{var2} mean expected to be
#'  greater than \code{var1} mean). If \code{NULL} (default), the test is
#'  two-sided.
#'@param ci Numeric; confidence level for returned CIs for Cohen's d and AUC.
#'@param bf_added Logical. If \code{TRUE} (default), Bayes factor is calculated
#'  and displayed.
#'@param bf_rscale The scale of the prior distribution (\code{0.707} by
#'  default).
#'@param bf_sample Number of samples used to estimate Bayes factor (\code{1000}
#'  by default). More samples (e.g. \code{10000}) take longer time but give more
#'  stable BF.
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
#'@param hush Logical. If \code{TRUE}, prevents printing any details to console.
#'@param plot_densities Logical. If \code{TRUE}, creates a density plot (i.e.,
#'  \code{\link[stats:density]{Gaussian kernel density estimates}}) from the two
#'  variables. When \code{auc_added} is \code{TRUE} (and the AUC is at least
#'  .5), the best threshold value for classification (maximal differentiation
#'  accuracy) is added to the plot as vertical line. (In case of multiple best
#'  thresholds with identical overall accuracy, all are added.)
#'@param y_label String or \code{NULL}; the label for the \code{y} axis.
#'  (Default: \code{"density estimate"}.)
#'@param x_label String or \code{NULL}; the label for the \code{x} axis.
#'  (Default: \code{"values"}.)
#'@param factor_name String or \code{NULL}; factor legend title. (Default:
#'  \code{NULL}.)
#'@param var_names A vector of two strings; the variable names to be displayed
#'  in the legend. (Default: \code{c("1", "2")}.)
#'@param reverse Logical. If \code{TRUE}, reverses the order of variable names
#'  displayed in the legend.
#'
#'@details
#' The Bayes factor (BF) supporting null hypothesis is denoted as BF01, while
#' that supporting alternative hypothesis is denoted as BF10. When the BF is
#' smaller than 1 (i.e., supports null hypothesis), the reciprocal is calculated
#' (hence, BF10 = BF, but BF01 = 1/BF). When the BF is greater than or equal to
#' 10000, scientific (exponential) form is reported for readability. (The
#' original full BF number is available in the returned named vector as
#' \code{bf}.)
#'
# For details about the nonparametric (rank-based, Wilcoxon) Bayes factors, see
# van Doorn et al. (2020). The source code for the calculation is a contribution
# by J. van Doorn; the original version is available via https://osf.io/gny35/.
#'
#'For simplicity, Cohen's d is reported for nonparametric tests too: you may
#'however want to consider reporting alternative effect sizes in this case.
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
#'@return Prints t-test statistics (including Cohen's d with CI, BF, and AUC, as
#'  specified via the corresponding parameters) in APA style. Furthermore, when
#'  assigned, returns a list, that contains a named vector '\code{stats}' with
#'  the following elements: \code{t} (t value), \code{p} (p value), \code{d}
#'  (Cohen's d), \code{bf} (Bayes factor), \code{auc} (AUC), \code{accuracy}
#'  (overall accuracy using the most optimal classification threshold), and
#'  \code{youden} (Youden's index: \code{specificity + sensitivity - 1}). The
#'  latter three are \code{NULL} when \code{auc_added} is \code{FALSE}. When
#'  \code{auc_added} is \code{TRUE}, there are also two additional elements of
#'  the list. One is '\code{roc_obj}', which is a \code{\link[pROC]{roc}}
#'  object, to be used e.g. with the \code{\link{roc_neat}} function. The other
#'  is '\code{best_thresholds}', which contains the best threshold value(s) for
#'  classification, along with corresponding specificity and sensitivity.
#'  Finally, if \code{plot_densities} is \code{TRUE}, the plot is displayed as
#'  well as returned as a \code{\link[ggplot2]{ggplot}} object, named
#'  \code{density_plot}.
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
#'@references 
#'
#'Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists should by
#'default use Welch's t-test instead of Student's t-test. International Review
#'of Social Psychology, 30(1). \doi{http://doi.org/10.5334/irsp.82}
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
#'van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2020). Bayesian
#'rank-based hypothesis testing for the rank sum test, the signed rank test, and
#'Spearman’s rho. Journal of Applied Statistics, 1–23.
#'\doi{https://doi.org/10.1080/02664763.2019.1709053}
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
                  nonparametric = FALSE,
                  greater = NULL,
                  ci = NULL,
                  bf_added = TRUE,
                  bf_rscale = sqrt(0.5),
                  bf_sample = 1000,
                  auc_added = FALSE,
                  r_added = TRUE,
                  for_table = FALSE,
                  test_title = "Descriptives:",
                  round_descr = 2,
                  round_auc = 3,
                  auc_greater = '1',
                  hush = FALSE,
                  plot_densities = FALSE,
                  y_label = "density estimate",
                  x_label = "\nvalues",
                  factor_name = NULL,
                  var_names = c("1", "2"),
                  reverse = FALSE) {
    validate_args(
        match.call(),
        list(
            val_arg(var1, c('num'), 0),
            val_arg(var2, c('num'), 0),
            val_arg(pair, c('bool'), 1),
            val_arg(nonparametric, c('bool'), 1),
            val_arg(greater, c('null', 'char'), 1, c('1', '2')),
            val_arg(ci, c('null', 'num'), 1),
            val_arg(bf_added, c('bool'), 1),
            val_arg(bf_rscale, c('num'), 1),
            val_arg(bf_sample, c('num'), 1),
            val_arg(auc_added, c('bool'), 1),
            val_arg(r_added, c('bool'), 1),
            val_arg(for_table, c('bool'), 1),
            val_arg(test_title, c('char'), 1),
            val_arg(round_descr, c('num'), 1),
            val_arg(round_auc, c('num'), 1),
            val_arg(auc_greater, c('char'), 1, c('1', '2')),
            val_arg(hush, c('bool'), 1),
            val_arg(plot_densities, c('bool'), 1),
            val_arg(y_label, c('null', 'char'), 1),
            val_arg(x_label, c('null', 'char'), 1),
            val_arg(factor_name, c('null', 'char'), 1),
            val_arg(var_names, c('char'), 0),
            val_arg(reverse, c('bool'), 1)
        )
    )
    if (is.null(ci)) {
        if (is.null(greater)) {
            ci = 0.95
        }
        else {
            ci = 0.90
        }
    }
    greater = toString(greater)
    if (anyNA(var1) || anyNA(var2)) {
        if (pair == TRUE) {
            message("NA values omitted pairwise.")
            if (anyNA(var1)) {
                var2 = var2[!is.na(var1)]
                var1 = var1[!is.na(var1)]
            }
            if (anyNA(var2)) {
                var1 = var1[!is.na(var2)]
                var2 = var2[!is.na(var2)]
            }
        } else {
            var1 = var1[!is.na(var1)]
            var2 = var2[!is.na(var2)]
            message("NA values omitted.")
        }
    }
    descr_1 = paste0(ro(mean(var1), round_descr),
                     "CHAR_PLUSMIN",
                     ro(stats::sd(var1), round_descr))
    descr_2 = paste0(ro(mean(var2), round_descr),
                     "CHAR_PLUSMIN",
                     ro(stats::sd(var2), round_descr))
    if (bf_added == TRUE &&
        nonparametric == TRUE) {
        if (pair == TRUE) {
            delta_samps = signRankGibbsSampler(
                var1,
                var2,
                progBar = (!hush),
                nSamples = bf_sample,
                cauchyPriorParameter = bf_rscale
            )$deltaSamples
        } else {
            delta_samps = rankSumGibbsSampler(
                var1,
                var2,
                progBar = (!hush),
                nSamples = bf_sample,
                cauchyPriorParameter = bf_rscale
            )$deltaSamples
        }
        if (hush == FALSE) {
            cat('', fill = TRUE)
        }
    }
    if (pair == TRUE & r_added == TRUE) {
        if (nonparametric == TRUE) {
            cat("Spearman's rank correlation: ")
            corr_neat(var1,
                      var2,
                      nonparametric = TRUE,
                      ci = 0.95,
                      bf_added = FALSE,
                      hush = hush)
        } else {
            cat("Pearson correlation: ")
            corr_neat(var1,
                      var2,
                      ci = 0.95,
                      bf_added = FALSE,
                      hush = hush)
        }
    }
    if (greater == "1") {
        if (hush == FALSE) {
            message("One-sided t-test and BF (with 90% CI default)! H1: first is greater than second.")
        }
        if (nonparametric == TRUE) {
            ttest = stats::wilcox.test(
                var1,
                var2,
                paired = pair,
                alternative = "greater",
                conf.level = ci,
                conf.int = TRUE,
                exact = TRUE
            )
        } else {
            ttest = stats::t.test(
                var1,
                var2,
                paired = pair,
                alternative = "greater",
                conf.level = ci
            )
        }
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(delta_samps,
                                               oneSided = "right",
                                               priorParameter = bf_rscale)
            } else {
                bf = as.vector(BayesFactor::ttestBF(
                    var1,
                    var2,
                    paired = pair,
                    iterations = bf_sample,
                    rscale = bf_rscale,
                    nullInterval = c(0, Inf)
                )[1])
            }
        }
    } else if (greater == "2") {
        if (hush == FALSE) {
            message("One-sided t-test and BF (with 90% CI default)! H1: second is greater than first.")
        }
        if (nonparametric == TRUE) {
            ttest = stats::wilcox.test(
                var1,
                var2,
                paired = pair,
                alternative = "less",
                conf.level = ci,
                conf.int = TRUE,
                exact = TRUE
            )
        } else {
            ttest = stats::t.test(
                var1,
                var2,
                paired = pair,
                alternative = "less",
                conf.level = ci
            )
        }
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(delta_samps,
                                               oneSided = "left",
                                               priorParameter = bf_rscale)
            } else {
                bf = as.vector(BayesFactor::ttestBF(
                    var1,
                    var2,
                    paired = pair,
                    iterations = bf_sample,
                    rscale = bf_rscale,
                    nullInterval = c(0, -Inf)
                )[1])
            }
        }
    } else {
        if (nonparametric == TRUE) {
            ttest = stats::wilcox.test(
                var1,
                var2,
                paired = pair,
                conf.level = ci,
                conf.int = TRUE,
                exact = TRUE
            )
        } else {
            ttest = stats::t.test(var1, var2, paired = pair, conf.level = ci)
        }
        if (bf_added == TRUE) {
            if (nonparametric == TRUE) {
                bf = computeBayesFactorOneZero(delta_samps,
                                               priorParameter = bf_rscale)
            } else {
                bf = as.vector(
                    BayesFactor::ttestBF(
                        var1,
                        var2,
                        paired = pair,
                        iterations = bf_sample,
                        rscale = bf_rscale
                    )
                )
            }
        }
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
    t_eq = stats::t.test(var1, var2, paired = pair, var.equal = TRUE)$statistic
    if (pair == TRUE) {
        sm = quiet(MBESS::ci.sm(
            ncp = t_eq,
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
            ncp = t_eq,
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
    mean_dif = ro(mean(var1) - mean(var2), round_descr)
    ci_r_low = ro(ttest$conf.int[1], round_descr)
    ci_r_upp = ro(ttest$conf.int[2], round_descr)
    if (greater == "1") {
        ci_r_upp = 'CHAR_INF'
        upper = 'CHAR_INF'
    } else if (greater == "2") {
        ci_r_low = 'CHAR_MINUSCHAR_INF'
        lower = 'CHAR_MINUSCHAR_INF'
    }
    if (hush == FALSE) {
        if (nonparametric == TRUE) {
            if (pair == TRUE) {
                outbegin = "W = "
            } else {
                outbegin = "U = "
            }
        } else {
            outbegin = paste0("t(",
                              df,
                              ") = ")
        }
        prnt(
            test_title,
            " MCHAR_PLUSMINSD = ",
            descr_1,
            " vs. ",
            descr_2,
            " (raw mean difference: ",
            mean_dif,
            ci_disp,
            " [",
            ci_r_low,
            ", ",
            ci_r_upp,
            "])"
        )
        out = paste0(
            outbegin,
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
    }
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
        if (hush == FALSE) {
            show_auc(
                theroc = the_roc,
                ci = ci,
                round_to = round_auc,
                for_table = for_table
            )
        }
        youdn = pROC::coords(the_roc, x = "best", ret = "youden")
        if (class(youdn) == "data.frame") {
            maxyouden = as.numeric(youdn$youden[1])-1
        } else {
            maxyouden =as.numeric(youdn[1])-1
        }
        bestacc = pROC::coords(the_roc, x = "best", ret = "accuracy")
        if (class(bestacc) == "data.frame") {
            max_acc = as.numeric(bestacc$accuracy[1])
        } else {
            max_acc =as.numeric(bestacc[1])
        }
        best_coords = pROC::coords(the_roc, x = "best")
        the_auc = pROC::auc(the_roc)
        if (class(best_coords) == "matrix") {
            plot_thres = best_coords["threshold",]
        } else {
            plot_thres = best_coords["threshold"]
        }
        plot_thres = plot_thres[!plot_thres %in% c(-Inf, Inf)]
    } else {
        the_auc = NULL
        max_acc = NULL
        maxyouden = NULL
        the_roc = NULL
        best_coords = NULL
        plot_thres = NULL
    }
    if (plot_densities == TRUE) {
        the_plot = plot_dens(
            v1 = var1,
            v2 = var2,
            y_label = y_label,
            x_label = x_label,
            thres = plot_thres,
            factor_name = factor_name,
            var_names = var_names,
            reverse = reverse
        )
        graphics::plot(the_plot)
    } else {
        the_plot = NULL
    }
    invisible(list(
        stats = c(
            t = as.numeric(t),
            p = pvalue,
            d = as.numeric(d_orig),
            bf = as.numeric(bf),
            auc = the_auc,
            accuracy = max_acc,
            youden = maxyouden
        ),
        roc_obj = the_roc,
        best_thresholds = best_coords,
        density_plot = the_plot
    ))
}

## density plot

plot_dens = function(v1,
                     v2,
                     y_label,
                     x_label,
                     thres,
                     factor_name,
                     var_names,
                     reverse) {
    dens_dat = data.frame(vals = c(v1, v2),
                          facts = c(rep(var_names[1], length(v1)),
                                    rep(var_names[2], length(v2))))
    if (reverse == TRUE) {
        dens_dat$facts =  factor(dens_dat$facts, levels = c(var_names[2], var_names[1]))
        start_color = 0.8
        end_color = 0.2
    } else {
        dens_dat$facts =  factor(dens_dat$facts, levels = var_names)
        start_color = 0.2
        end_color = 0.8
    }
    the_plot = ggplot(data = dens_dat, aes(x = dens_dat$vals,
                                           fill = dens_dat$facts)) + geom_density(alpha = 0.4, trim = FALSE) +
        theme_classic() +
        theme(text = element_text(family = "serif", size = 17),
              panel.border = element_rect(fill = NA)) +
        scale_fill_grey(name = factor_name,
                        start = start_color,
                        end = end_color) +
        geom_vline(
            xintercept = c(mean(v1), mean(v2)),
            color = "#777777",
            linetype = "dashed",
            size = 0.5
        )  +
        ylab(y_label) + xlab(x_label)
    if (!is.null(thres)) {
        the_plot = the_plot +
            geom_vline(
                xintercept = c(thres) ,
                color = "#8f8f8f",
                linetype = "solid",
                size = 0.5
            )
    }
    return(the_plot)
}
