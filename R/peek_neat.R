#' @title Cursory Summaries and Plots per Group
#'
#' @description Cursory summaries and plots per group.
#' @param dat Data frame or vector.
#' @param values Values.
#' @param group_by Character.
#' @param iqr_times Numeric.
#' @param display_n Logical.
#'
#'
#' @details
#'
#'By default, prints (and invisibly returns) the following data (per group):
#'\code{mean}; 95% CI of the mean as \code{ci_low} and \code{ci_upp}; \code{sd};
#'\code{median}; \code{quantile_1st} and \code{quantile_3rd} (first and third
#'quantiles); "Tukey's fences" as \code{fence_low} and \code{fence_upp}. Tukey's
#'fences are the upper and lower limits with distances of \code{X} times the
#'\code{\link[stats]{IQR}} from the actual IQR, where \code{X} is specified via
#'the \code{iqr_times} parameter.
#'
#'
#'
#' @return The given function results and plots by group.
#'
#' @examples
#'
#' @export
peek_neat = function(num,
              round_to = 2,
              leading_zero = TRUE) {
    validate_args(match.call(),
                  list(val_arg(leading_zero, c('bool'), 1)))

    return(formtd)
}

sum_neat = function(numvec, iqr_times) {
    quantile_1st = as.numeric(stats::quantile(numvec, .25, na.rm = TRUE))
    quantile_3rd = as.numeric(stats::quantile(numvec, .75, na.rm = TRUE))
    mycis = neatStats::mean_ci(numvec, distance_only = FALSE)
    out = c(
        mean = mean(numvec, na.rm = TRUE),
        ci_low = as.numeric(mycis[1]),
        ci_upp = as.numeric(mycis[2]),
        sd = sd(numvec, na.rm = TRUE),
        median = median(numvec, na.rm = TRUE),
        quantile_1st = quantile_1st,
        quantile_3rd = quantile_3rd,
        fence_low = quantile_1st - iqr_times * (quantile_3rd - quantile_1st),
        fence_upp = iqr_times * (quantile_3rd - quantile_1st) + quantile_3rd
    )
    to_print = as.numeric(ro(out, 4, signi = TRUE))
    names(to_print) = names(out)
    prnt(to_print)
    invisible(out)
}

box_neat = function(values, group, group_n = TRUE) {
    plot_dat = data.frame(values = values, group = group)
    if (group_n == TRUE) {
        plot_dat$group = paste0(plot_dat$group, '\n(n = ',
                                table(plot_dat$group)[plot_dat$group], ')')
    }
    ggplot(plot_dat, aes(x = group,
                         y = values)) +
        stat_boxplot(geom = 'errorbar', width = 0.5) +
        geom_boxplot(fill = '#cdebd0', outlier.shape = NA) +
        geom_violin(color = '#9f9fcc',
                    alpha = 0.2,
                    scale = 'count') +
        geom_boxplot(
            outlier.shape = 4,
            outlier.size = 3,
            outlier.stroke = 0.8,
            alpha = 1,
            fill = NA
        ) +
        theme_bw() + xlab(NULL) + ylab(NULL)
}
