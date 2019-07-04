#'@title Plots of Means
#'
#'@description Plots for factorial designs.
#' @export

plot_neat = function() {
    cat('hi')
}


re_n = function(name, n_dict) {
    return( if (is.null(n_dict) || is.na(n_dict[name])) name else n_dict[name])
}

# NOTE
# For error bars, use SD to illustrate variability, CI to illustrate certainty.

# labeller function
# list with names of any potential label (e.g. color = "Colors")
# list with names of any potential value (e.g. neg = "negative")
# then check if in list, e.g. is.null(labels[['neg']]), and use just 'neg' if NULL
# -> function for this check


# use for example ggpubr to combine plots
