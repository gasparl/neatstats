#'@title Plots of Means
#'
#'@description Plots for factorial designs.
#' @export

# NOTE
# For error bars, use SD to illustrate variability, CI to illustrate certainty.

# use for example ggpubr to combine plots

plot_neat = function(data_per_subject,
                     values,
                     between_vars = NULL,
                     within_ids = NULL,
                     factor_names = NULL,
                     value_names = NULL,
                     y_title = NULL,
                     reverse = FALSE,
                     panels = NULL,
                     type = 'line',
                     dodge = NULL,
                     bar_colors = c('#555555', '#AAAAAA'),
                     method = mean,
                     eb_method = stats::sd) {
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
            val_arg(values, c('char'), 1),
            val_arg(between_vars, c('null', 'char'), 1),
            val_arg(within_ids, c('null', 'char', 'list'), 1),
            val_arg(factor_names, c('null', 'char')),
            val_arg(value_names, c('null', 'char')),
            val_arg(y_title, c('null', 'char'), 1),
            val_arg(reverse, c('bool'), 1),
            val_arg(panels, c('null', 'char'), 1),
            val_arg(type, c('char'), 1, c('bar', 'line')),
            val_arg(dodge, c('null', 'num')),
            val_arg(bar_colors, c('char'), 0),
            val_arg(method, c('function'), 1),
            val_arg(eb_method, c('null', 'function'), 1)
        )
    )
    val_wi_id(match.call(), within_ids, values)
    name_taken('within_factor', data_wide)
    name_taken('neat_unique_values', data_wide)
    name_taken('neat_unique_id', data_wide)
    id_col = 'neat_unique_id'
    data_wide[[id_col]] = as.character(seq.int(nrow(data_wide)))
    values = to_c(values)
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
    }
    this_data[, id_col] = to_fact(this_data[[id_col]])

    if (is.null(between_vars)) {
        g_by = within_vars
    } else if (is.null(within_vars)) {
        g_by = between_vars
    } else {
        g_by = paste(within_vars, between_vars, sep = ',')
    }
    if (length(to_c(g_by)) > 3) {
        stop("Maximum three factors can be plotted. See help(plot_neat)")
    } else if (length(to_c(g_by)) < 1) {
        stop("Minimum two factors are needed for plotting. See help(plot_neat)")
    }
    to_plot = mains_ebs(
        data_long = this_data,
        method = method,
        eb_method = eb_method,
        g_by = g_by
    )
    fact_names = to_c(g_by)
    names(to_plot)[1:length(fact_names)] = fact_names

    if (!is.null(value_names)) {
        i = sapply(to_plot, is.factor)
        to_plot[i] = lapply(to_plot[i], as.character)
        for (v_name in names(value_names)) {
            to_plot[to_plot == v_name] = value_names[v_name]
        }
    }
    if (!is.null(panels) &&
        panels %in% fact_names && length(fact_names) == 3) {
        fact_names = c(fact_names[!fact_names == panels], panels)
    }
    if (reverse == TRUE) {
        fact_names[c(1, 2)] = fact_names[c(2, 1)]
    }
    p_close = fact_names[1]
    p_mid = fact_names[2]

    if (is.null(dodge)) {
        if (type == 'bar') {
            dodge = 0.9
        } else {
            dodge = 0.1
        }
    }
    if (type == 'line') {
        dodge = 0.1
    } else {
        dodge = 0.9
    }
    if (type == 'line') {
        the_plot = ggplot2::ggplot(data = to_plot, aes(x = to_plot[[p_mid]],
                                                       y = x.main,
                                                       group = to_plot[[p_close]])) +
            geom_line(aes(linetype = to_plot[[p_close]]),
                      position = position_dodge(dodge)) +
            geom_point(aes(shape = to_plot[[p_close]]),
                       position = position_dodge(dodge))  +
            theme_bw()  +
            labs(x = re_n(p_mid, factor_names), y = y_title) +
            scale_shape_discrete(name = re_n(p_close, factor_names)) +
            scale_linetype_discrete(name = re_n(p_close, factor_names))
    } else {
        color_gen = grDevices::colorRampPalette(bar_colors)
        the_plot = ggplot2::ggplot(data = to_plot, aes(x = to_plot[[p_mid]],
                                                       y = x.main,
                                                       fill = to_plot[[p_close]])) +
            geom_bar(stat = "identity", position = position_dodge(dodge)) +
            theme_bw() + scale_fill_manual(values = color_gen(length(unique(to_plot[[p_close]]))),
                                           name = re_n(p_close, factor_names)) +
            labs(x = re_n(p_mid, factor_names), y = y_title)
    }
    if (!is.null(eb_method)) {
        the_plot = the_plot + geom_errorbar(aes(
            ymin = x.main - x.eb,
            ymax = x.main + x.eb,
            width = 0.2
        ),
        position = position_dodge(dodge))
    }
    if (length(fact_names) == 3) {
        the_plot = the_plot + facet_wrap(~ to_plot[[fact_names[3]]], nrow = 1)
    }
    return(the_plot)
}
