#' @title Exclusion
#'
#' @description Filters dataset by rows (normally: subjects, observations) and
#'   prints the numbers of excluded rows and remaining rows.
#' @param dat Data frame to be filtered.
#' @param filt An expression to filter, by column values, the \code{dat}
#'   data frame. The expression should use column names alone; see Examples.
#' @param group_by String, or vector of strings: the name(s) of the column(s) in
#'   the \code{dat} data frame, containing the vector(s) of factors by which the
#'   printed counts are grouped.
#' @param sort_by String; specifies whether the printed counts should be sorted
#'  by exclusion (default; \code{"exclusion"} or its short forms, e.g.
#'  \code{"excl"}), or by the factors given for \code{group_by} (for this, give
#'  any other string, e.g. \code{"conditions"}). If \code{NULL} (default).
#'@param hush Logical. If \code{TRUE}, prevents printing counts to console.
#' @return A data frame with the statistics per group, with a single column
#'   (\code{"aggr_group"}) indicating the grouping.
#' @seealso \code{\link{table_neat}} to create full tables using multiple
#'   variables
#' @examples
#'
#' @export

excl_neat = function(dat,
                     filt,
                     group_by = NULL,
                     sort_by = 'exclusion',
                     hush = FALSE) {
    if (typeof(dat) == "character") {
        dat = eval(parse(text = dat))
    }
    validate_args(match.call(),
                  list(
                      val_arg(dat, c('df')),
                      val_arg(group_by, c('null', 'char')),
                      val_arg(sort_by, c('char'), 1),
                      val_arg(hush, c('bool'), 1)
                  ))
    name_taken('neat_unique_ids', dat)
    dat$neat_unique_ids = paste0('id', seq.int(nrow(dat)))
    filt = deparse(substitute(filt))
    if (filt != "NULL") {
        dat_filted = eval(parse(text = paste0(
            'with(data = dat, dat[',
            filt = trimws(filt, whitespace = "['\"]"),
            ',])'
        )))
    }
    if (hush == FALSE) {
        dat$remaining = ifelse(
            dat$neat_unique_ids %in% dat_filted$neat_unique_ids,
            'remained',
            'excluded'
        )
        if (substr("exclusion", 1, nchar(sort_by)) == sort_by) {
            grouppin = c(group_by, 'remaining')
        } else {
            grouppin = c('remaining', group_by)
        }
        print(
            aggr_neat(
                dat = dat,
                values = neat_unique_ids,
                group_by = grouppin,
                method = length,
                new_name = 'count'
            )
        )
    }
    invisible(dat_filted)
}
