#' @title Merge by Columns in Loops
#'
#' @description Merges raws by columns in a loop using the \code{\link{enum}}
#'   function. On first iteration, indicated by \code{\link{enum}}, initiates a
#'   new data frame with the data to merge as first row. On all following
#'   iterations, adds data to merge as subsequent rows (using
#'   \code{\link[plyr:rbind.fill]{plyr:rbind.fill}}).
#'
#' @param merged The name of the data frame for the merged data (without
#'   quotes).
#' @param to_merge Data frame to be merged. (Normally a single-row data frame.)
#' @param hush Logical. If \code{TRUE} (default), prints message when the data
#'   frame for merging is initated.
#'@details See an extensive example via https://github.com/gasparl/neatstats.
#' @seealso \code{\link{enum}}
#' @examples
#'
#' my_vector = c('aa', 'bb', 'cxyz', 'last')
#' for (elem in enum(my_vector)) {
#'     cat(elem, fill = TRUE)
#'     datfram = data.frame(item = elem[2],
#'                          number = elem[1],
#'                          whatever = paste0('number (', elem[1], ')'))
#'     rbind_loop(merged_data, datfram)
#' }
#' # merged_data now contained all merged rows
#' print(merged_data)
#'
#' # an extensive example to show how to collect and aggregate raw data is
#' # available via the README file at the repository:
#' # https://github.com/gasparl/neatstats
#'

#' @export
rbind_loop = function(merged, to_merge,
                      hush = FALSE) {
    merged_str = deparse(substitute(merged))
    if (is.null(pkg.globals$my_unique_first_iter)) {
        stop(
            'First iteration was not indicated via the neatStats::enum() function. ',
            'Please use enum() if you want to use rbind_neat(). See ?rbind_neat for details'
        )
    } else if (pkg.globals$my_unique_first_iter  == TRUE) {
        if (hush == FALSE) {
            message('Initiated',
                    merged_str,
                    '(with',
                    ncol(merged_str),
                    'columns).',
                    fill = TRUE)
        }
        pkg.globals$my_unique_first_iter = FALSE
        assign(merged_str, to_merge, envir = .GlobalEnv)
    } else if (pkg.globals$my_unique_first_iter  == FALSE) {
        assign(merged_str,
               plyr::rbind.fill(merged, to_merge),
               envir = .GlobalEnv)
    }
}
