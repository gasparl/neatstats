% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_neat.R
\name{table_neat}
\alias{table_neat}
\title{Table, descriptives}
\usage{
table_neat(
  values_list,
  group_by = NULL,
  group_per = "rows",
  to_clipboard = FALSE,
  method = "mean+sd",
  transpose = FALSE
)
}
\arguments{
\item{values_list}{Data frames as returned from the \code{\link{aggr_neat}}
function: variables from which the statistics for the table are to be
calculated. The \code{group_by}, \code{method}, and \code{prefix}
parameters are ignored when they are given in the \code{\link{table_neat}}
function; see Details and also an extensive example via
https://github.com/gasparl/neatstats.}

\item{group_by}{String, or vector of strings: the name(s) of the column(s) in
the \code{dat} given data frame, containing the vector(s) of factors by
which the statistics are grouped. (Overwrites \code{group_by} in
\code{\link{aggr_neat}}; see Details.)}

\item{group_per}{String, "rows" or "columns". If set to "columns" (or just
"c" or "col", etc.), each column contains statistics for one group.
Otherwise (default), each row contains statistics for one group.}

\item{to_clipboard}{Logical. If \code{TRUE}, the table is copied to the
clipboard (default: \code{FALSE}).}

\item{method}{Function or string; overwrites the \code{method} argument in
\code{\link{aggr_neat}} when used within this function. See \code{method}
in the \code{\link{aggr_neat}} function for details. Default value:
\code{"mean+sd"} (to calculate means and standard deviations table).}

\item{transpose}{Logical (default: \code{FALSE}) or string. If \code{TRUE} or
string, ignores all other parameters (except \code{values_list}), but
merges all values from given list of data frames (as returned from the
\code{\link{aggr_neat}}) and transposes them into a single row, using, by
default, the \code{"aggr_group"} column values for new headers
(corresponding to the output of \code{\link{aggr_neat}}; see Examples).
However, a string given as argument for the \code{transpose} parameter can
also specify a custom column name.}
}
\value{
Returns a data frame with means or medians and SDs per variable and
  per group.
}
\description{
Creates a neat means (or similar descriptives) and standard
  deviations table, using \code{\link{aggr_neat}} functions as arguments.
  Alternatively, merges and transposes data frames into rows.
}
\details{
The \code{values}, \code{round_to}, and \code{new_name} arguments
  given in the \code{\link{aggr_neat}} function are always applied. However,
  the \code{prefix} parameter will be overwritten as \code{NULL}. If
  \code{new_name} in \code{\link{aggr_neat}} is \code{NULL}, the given input
  variable names will be used instead of \code{"aggr_value"}. Furthermore,
  the \code{group_by} or \code{method} given in the \code{\link{aggr_neat}}
  function are only applied when no arguments are given in the
  \code{\link{table_neat}} function for the identical parameters
  (\code{group_by} or \code{medians}). If either parameter is given in the
  \code{\link{table_neat}} function, all separately given respective
  argument(s) in the \code{\link{aggr_neat}} function(s) are ignored.
}
\examples{
data("mtcars") # load base R example dataset

# overall means and SDs table for disp (Displacement) and hp (Gross horsepower)
table_neat(list(aggr_neat(mtcars, disp),
                aggr_neat(mtcars, hp)))

# means and SDs table for mpg (Miles/(US) gallon), wt (Weight), and hp (Gross horsepower)
# grouped by cyl (Number of cylinders)
# each measure rounded to respective optimal number of digits
# wt renamed to weight (for the column title)
table_neat(list(
    aggr_neat(mtcars, mpg, round_to = 1),
    aggr_neat(mtcars, wt, new_name = 'weight', round_to = 2),
    aggr_neat(mtcars, hp, round_to = 0)
),
group_by = 'cyl')

# same as above, but with medians, and with groups per columns
table_neat(
    list(
        aggr_neat(mtcars, mpg, round_to = 1),
        aggr_neat(mtcars, wt, new_name = 'weight', round_to = 2),
        aggr_neat(mtcars, hp, round_to = 0)
    ),
    group_by = 'cyl',
    method = 'median+sd',
    group_per = 'columns'
)

# an extensive example to show how to collect and aggregate raw data is
# available via the README file at the repository:
# https://github.com/gasparl/neatstats

}
\seealso{
\code{\link{aggr_neat}} for more related details
}
