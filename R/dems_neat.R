#' @title Demographics
#'
#' @description Prints participant count, age mean and SD, and gender ratio,
#'   from given dataset.
#' @param data_per_subject Data frame from which demographics are to be
#'   calculated. Must contain columns named precisely as "\code{age}" and as
#'   "\code{gender}". The \code{age} column must contain only numbers or
#'   \code{NA}, while \code{gender} column must contain only 1 (= male) or 2 (=
#'   female), either as numbers or as strings, or \code{NA}.
#' @param group_by A vector of factors by which the statistics are grouped,
#'   typically a column from the data frame provided as \code{data_per_subject}.
#' @param percent Logical. If \code{TRUE}, gender ratios (and the
#'   "unknown" ratios based on \code{NA} values) are presented as percentage. If
#'   \code{FALSE}, they are presented as counts (i.e., numbers of subjects).
#' @param round_perc Number \code{\link[=ro]{to round}} to, when using
#'   percentages.
#'
#'@details If \code{NA} values are found in either the \code{age} or
#'  \code{gender} column, the ratio (or count) of unknown cases will be displayed
#'  everywhere. Otherwise it will simply not be displayed anywhere.
#'
#' @examples
#' # below is an illustrative example dataset
#' # (the "subject" and "measure_x" columns are not used in the function)
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     gender = c(2, 2, 1, 2, 1, 2, 2, 2, 1, 1),
#'     age = c(6, 7, 8.5, 6, 5, 16, 17, 16, 45, 77),
#'     measure_x = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85)
#' )
#'
#' # print demographics (age and gender) per "conditions":
#' dems_neat(dat, group_by = dat$conditions)
#'
#' # another dataset, with some missing values
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     gender = c(2, 2, NA, NA, 1, 1, 1, 2, NA, NA),
#'     age = c(6, 7, 8.5, 6, 5, 16, NA, 16, 45, 77),
#'     measure_x = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85)
#' )
#' # again print demographics per "conditions":
#' dems_neat(dat, group_by = dat$conditions)
#'
#' @export
dems_neat = function(data_per_subject,
                     group_by = NULL,
                     percent = FALSE,
                     round_perc = 0) {
    if (class(data_per_subject) == "character") {
        s_dat = eval(parse(text = data_per_subject))
    } else {
        s_dat = data_per_subject
        data_per_subject = data_per_subject
    }
    validate_args(match.call(),
                  list(
                      val_arg(data_per_subject, c('df')),
                      val_arg(percent, c('bool'), 1),
                      val_arg(round_perc, c('num'), 1)
                  ))
    if (!'age' %in% names(data_per_subject)) {
        if (!'gender' %in% names(data_per_subject)) {
            stop(
                'The data frame must contain the columns "gender" and "age", but neither was found among the column names.'
            )
        } else {
            stop(
                'The data frame must contain the columns "gender" and "age". Column name "age" was not found.'
            )
        }
    } else if (!'gender' %in% names(data_per_subject)) {
        stop(
            'The data frame must contain the columns "gender" and "age". Column name "gender" was not found.'
        )
    }
    if (all(data_per_subject$gender == '1' |
            data_per_subject$gender == '2'|
            is.na(data_per_subject$gender)) == FALSE) {
        stop('The "gender" column must only contain the values 1 (male) or 2 (female).')
    }
    s_dat$age = as.numeric(as.character(s_dat$age))

    if (is.null(group_by)) {
        s_dat$neat_cond = 0
    } else if (class(group_by) == "character") {
        s_dat$neat_cond = s_dat[[group_by]]
    } else {
        s_dat$neat_cond = group_by
    }
    s_dat$gender = factor(s_dat$gender, levels = c(1, 2))
    gender = as.data.frame.matrix(stats::xtabs(~ neat_cond + gender, s_dat))
    if (!'1' %in% colnames(gender)) {
        gender = data.frame('1' = 0, gender)
    } else if (!'2' %in% colnames(gender)) {
        gender[['2']] = 0
    }
    gender$ratio = gender[['1']] / (gender[['1']] + gender[['2']]) * 100
    gender$neat_cond = row.names(gender)

    age = do.call(data.frame,
                  stats::aggregate(s_dat$age, by = list(s_dat$neat_cond), function(x)
                      c(
                          count = length(x),
                          mean = mean(x, na.rm = TRUE),
                          sd = stats::sd(x, na.rm = TRUE)
                      )))
    names(age)[names(age) == "Group.1"] <- "neat_cond"
    age_gend = merge(age, gender, by = 'neat_cond')
    if (sum(is.na(s_dat$age)) > 0 | sum(is.na(s_dat$gender)) > 0) {
        na_age = do.call(data.frame,
                         stats::aggregate(s_dat$age, by = list(s_dat$neat_cond), function(x)
                             c(
                                 count = sum(is.na(x)),
                                 percent = 100 * sum(is.na(x)) / length(x)
                             )))
        na_gender = do.call(data.frame,
                            stats::aggregate(s_dat$gender, by = list(s_dat$neat_cond), function(x)
                                c(
                                    count = sum(is.na(x)),
                                    percent = 100 * sum(is.na(x)) / length(x)
                                )))
        if (percent == TRUE) {
            age_gend$age_missing = paste0(' [', ro(na_age$x.percent, round_perc), '% unknown]')
            age_gend$gender_missing = paste0(' [', ro(na_gender$x.percent, round_perc), '% unknown]')
        } else {
            age_gend$age_missing = paste0(' [', ro(na_age$x.count, round_perc), ' unknown]')
            age_gend$gender_missing = paste0(' [', ro(na_gender$x.count, round_perc), ' unknown]')
        }
        age_gend$age_missing = paste0(age_gend$age_missing)
    } else {
        age_gend$age_missing = ""
        age_gend$gender_missing = ""
    }
    if (percent != FALSE) {
        for (i in 1:nrow(age_gend)) {
            row <- age_gend[i,]
            prnt(
                'Group < ',
                row[[1]],
                ' >: ',
                row[2],
                ' subjects (age = ',
                ro(row[3], 1),
                'CHAR_PLUSMIN',
                ro(row[4], 1),
                row[8],
                ', ',
                ro(row[7], round_perc),
                "% male",
                row[9],
                ")"
            )
        }
    } else {
        for (i in 1:nrow(age_gend)) {
            row <- age_gend[i,]
            prnt(
                'Group < ',
                row[[1]],
                ' >: ',
                row[2],
                ' subjects (age = ',
                ro(row[3], 1),
                'CHAR_PLUSMIN',
                ro(row[4], 1),
                row[8],
                ', ',
                row[5],
                " male",
                row[9],
                ")"
            )
        }
    }
}
