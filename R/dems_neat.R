#' @title Demographics
#'
#' @description Prints participant count, age mean and SD, and gender ratio,
#'   from given dataset.
#' @param data_per_subject Data frame from which demographics are to be
#'   calculated. Should contain columns named as "\code{age}" and as
#'   "\code{gender}" (or, alternatively, "\code{sex}"). Alternatively, these
#'   columns can be specified via the \code{gender_col} and \code{age_col}
#'   parameters. The \code{age} column must contain only numbers or \code{NA},
#'   while \code{gender} column must contain only \code{1} (= male) or \code{2}
#'   (= female), either as numbers or as strings, or \code{NA}. Alternatively,
#'   male can be indicated, instead of \code{1}, with the string \code{male} (or
#'   its abbreviations, e.g. \code{m}), while in that case female can be
#'   indicated, instead of \code{2}, with the string \code{female} (or its
#'   abbreviations, e.g. \code{f} or \code{fem} ). (Lettercases do not matter,
#'   e.g. \code{Male} or \code{MALE} are both evaluated same as \code{male}.)
#' @param group_by Optionally the name(s) of column(s) from the data frame
#'   provided as \code{data_per_subject} to group by.
#' @param gender_col Optionally the name of column from the data frame
#'   that contains the gender (sex) information.
#' @param age_col Optionally the name of column from the data frame
#'   that contains the age information.
#' @param percent Logical. If \code{TRUE}, gender ratios (and the
#'   "unknown" ratios based on \code{NA} values) are presented as percentage. If
#'   \code{FALSE}, they are presented as counts (i.e., numbers of subjects).
#' @param round_perc Number \code{\link[=ro]{to round}} to, when using
#'   percentages.
#' @param show_fem Logical or \code{NULL}. If \code{TRUE}, the numbers of both
#'   male and female are displayed. If \code{FALSE}, only the number of males is
#'   displayed. If \code{NULL} (default), only the number of males is displayed
#'   when there are no unknown cases, but both numbers are displayed when there
#'   are any unknown cases.
#'
#'@details If \code{gender_col} and/or \code{age_col} are not specified, the
#'  function will first look for columns named precisely "\code{age}" and as
#'  "\code{gender}". If either is not found, the function looks for the same
#'  names but with any lettercase (e.g. "\code{AGE}" or "\code{Gender}"). If
#'  still no "\code{gender}" column is found, the function looks for
#'  "\code{sex}" column in the same manner. If no column is found for either,
#'  all related values will be counted as "unknown" (\code{NA}).
#'
#'  If \code{NA} values are found in either the \code{age} or \code{gender}
#'  column, the ratio (or count) of unknown cases will be displayed everywhere.
#'  Otherwise it will simply not be displayed anywhere.
#'
#' @examples
#' # below is an illustrative example dataset
#' # (the "subject" and "measure_x" columns are not used in the function)
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     gender = c(2, 2, 1, 2, 1, 2, 2, 2, 1, 1),
#'     age = c(6, 7, 8.5, 6, 5, 16, 17, 16, 45, 77),
#'     measure_x = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     stringsAsFactors = TRUE
#' )
#'
#' # print demographics (age and gender) per "conditions":
#' dems_neat(dat, group_by = 'conditions')
#'
#' # another dataset, with some missing values
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     gender = c(2, 2, NA, NA, 1, 1, 1, 2, NA, NA),
#'     age = c(6, 7, 8.5, 6, 5, 16, NA, 16, 45, 77),
#'     measure_x = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     stringsAsFactors = TRUE
#' )
#' # again print demographics per "conditions":
#' dems_neat(dat, group_by = 'conditions')
#'
#' # another dataset, with no "age"/"gender" columns
#' dat = data.frame(
#'     subject = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     conditions = c('x', 'y', 'x', 'y', 'y', 'x', 'x', 'x', 'y', 'x'),
#'     geschlecht = c(2, 2, NA, NA, 1, 1, 1, 2, NA, NA),
#'     alter = c(6, 7, 8.5, 6, 5, 16, NA, 16, 45, 77),
#'     measure_y = c(83, 71, 111, 70, 92, 75, 110, 111, 110, 85),
#'     stringsAsFactors = TRUE
#' )
#'
#' # the folliwing will return "unknowns"
#' dems_neat(dat, group_by = 'conditions')
#'
#' # age column specified
#' dems_neat(dat, group_by = 'conditions', gender_col = 'geschlecht')
#'
#' # both columns specified
#' dems_neat(dat,
#'           group_by = 'conditions',
#'           age_col = 'alter',
#'           gender_col = 'geschlecht')
#'
#' @export

dems_neat = function(data_per_subject,
                     group_by = NULL,
                     gender_col = NULL,
                     age_col = NULL,
                     percent = FALSE,
                     round_perc = 0,
                     show_fem = NULL) {
    if (class(data_per_subject) == "character") {
        s_dat = eval(parse(text = data_per_subject))
    } else {
        s_dat = data_per_subject
    }
    validate_args(match.call(),
                  list(
                      val_arg(s_dat, c('df')),
                      val_arg(group_by, c('char', 'null')),
                      val_arg(gender_col, c('char', 'null')),
                      val_arg(age_col, c('char', 'null')),
                      val_arg(percent, c('bool'), 1),
                      val_arg(round_perc, c('num'), 1),
                      val_arg(show_fem, c('bool', 'null'), 1)
                  ))
    if (!is.null(age_col)) {
        s_dat$age = s_dat[[age_col]]
    } else if (!'age' %in% names(s_dat)) {
        names(s_dat)[tolower(names(s_dat)) == 'age'] = 'age'
    }
    if (!is.null(gender_col)) {
        s_dat$gender = s_dat[[gender_col]]
    } else if (!'gender' %in% names(s_dat)) {
        names(s_dat)[tolower(names(s_dat)) == 'gender'] = 'gender'
        if (!'gender' %in% names(s_dat)) {
            names(s_dat)[names(s_dat) == 'sex'] = 'gender'
            if (!'gender' %in% names(s_dat)) {
                names(s_dat)[tolower(names(s_dat)) == 'sex'] = 'gender'
            }
        }
    }
    no_age = FALSE
    no_gender = FALSE
    if (!'age' %in% names(s_dat)) {
        no_age = TRUE
        s_dat$age = NA
    }
    if (!'gender' %in% names(s_dat)) {
        no_gender = TRUE
        s_dat$gender = NA
    }
    s_dat$gender = tolower(as.character(s_dat$gender))
    if (all(
        s_dat$gender == '1' |
        s_dat$gender == '2' |
        is.na(s_dat$gender)
    ) == FALSE) {
        s_dat$gender[substring("male", 1, nchar(s_dat$gender)) == s_dat$gender] = '1'
        s_dat$gender[substring("female", 1, nchar(s_dat$gender)) == s_dat$gender] = '2'
        if (all(
            s_dat$gender == '1' |
            s_dat$gender == '2' |
            is.na(s_dat$gender)
        ) == FALSE) {
            stop(
                'The "gender" column must only contain the values 1 (male) or 2 (female) or NA. ',
                'Alternatively, it must only contain the values "male" and "female" (or abbreviations of these).'
            )
        }
    }
    if (is.null(group_by)) {
        s_dat$neat_cond = 0
    } else {
        s_dat$neat_cond = as.factor(eval(parse(
            text = paste0(
                'with(data = s_dat, paste(',
                paste(group_by, collapse = ','),
                ', sep = "_"))'
            )
        )))
    }
    s_dat$age = as.numeric(as.character(s_dat$age))
    s_dat$gender = factor(s_dat$gender, levels = c('1', '2'))
    gender = as.data.frame.matrix(stats::xtabs( ~ neat_cond + gender, s_dat))
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
        if (is.null(show_fem)) {
            show_fem = TRUE
        }
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
            age_gend$age_missing = paste0(' [',
                                          ro(na_age$x.percent, round_perc),
                                          '% unknown]')
            age_gend$gender_missing = paste0(' [',
                                             ro(na_gender$x.percent, round_perc),
                                             '% unknown]')
        } else {
            age_gend$age_missing = paste0(' [', na_age$x.count, ' unknown]')
            age_gend$gender_missing = paste0(' [',
                                             na_gender$x.count,
                                             ' unknown]')
        }
        age_gend$age_missing = paste0(age_gend$age_missing)
    } else {
        if (is.null(show_fem)) {
            show_fem = FALSE
        }
        age_gend$age_missing = ""
        age_gend$gender_missing = ""
    }
    if (percent != FALSE) {
        for (i in 1:nrow(age_gend)) {
            row <- age_gend[i, ]
            thefems = ''
            if (show_fem == TRUE) {
                thefems = paste0(', ', ro(100 - row[7], round_perc), '% female')
            }
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
                '; ',
                ro(row[7], round_perc),
                "% male",
                thefems,
                row[9],
                ")"
            )
        }
    } else {
        for (i in 1:nrow(age_gend)) {
            row <- age_gend[i, ]
            thefems = ''
            if (show_fem == TRUE) {
                thefems = paste0(', ', row[6], ' female')
            }
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
                '; ',
                row[5],
                " male",
                thefems,
                row[9],
                ")"
            )
        }
    }
}
