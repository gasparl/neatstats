### Usage example: pipeline from raw data to reportable statistics



# you can completely ignore the following function
# it serves merely to simulate example data
next_subject = function(sub_num) {
    N = 150
    sub_dat = data.frame(
        subject_num = toString(sub_num),
        condition = sample(c('fullvision', 'colorblind'), 1),
        rt = rnorm(n = N, mean = 400, sd = 150),
        response = sample(
            c(rep('correct', 9), 'incorrect', 'tooslow'),
            size = N,
            replace = TRUE
        ),
        color = sample(c('red', 'green'), size = N, replace = TRUE),
        valence = sample(
            c('positive', 'negative'),
            size = N,
            replace = TRUE
        )
    )
    if (sub_dat$condition[1] == 'fullvision') {
        green_neg = (sub_dat$color == 'green' &
                         sub_dat$valence == 'negative')
        sub_dat$rt[green_neg] = sub_dat$rt[green_neg] + rnorm(n = length(sub_dat$rt[green_neg]),
                                                              mean = 43,
                                                              sd = 30)
        sub_dat$response[green_neg] = sample(
            c(rep('correct', 6), 'incorrect', 'tooslow'),
            size = length(sub_dat$response[green_neg]),
            replace = TRUE
        )
        red_pos = (sub_dat$color == 'red' &
                       sub_dat$valence == 'positive')
        sub_dat$rt[red_pos] = sub_dat$rt[red_pos] + rnorm(n = length(sub_dat$rt[red_pos]),
                                                          mean = 37,
                                                          sd = 30)
        sub_dat$response[red_pos] = sample(
            c(rep('correct', 6), 'incorrect', 'tooslow'),
            size = length(sub_dat$response[red_pos]),
            replace = TRUE
        )
    }
    neg = (sub_dat$valence == 'negative')
    sub_dat$rt[neg] = sub_dat$rt[neg] + rnorm(n = length(sub_dat$rt[neg]),
                                              mean = 40,
                                              sd = 25)
    sub_dat$response[neg] = sample(
        c(rep('correct', 6), 'incorrect', 'tooslow'),
        size = length(sub_dat$response[neg]),
        replace = TRUE
    )
    return(sub_dat)
}
# the variable below simulates file names
filenames = 1:60

# let's say you have data files from an experiment
# all files start as "color_exp", and they are all ".txt"
# you can collect all of them from a given folder:
#

if (exists("subjects_merged")) {
    rm(subjects_merged)
}

# now loop thru all data files
for (file_name in filenames) {
    subject_data = next_subject(file_name)
    # with real data, this would be e.g.:
    # subject_data = read.table(file_name, stringsAsFactors=F, fill=T, header=T)
    
    # print current file name - just to monitor the process
    cat(file_name, ' ')
    
    # now aggregate rt data per type
    rts = aggr_neat(
        subject_data,
        rt,
        group_by = 'color, valence',
        method = mean,
        prefix = 'rt'
    )
    # same with error rates
    ers = aggr_neat(
        subject_data,
        response,
        group_by = 'color, valence',
        method = 'incorrect',
        prefix = 'er',
        filt = (response %in% c('correct', 'incorrect'))
    )
    # transpose to get the subject's data in one line
    subject_line = table_neat(list(rts, ers), transpose = TRUE)
    # add the subject_id and condition to the beginning
    subject_line = data.frame(
        subject_id = subject_data$subject_num[1],
        condition = subject_data$condition[1],
        subject_line
    )
    
    # merge aggregated subject data
    if (!exists("subjects_merged")) {
        # if doesn't yet exist, create first line
        subjects_merged = subject_line
    } else {
        # if exists, add the next lines
        subjects_merged = rbind(subjects_merged, subject_line)
        # note: if some data may be discrepant for some participants (e.g., some
        # participants are tested with blue and yellow colors too), you can use
        # rbind.fill from the 'plyr' library to fill in missing data with NAs
    }
}
# data is ready for analysis

# you can list column names, for convenient copy-pasting as:
# names(subjects_merged)


plot_neat(
    data_per_subject = subjects_merged,
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    between_vars = 'condition'
)


# how sure are we about these apparent differences? even before statistical comparisons, we can take a look at the CIs of the means

plot_neat(
    data_per_subject = subjects_merged,
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    between_vars = 'condition',
    eb_method = mean_ci
)

# that seems convincing. still, we can also look at medians and median absolute deviation to control for outliers and see whether the picture changes then

plot_neat(
    data_per_subject = subjects_merged,
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    between_vars = 'condition',
    method = median,
    eb_method = mad
)

# pretty much the same.

# now ANOVA on RTs for the main question: Color/Valence/Group interaction
anova_neat(
    subjects_merged,
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    between_vars = 'condition',
    bf_added = FALSE
)

anova_neat(
    subjects_merged[subjects_merged$condition == 'colorblind', ],
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    bf_added = T
)

anova_neat(
    subjects_merged[subjects_merged$condition == 'fullvision', ],
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    ),
    bf_added = T
)

subjects_fullv = subjects_merged[subjects_merged$condition == 'fullvision', ]

t_neat(subjects_fullv$rt_green_negative,
       subjects_fullv$rt_red_negative,
       pair = T)

t_neat(subjects_fullv$rt_green_positive,
       subjects_fullv$rt_red_positive,
       pair = T)


# table to show basic data
table_neat(
    list(
        aggr_neat(subjects_merged, rt_green_negative),
        aggr_neat(subjects_merged, rt_green_positive),
        aggr_neat(subjects_merged, rt_red_negative),
        aggr_neat(subjects_merged, rt_red_positive),
        aggr_neat(subjects_merged, er_green_negative),
        aggr_neat(subjects_merged, er_green_positive),
        aggr_neat(subjects_merged, er_red_negative),
        aggr_neat(subjects_merged, er_red_positive)
    ),
    group_by = 'condition'
)

table_neat(
    list(
        aggr_neat(subjects_merged, rt_green_negative, round_to = 0),
        aggr_neat(subjects_merged, rt_green_positive, round_to = 0),
        aggr_neat(subjects_merged, rt_red_negative, round_to = 0),
        aggr_neat(subjects_merged, rt_red_positive, round_to = 0),
        aggr_neat(subjects_merged, subjects_merged$er_green_negative * 100),
        aggr_neat(subjects_merged, subjects_merged$er_green_positive * 100),
        aggr_neat(subjects_merged, subjects_merged$er_red_negative * 100),
        aggr_neat(subjects_merged, subjects_merged$er_red_positive * 100)
    ),
    group_by = 'condition',
    to_clipboard = TRUE
)
