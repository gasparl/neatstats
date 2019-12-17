## Usage example: pipeline from raw data to reportable statistics

### Material

The data used for the example is available at https://osf.io/49sq5/, under the _example_data_ folder. The data was created artificially to simulate results from a hypothetical experiment. Each file represents the results of a participant who completed a task with positive and negative words (e.g. "happy", "peaceful" or "terror", "evil") displayed in green or red, classifying each word with key presses according to valence (positive vs. negative). The hypotheses are that (a) negative words have slower responses (lower response times; RTs) in general (Valence main effect), and (b) positive words displayed in red and negative words displayed in green have slower responses (Valence x Color interaction). Error rates (ERs) should follow similar pattern (i.e. more incorrect responses are expected in cases where slower responses are expected). However, for about half of the participants, the green and red words were presented in separate blocks: all words shown first in green, and then again all words in red, or vice versa (all in red, then all in green). For these participants, color effects should be absent (due to no polarity-correspondance): no Valence x Color interaction expected in this group.

Each participant's data is given as a simple text file with _.txt_ extention, whose file name contains the experiment title "expsim_color_valence", the given condition ("separate" or "mixed"), and the subject number (1-180), hence, for example, "expsim_color_valence_mixed_1.txt".

In this simplified case, each file contains the following data columns:

- _subject_num_: subject number;
- _condition_: "mixed" for the task with red and green colored words randomly mixed, and "separate" for the task with the two colors separated into two blocks;
- _rt_: RTs, in ms, of the response to each stimulus;
- _response_: "correct" for correct key pressed, "incorrect" for wrong key pressed, or "tooslow" for waiting too long with the response;
- _color_: color of each given presented stimulus ("green" or "red");
- _valence_: color of each given presented stimulus ("positive" or "negative");
- _age_: age of the participant;
- _gender_: gender of the participant (_1_, for male, or _2_, for female).

(The values for _subject_num_, _condition_, _age_, and _gender_ are of course constant for each participant, i.e., have same value in every row.)

The script that is below presented step by step and described in detail, is also available with only brief inline comments at https://osf.io/49sq5/ (as "example_analysis.R").

### Processing the data

First load the library. (If not yet installed, see https://github.com/gasparl/neatstats/)

```R
library('neatStats')
```

Then, set, as current working directory, the path to the folder that contains the data files. If you use RStudio, the `path_neat()` function returns the path of the script file from which this function is executed. This function takes a single argument, which will simply be appended to the path. For example, if the data files are placed into a folder, named "example_data", next to the analysis script, then the `path_neat('example_data')` will return the full path to that folder. Therefore, you can set the current working directory as follows.

```R
setwd(path_neat('example_data'))
```

If you are using anything other than RStudio, you need to give the full path manually, for example, as `setwd("C:/research/proj_neatstats/example_data")`. (To note, `path_neat()` is the only function in the neatStats package, as well as this Example document, that requires RStudio.)

To collect all file names in the directory, you can use the `list.files()` function. Since all result file names start with "expsim_color_valence_", and end with ".txt", you can specify this pattern to make sure that no other files that may be in this folder are collected.

```R
filenames = list.files(pattern = "^color_exp.*txt$")
```

Now that you have the list of all the file names (in the `filenames` variable), you can loop through it, and, for each file name, read in the data from the corresponding file and extract the data that you need. The data from the participants will be merged together in one data frame, named `subjects_merged`, which will contain in each of its row the extracted data of a single participant: namely, condition, age, gender, as well as the mean (aggregated) RTs and error rates for each stimulus type (detailed explanation will follow later).

Before starting the loop, as a precaution I always check if the merged data frame variable (here: `subjects_merged`) already exists (e.g., because the loop has already been run for some reason in the current R session), and remove it if it does. (Otherwise, the loop would add new lines to the existing data frame, hence wrongly including previous data.)

```R
if (exists("subjects_merged")) { rm(subjects_merged) }
```

Now, loop through all data files to extract the relevant data. See detailed explanations following the code.

```R
for (file_name in filenames) {
    cat(file_name, fill = TRUE)
    subject_data = read.table(
        file_name,
        stringsAsFactors = FALSE,
        fill = TRUE,
        header = TRUE
    )
    if (nrow(subject_data) != 100) {
        stop("unexpected trial number: ", nrow(subject_data))
    }
    rts = aggr_neat(
        subject_data,
        rt,
        group_by = c('color', 'valence'),
        method = mean,
        prefix = 'rt',
        filt = (rt > 150 & response == 'correct')
    )
    ers = aggr_neat(
        subject_data,
        response,
        group_by = c('color', 'valence'),
        method = 'incorrect',
        prefix = 'er',
        filt = (response %in% c('correct', 'incorrect'))
    )
    subject_line = table_neat(list(rts, ers), transpose = TRUE)
    er_overall = aggr_neat(subject_data,
                           response,
                           method = 'incorrect',
                           filt = (response %in% c('correct', 'incorrect')))$aggr_value
    subject_line = data.frame(
        subject_id = subject_data$subject_num[1],
        condition = subject_data$condition[1],
        gender = subject_data$gender[1],
        age = subject_data$age[1],
        er_overall = er_overall,
        subject_line
    )
    if (!exists("subjects_merged")) {
        subjects_merged = subject_line
    } else {
        subjects_merged = rbind(subjects_merged, subject_line)
    }
}
```

A preliminary note: to test the code within the loop in detail, we can assign a single file name to the `file_name` variable (e.g., as `file_name = 'expsim_color_valence_mixed_1.txt'`), and then proceed to execute the following lines one by one and check the corresponding results.

The `cat(file_name, fill = TRUE)` line just prints the present file name to the console, to let you know which file is currently being processed. This is especially useful when the script is stopped due to an error: in that case you know which file caused the error.

```R
cat(file_name, fill = TRUE)
```

Then the `read.table()` function reads in the data of the current file. Since the data files contain headers, we have to specify `header = TRUE`, otherwise the returned data would contain the headers as the first row of data. I always set the `stringsAsFactors` and `fill` parameters to the given values as a precaution, although it should not matter in the present data; see `?read.table` for details.

```R
subject_data = read.table(
    file_name,
    stringsAsFactors = FALSE,
    fill = TRUE,
    header = TRUE
)
```

Next, as a quick check to ensure the basic integrity of the data, I always verify that the data contains the expected amount of rows (i.e., the number of trials in the experiment). Otherwise the script is stopped an the "unexpected trial number" (i.e., number of rows) is printed.

```R
if (nrow(subject_data) != 100) {
    stop("unexpected trial number: ", nrow(subject_data))
}
```

We can then get the mean RTs and ERs, per each stimulus type, using `aggr_neat()`. Here there are four stimulus types: (1) positive words in green, (2) positive words in red, (3) negative words in green, and (4) negative words in red. These four combinations can be obtained by grouping by the `color` and `valence` columns. For RTs, we need to get the mean of the values from the `rt` column (for each stimulus type). The `method` is `mean` by default, I write it out explicitly only for clarity. Without giving `prefix`, the default output names for item types would be like, for example, `green_negative`, `red_positive`, etc. (automatically derived from the `group_by` arguments). To clarify that this is our RT measure, we can add 'rt' as prefix, so that the item type names will be as, for example, `rt_green_negative`. Finally, since we are only interested in the RTs of correct responses, we filter for `response == 'correct'`, and, since RTs below 150 ms are probably just accidental (as such fast reactions are extremely unlikely), we also filter for `rt > 150`.

```R
rts = aggr_neat(
    subject_data,
    rt,
    group_by = c('color', 'valence'),
    method = mean,
    prefix = 'rt',
    filt = (rt > 150 & response == 'correct')
)
```

The procedure is similar for ERs, except that the `aggr_neat()` has a special method for getting ratios of specific values (which are otherwise not straightforward with a single function): whenever the argument for the `method` parameter is a string (i.e., text in quotation marks; `character` mode), the ratio of occurances of the given string (in this case, the text `"incorrect"`) in the specified column (here: `response`) is returned (for each stimulus type). Since we usually do not want to include too slow responses in the calculation of error rates, we can filter for `response %in% c('correct', 'incorrect')`, and thereby get the ratio of the number of incorrect responses as compared to the number of correct and incorrect responses. (Of course, in this case the same could be achieved by a filter `response != 'tooslow'`, but perhaps the other one is clearer.) As always, you can check `?aggr_neat` for more details.

```R
ers = aggr_neat(
    subject_data,
    response,
    group_by = c('color', 'valence'),
    method = 'incorrect',
    prefix = 'er',
    filt = (response %in% c('correct', 'incorrect'))
)
```

The `aggr_neat()` function returns the value names (e.g., `rt_green_negative`) and values as columns (with column names `aggr_group` and `aggr_value`). However, in the end we want all these values in a single row. To transform the RT and ER values into a single line (at the same time merging them), we can use the `table_neat()` function. The returned row is assigned to the `subject_line` variable, which will be eventually constitute a single row in the `subjects_merged` data frame.

```R
subject_line = table_neat(list(rts, ers), transpose = TRUE)
```
    
There is however still some additional information we want to add to this row, although these will be single values. For one, we would like to get the overall ER (regardless of stimulus type), because we want to exclude participants with generally very high ER (they may not have been paying attention or had undisclosed vision problems, etc.). For this too, we can use the `aggr_neat()` function, only omitting the `group_by` argument, and appending, at the end, `$aggr_value`, in order to access the single value returned under this column. To note, the same value can also be quite easily obtained without `aggr_neat()`, by writing, for example, `nrow(subject_data[subject_data$response == 'incorrect',]) / nrow(subject_data[subject_data$response %in% c('correct', 'incorrect'),])`. But again, using `aggr_neat()` might be clearer.

```R
er_overall = aggr_neat(subject_data,
                       response,
                       method = 'incorrect',
                       filt = (response %in% c('correct', 'incorrect')))$aggr_value
```

We should now add the overall ER to the `subject_line`, as well as some additional information: _subject_num_, _condition_, _age_, and _gender_. These latter variables values are constant in their respective columns, so we might as well take them from any row, for example the first row (e.g., for _subject_num_: `subject_data$subject_num[1]`). We want to add all these values, as well as the overall ERs obtained previously, to the beginning of the row. The easiest way is probably to create a new data frame, where these single values constitute the first columns, and the original `subject_line` constitutes all the remaining columns.

```R
subject_line = data.frame(
    subject_id = subject_data$subject_num[1],
    condition = subject_data$condition[1],
    gender = subject_data$gender[1],
    age = subject_data$age[1],
    er_overall = er_overall,
    subject_line
)
```

Finally, we append (bind) the `subject_line` to `subjects_merged` if `subjects_merged` exists. If it does not yet exist (as in the first cycle of the loop), we create it by assigning `subject_line` to `subjects_merged` as first row. Hint: if some data may be discrepant for some participants (e.g., some of the participants are tested with blue and yellow colors too), `rbind` will throw an error, but you can use the `rbind.fill` function from the `plyr` package to automatically pair the matching columns (by name) and fill in missing data with NAs.

```R
if (!exists("subjects_merged")) {
    subjects_merged = subject_line
} else {
    subjects_merged = rbind(subjects_merged, subject_line)
}
```

When running the full _for loop_, the above described steps are repeated for each data file. After the loop has been run, the `subjects_merged` data frame is ready for statistical analysis. It might be worth noting that, while there is a "subject_id" column in this `subjects_merged` data frame, this is merely to keep track of records, but none of the statistical functions below require it: each participant is represented by a single row in the data frame, hence no additional identifier is needed.

### Statistics

At this point you might want to list column names, using `str(subjects_merged)`, for a quick overview of the content as well as for the convenient copy-pasting for subsequent use in statistical functions.

Before any tests, we exclude subjects with overall error rate larger than 20%.

```R
data_final = subjects_merged[subjects_merged$er_overall < 0.20, ]
```

Next, we can calculate number of exclusions by adding a column `remains` to `subjects_merged` with the value `'remained'` wherever the `subject_id` (in `subjects_merged`) is not in the `final_data`, and the value `'excluded'` otherwise.

subjects_merged$remains = ifelse(subjects_merged$subject_id %in% data_final$subject_id,
                                 'remained',
                                 'excluded')

Thus we can list number of exclusions and number of remaining participants per condition using another aggregation.

aggr_neat(
    dat = subjects_merged,
    values = subject_id,
    group_by = c('condition', 'remains'),
    method = length
)

This shows three exclusions in the 'mixed' condition, and two in the 'separate' condition, leaving 87 and 89, respectively.

Moving on to the first (descriptive) statistics, the `dems_neat()` gives the average age, and number of males (or percentage, if so set), using (automatically) the `age` and `gender` columns.

dems_neat(data_final, group_by = 'condition')

The console output is:

>Group < mixed >: 87 subjects (age = 24.2±3.8, 49 male)  
>Group < separate >: 89 subjects (age = 24.8±3.3, 45 male)

To start with the main analysis, we can first take a look at the means of the main independent variables in a factorial plot. Since each participant may have several variables of interest (in case of a within-subject design such as in this example), all variables to be included in the test are given using their column names (as strings) in a string vector (or, in case of no within-subject factors, as a single string element), as the `values` parameter. To determine which within-subject factors we want to contrast in the plot (using the given `values`), there is a `within_ids` parameter that accepts a list as argument. In this list, the name of each element is the chosen display name for each factor; in this case "color" and "valence" (but we could use any other names as well). Each element must contain a vector of names that are used to identify which of the value names (given as `values`) belong to which factor. For example, the Color factor is given as `color = c('green', 'red')`: using the given words `'green'` and `'red'`, the given variable (or value) names `'rt_green_negative'` and `'rt_green_positive'` will be automatically identified as `'green'` (since they contain the string `'green'`), while the values `'rt_red_negative'` and `'rt_red_positive'` will be identified as `'red'` (since they contain the string `'red'`). The between subject variables can simply given assigned to the `between_vars` parameter as a string vector, or, in case of only one between-subject factor (as in this example), as a single string element.

```R
plot_neat(
    data_per_subject = data_final,
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
```

![Mean+sd plot.](example_images/example_image_1.png)

All seems as expected. But how certain are we about these apparent differences? Even before statistical comparisons, we can take a look at the 95% CIs of the means.

```R
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
```

![Mean+ci plot.](example_images/example_image_2.png)

Seems convincing. (Although, as a side note: in some cases such a plot can actually lead one to underestimate the certainty because it gives no information about the correlation of within-subject variables, which, e.g. in case of RTs, can be extremely high, _r_ > 0.9, hence potentially giving substantial evidence despite very small mean differences.) 

The main method could be replaced as well, for example, by setting `method = median` to get medians instead of means to control for outliers and see whether the picture changes then. (The corresponding error bars could be median absolute deviation; `eb_method = mad`.)

The plots could be repeated for error rates by simply replacing "rt_" with "er_" in the four variable names for the `values` parameter. Similarly, all the tests below would be the same for ERs (except for changing the variable input), but these are omitted here for brevity.

Now let's do an actual test: ANOVA on RTs for the Color x Valence x Group interaction. The arguments are identical to the ones for `plot_neat()` (and have the same logic), only here we add `bf_added = FALSE`: the calculation of Bayes factors (BFs) can take some time with this many factors, so for now we omit it.

```R
anova_neat(
    data_final,
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
```

While the rest of the numbers will always be identical for the same data, the BF can vary slightly (typically only in fracional digits) due to its inherent random sampling process. My specific out put is:

>F(1,174) = 29507.56, p < .001, ηp2 = .994, 90% CI [.993, .995], ηG2 = .991. ((Intercept))  
>F(1,174) = 27.03, p < .001, ηp2 = .134, 90% CI [.065, .213], ηG2 = .094. (condition)  
>F(1,174) = 0.78, p = .379, ηp2 = .004, 90% CI [0, .035], ηG2 = .001. (color)  
>F(1,174) = 223.33, p < .001, ηp2 = .562, 90% CI [.483, .622], ηG2 = .112. (valence)  
>F(1,174) = 0.11, p = .736, ηp2 = .001, 90% CI [0, .019], ηG2 < .001. (color × condition)  
>F(1,174) = 0.02, p = .888, ηp2 < .001, 90% CI [0, .009], ηG2 < .001. (condition × valence)  
>F(1,174) = 56.89, p < .001, ηp2 = .246, 90% CI [.159, .330], ηG2 = .038. (color × valence)  
>F(1,174) = 34.92, p < .001, ηp2 = .167, 90% CI [.090, .248], ηG2 = .024. (color × condition × valence)  

Without going into details, the three-way interaction is significant. (To note, the statistics are as close to as possible to reportable format, but italics, subscripts, and superscripts are not well supported as console outputs - hence these have to be adjusted when preparing a manuscript.)

You follow up (as preregistered of course) with two separate ANOVAs to show the absence of Color x Valence interaction `separate` condition, and its presence in the `mixed` condition.

```R
anova_neat(
    data_final[data_final$condition == 'separate', ],
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    )
)
```

My output is:
>F(1,88) = 13114.19, p < .001, ηp2 = .993, 90% CI [.991, .995], ηG2 = .990. ((Intercept))  
>F(1,88) = 0.72, p = .398, ηp2 = .008, 90% CI [0, .064], ηG2 = .001, BF01 = 6.25. (color)  
>F(1,88) = 117.52, p < .001, ηp2 = .572, 90% CI [.456, .650], ηG2 = .107, BF10 = 2.15 × 10^16. (valence)  
>F(1,88) = 1.27, p = .262, ηp2 = .014, 90% CI [0, .079], ηG2 = .002, BF01 = 3.14. (color × valence)  

As expected, no significant interaction. The BF for the interaction is also just large enough to be labeled as substantial evidence for equivalence. (If you are not convinced, you can use the _data_generation_code.R_ script at https://osf.io/49sq5/ to "take more participants", and rerun the test with the increased sample size.) To note, BFs supporting equivalence (i.e., are below 1) are always inverse, hence all BFs displayed are above 1, and support for equivalence is indicated by the numbers 01, such as in BF01 (as opposed to BF10, for BF supporting difference). When assigning the `anova_neat()` function (e.g., `my_results = anova_neat(...)`), it will return a list that contains, among other things, the exact values of the statistics for each effect, including unconverted BFs.

Now, for the `mixed` condition.

```R
anova_neat(
    data_final[data_final$condition == 'mixed', ],
    values = c(
        'rt_green_negative',
        'rt_green_positive',
        'rt_red_negative',
        'rt_red_positive'
    ),
    within_ids = list(
        color = c('green', 'red'),
        valence = c('positive', 'negative')
    )
)
```

My output is:
 >F(1,86) = 16701.63, p < .001, ηp2 = .995, 90% CI [.993, .996], ηG2 = .992. ((Intercept))  
 >F(1,86) = 0.15, p = .699, ηp2 = .002, 90% CI [0, .041], ηG2 < .001, BF01 = 8.11. (color)  
 >F(1,86) = 106.02, p < .001, ηp2 = .552, 90% CI [.432, .635], ηG2 = .117, BF10 = 4.15 × 10^12. (valence)  
 >F(1,86) = 106.33, p < .001, ηp2 = .553, 90% CI [.433, .635], ηG2 = .121, BF10 = 7.01 × 10^17. (color × valence)  

Interaction significant as expected. Now to explore the interaction in the `mixed` condition, we could do various _t_-tests (four in "parallel" and even two "crosswise"), but perhaps what's interesting is to check whether there is a significant difference between red and green in case of either positive or negative words.

First, for convenience, I create a new data frame with only `mixed` condition.

```R
subjects_mx = data_final[data_final$condition == 'mixed',]
```

Now test red versus green for positive words.

```R
t_neat(subjects_mx$rt_green_positive,
       subjects_mx$rt_red_positive,
       pair = TRUE)
```

>Correlation: r(85) = .613, 95% CI [.462, .729], p < .001.  
>Descriptives: M±SD = 538.72±51.31 vs. 574.93±56.86 (raw mean difference: 36.20, 95% CI [–46.40, –26.01])  
>t(86) = –7.06, p < .001, d = –0.76, 95% CI [–0.99, –0.52], BF10 = 2.45 × 10^7.  

(Along with descriptives, in case of paired samples, `t_neat()` by default also prints the correlation between the two tested variables.)

Now red versus green for negative words.

```R
t_neat(subjects_mx$rt_green_negative,
       subjects_mx$rt_red_negative,
       pair = TRUE)
```

>Correlation: r(85) = .454, 95% CI [.269, .606], p < .001.  
>Descriptives: M±SD = 613.31±48.62 vs. 574.20±46.57 (raw mean difference: –39.11, 95% CI [28.50, 49.72])  
>t(86) = 7.33, p < .001, d = 0.79, 95% CI [0.54, 1.02], BF10 = 8.06 × 10^7.  

Both significant. All left to do is print a nice table to show means and SDs as customary.

```R
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
```

This will produce a table as follows. (Well, this table here is formatted with Markdown notation, but the names and numbers are verbatim.)

| aggr_group | rt_green_negative | rt_green_positive | rt_red_negative | rt_red_positive | er_green_negative | er_green_positive | er_red_negative | er_red_positive |
|------------|-------------------|-------------------|-----------------|-----------------|-------------------|-------------------|-----------------|-----------------|
| mixed      | 612.79±48.00      | 537.93±50.81      | 574.11±46.99    | 574.40±56.28    | 0.14±0.08         | 0.09±0.06         | 0.15±0.08       | 0.15±0.07       |
| separate   | 564.45±57.23      | 522.21±54.49      | 556.85±55.88    | 523.64±49.57    | 0.14±0.07         | 0.10±0.07         | 0.16±0.09       | 0.10±0.05       |


This is not so nice though: let's modify rounding of RTs to zero, and convert error rates to percentages. (For the latter, you need to use a vector input, so in this case the original column values, e.g. `subjects_merged$er_green_negative`, multiplied manually by `100`.)

Also, here I set `to_clipboard = TRUE`, which puts the table on your clipboard with plain format. This you can copy into Excel, and from that to Word, and you have the table. (Unfortunately Word doesn't produce nice table when copied there directly.)

(Note: you can also add new names for each column by the `aggr_neat`'s `new_name` parameter, but I didn't; it may be clearer to keep the original names here, and just rename them in the final table in Word.)

```R
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
```
| aggr_group | rt_green_negative | rt_green_positive | rt_red_negative | rt_red_positive | data_final$er_green_negative * 100 | data_final$er_green_positive * 100 | data_final$er_red_negative * 100 | data_final$er_red_positive * 100 |
|------------|-------------------|-------------------|-----------------|-----------------|------------------------------------|------------------------------------|----------------------------------|----------------------------------|
| mixed      | 613±49            | 539±51            | 574±47          | 575±57          | 13.15±7.47                         | 8.69±6.15                          | 14.46±7.78                       | 14.87±7.06                       |
| separate   | 564±58            | 522±55            | 556±56          | 523±50          | 14.09±6.82                         | 9.35±6.94                          | 15.94±9.32                       | 10.22±5.52                       |


### That's all

You have everything you need to report from this (hypothetical) experiment. This is of course a single specific case, but I think it is fairly easy to generalize to most typical designs used in psychological science.
