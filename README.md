**R package in testing**

**To install in R, just run:**

install.packages("devtools")

library(devtools)

install_github("gasparl/neatstats")

**For t-tests, use t_neat:**

tneat( var1, var2, pair = F, greater = "" )

var1: first data vector

var2: second data vector

pair: True for paired (within-subject) test, False for independent (between-subject) test

greater: "1" for one-sided test with _first_ data assumed to be greater, "2" for one-sided test with _second_ data assumed to be greater.

**For ANOVAs, use anova_neat:**

anova_neat( data_long, value_col, id_col, between_vars = NULL, within_vars = NULL, bf_added = T ) {

data_long: dataset name

value_col: value_col

id_col: unique identifier, e.g. participant ID

between_vars: all columns for between-subject factors to be included, separated by commas

within_vars: all columns for within-subject factors to be included, separated by commas

bf_added: True to include Bayes factors, False to omit them (which makes the test run faster)

For anova_neat, all parameters have to be given as strings (i.e., in quotes).

Example:

anova_neat(data_long = "my_data", value_col = "rts", id_col = "subject_id", between_vars = "task_version", within_vars = "item_types, session" )


**To be continued...**