# 2 - Cleaning k-anon 2017-2018 EP data.R
# Daniel Greene
# 7/10/2019

# The purpose of this script is to take the 2018 k-anon cleaned data and do some final checks and variable creation
# before it's ready for analysis.


############### USER-DEFINED INPUTS #####################################

general_data_folder_crypt_path <- "old 2017-2018 EP data"
input_data_crypt_path <- "old 2017-2018 EP data/2 - EP 2017-2018 k-anon data.csv"


############### LOAD LIBRARIES AND PATHS #####################################

github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"

tryCatch({
  source("~/Sites/gymnast/R/util.R")
  gymnast_install()
  library(tidyverse)
  source("~/Sites/gymnast/R/util_data_summaries.R")
  source("~/Sites/gymnast/R/util_qualtrics_cleaning.R")
}, error = function(e){
  source(github_base_path %+% "R/util.R")
  gymnast_install()
  source(github_base_path %+% "R/util_data_summaries.R")
  source(github_base_path %+% "R/util_qualtrics_cleaning.R")
})

library(tidyverse)
library(lubridate)


############### LOAD DATA #####################################

d <- util.find_crypt_paths(list(a = input_data_crypt_path))$a %>%
  read.csv()


############### CREATE/RENAME NEW VARIABLES FOR ANALYSIS #####################################

# rename vars
d <- rename(d,
            effort = eng_1,
            expected_grade = va_grade_1,
            prior_gpa = ba_gpa_1)

# Define comb_lc_score
# It's NA if it is not composed of at least 1 question
# NOTE: slightly different versions of questions than in 2018-2019.
lc_qs <- c("fg1_1", "fg2_1", "fg3_1",
           "mw1_1", "mw2_1", "mw3_1",
           "tc1_1", "tc2_1", "tc4_1")
d$comb_lc_score <- rowMeans(d[, lc_qs], na.rm = TRUE)


# define comb_lc_score_classmates:
# find the overall mean and n-students for each class-week
# use the above to calculate what the overall mean would be without a given student's comb_lc_score
d <- d %>%
  group_by(class_id, week_start) %>%
  mutate(class_mean_comb_lc_score = mean(comb_lc_score, na.rm = TRUE),
         n_students_in_class_week = length(student_id),
         comb_lc_score_classmates = (class_mean_comb_lc_score*n_students_in_class_week - comb_lc_score) /
                                    (n_students_in_class_week-1) )
# Exceptional cases:
# Class size of 1: comb_lc_score_classmates is NA bc there are no classmates
d$comb_lc_score_classmates[d$n_students_in_class_week %in% 1] <- NA
# Your own comb_lc_score is NA: comb_lc_score_classmates is class_mean_comb_lc_score because your NA score doesn't contribute
d$comb_lc_score_classmates[is.na(d$comb_lc_score)] <- d$class_mean_comb_lc_score[is.na(d$comb_lc_score)]


# create variables
d <- d %>%
arrange(student_id, week_start) %>%
  group_by(student_id) %>%
  mutate(survey_ordinal = 1:length(student_id),
         week_start = ymd(week_start),
         first_comb_lc_score = first(na.omit(comb_lc_score)))

# choose demographic variables for analysis: gender_3 and race_3
# and drop the others
d$gender <- d$gender___3
d$race <- d$race___3
d$gender[d$gender %in% "__masked__"] <- "masked"
d$race[d$race %in% "__masked__"] <- "masked"
k_anon_vars <- names(d)[grep("(gender___)|(race___)", names(d))]
d[, k_anon_vars] <- NULL

# recode expected_grade
d$expected_grade <- util.recode(d$expected_grade,
                                c("A", "B", "C", "D", "F", "I don't get a grade in this class."),
                                c(4, 3, 2, 1, 0, NA)) %>%
  as.numeric()

# recode prior_gpa
d$prior_gpa <- d$prior_gpa - 1


# Would like to make scales,
# but we have almost NO data for them. So we skip.
# ds.summarize_by_column(d[, c("gms_2.0", "gms_3.0", "gms_3.1", "va_gms_1")])
# ds.summarize_by_column(d[, c("belong_2.0", "belong_3.0")])
# ds.summarize_by_column(d[, c("mastery1", "mastery2")])


# Only save the needed vars
needed_vars <- c("effort", "expected_grade", "prior_gpa", "comb_lc_score", "comb_lc_score_classmates", "first_comb_lc_score",
                 "student_id", "class_id", "teacher_id", "team_id", "week_start", "survey_ordinal",
                 "gender", "race")
d <- d[, needed_vars]


############### FINAL SANITY CHECKS #####################################

# What's the NA count for different vars?
ds.summarize_by_column(d)

############### SAVE #####################################

generic_save_path <- util.find_crypt_paths(list(a = general_data_folder_crypt_path))$a
write.csv(d, generic_save_path %+% "/3 - EP 2017-2018 clean data ready for analysis.csv")

