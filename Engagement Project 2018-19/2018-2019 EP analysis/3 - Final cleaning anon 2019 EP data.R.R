# 3 - Final cleaning anon 2019 EP data.R
# Daniel Greene
# 6/28/2019

# The purpose of this script is to take the k-anon cleaned data and do some final checks and variable creation
# before it's ready for analysis.


############### USER-DEFINED INPUTS #####################################

general_data_folder_crypt_path <- "Data for 2018-2019 EP analysis"
input_data_crypt_path <- "Data for 2018-2019 EP analysis/3 - 2018-2019 k-anon cleaned data.csv"


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

### Test teams info
d <- util.find_crypt_paths(list(a = input_data_crypt_path))$a %>%
  read.csv()


############### CREATE/RENAME NEW VARIABLES FOR ANALYSIS #####################################

# rename vars
d <- rename(d,
            effort = eng_1,
            expected_grade = va_grade_1,
            prior_gpa = ba_gpa_1)

# Define individual LC scores
d$fg <- rowMeans(d[, c("fg1_2", "fg2_2", "fg3_2")], na.rm = TRUE)
d$mw <- rowMeans(d[, c("mw1_2", "mw2_2", "mw3_2")], na.rm = TRUE)
d$tc <- rowMeans(d[, c("tc1_2", "tc2_2", "tc4_2")], na.rm = TRUE)


# Define comb_lc_score
# It's NA if it is not composed from at least one LC question
lc_qs <- c("fg1_2", "fg2_2", "fg3_2",
           "mw1_2", "mw2_2", "mw3_2",
           "tc1_2", "tc2_2", "tc4_2")
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


# create diff-based variables and first_comb_lc_score
d <- d %>%
arrange(student_id, week_start) %>%
  group_by(student_id) %>%
  mutate(survey_ordinal = 1:length(student_id),
         comb_lc_score_diff = comb_lc_score - lag(comb_lc_score),
         effort_diff = effort - lag(effort),
         week_start = ymd(week_start),
         weeks_between_obs = round((week_start-lag(week_start))/7),
         first_comb_lc_score = first(na.omit(comb_lc_score)))


# choose demographic variables for analysis: gender_3 and race_3
# and drop the others
d$gender <- d$gender___3
d$race <- d$race___3
d$gender[d$gender %in% "__masked__"] <- "masked"
d$race[d$race %in% "__masked__"] <- "masked"
k_anon_vars <- names(d)[grep("(gender___)|(race___)", names(d))]
d[, k_anon_vars] <- NULL


# Make scales
d$gms <- rowMeans(d[, c("gms_2.0", "gms_3.0")], na.rm = TRUE)
d$belonging <- rowMeans(d[, c("belong_4", "belong_3.1")], na.rm = TRUE)
d$teacher_mastery <- rowMeans(d[, c("mastery1", "mastery2")], na.rm = TRUE)

# Cut unneeded vars
# note SG realized she needed some of these, so commented them out and renamed the data output file to v1.1
unneeded_vars <- c( "any_fg", "any_mw", "any_tc", "at_least_one_from_each_lc", "comb_lc_score_or_zero_if_na", 
                    "class_mean_comb_lc_score", "n_students_in_class_week"#, "gms_2.0", "gms_3.0",
                    #"belong_4", "belong_3.1", "mastery1", "mastery2"
                    )
d[, unneeded_vars] <- NULL


############### FINAL SANITY CHECKS #####################################

# What's the NA count for different vars?
ds.summarize_by_column(d)
# About 50-60% of the values are NA for the diff vars because they require multiple measures per student.


############### SAVE #####################################

generic_save_path <- util.find_crypt_paths(list(a = general_data_folder_crypt_path))$a
write.csv(d, generic_save_path %+% "/4 - 2018-2019 cleaned data for PERTS analysis v1.1.csv")

