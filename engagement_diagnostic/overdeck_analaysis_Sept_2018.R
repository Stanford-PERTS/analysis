# We need to answer a few questions from Overdeck regarding the 2018 Spring EP data


############ SETTINGS ############
options(stringsAsFactors = FALSE)
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"
#github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/no-install/"
library(lubridate)
library(dplyr)
library(knitr)
library(markdown)
library(rmarkdown)
library(purrr)
library(tidyr)

"%+%" <- function (x, y) { paste(x, y, sep="") }

source(github_base_path %+% "R/util.R")
gymnast_install() # this shouls be added separately since now we try no to run
# anything inside the util functions (requirement for Rserve proper functioning)
source(github_base_path %+% "R/util_data_summaries.R")
source(github_base_path %+% "R/util_qualtrics_cleaning.R")
source(github_base_path %+% "R/util_graphing.R")
source(github_base_path %+% "R/util_scale_computation.R")
source(github_base_path %+% "R/util_dfc.R")



############### PREPARE DATA  ###############

qc_path <- "/Users/rumen/Downloads/Engagement_Project_OLD_20172018.csv" # if you change the name, chnage it in also qualtrics_api.R

qc_df <- read.csv(qc_path) %>% qc.clean_qualtrics()

# remove testing
qc_df <- qc_df %>% dplyr::filter(!testing == "true")

# remove empty ids
qc_df <- qc_df %>% dplyr::filter(!util.is_blank(participant_id))


# remove missing code
qc_df <- qc_df %>% dplyr::filter(!util.is_blank(code))


qc_df$start_date <- qc_df$StartDate %>% as.Date()
participation_range <- qc_df %>%
  group_by(code) %>%
  summarise(time_range = max(start_date) - min(start_date))



all_lc <- c("tc1_1", "tc2_1", "tc4_1",
            "mw1_1", "mw2_1", "mw3_1",
            "fg1_1", "fg2_1", "fg3_1")

#check_ranges
for (lc in all_lc) {
  print(lc)
  print(min(qc_df[,lc], na.rm = T))
  print(max(qc_df[,lc], na.rm = T))
}
# same ranges, so you can take averages

# assign weeks
qc_df$year_week <- paste0(lubridate::year(qc_df$StartDate), "-", str_pad(lubridate::week(qc_df$StartDate),2, pad = "0"))

# filter to Spring 2018 only
qc_df <- qc_df %>% filter(as.Date(start_date) >= as.Date("2018-03-01"))


# take average for each student for each week

weekly_scores_df <- qc_df %>% group_by(participant_id, year_week) %>%
  summarise(
    tc1_1 = mean(tc1_1, na.rm = T),
    tc2_1 = mean(tc2_1, na.rm = T),
    tc4_1 = mean(tc4_1, na.rm = T),
    mw1_1 = mean(mw1_1, na.rm = T),
    mw2_1 = mean(mw2_1, na.rm = T),
    mw3_1 = mean(mw3_1, na.rm = T),
    fg1_1 = mean(fg1_1, na.rm = T),
    fg2_1 = mean(fg2_1, na.rm = T),
    fg3_1 = mean(fg3_1, na.rm = T),
    eng_1 = mean(eng_1, na.rm = T),
    code = first(code)
  ) %>% arrange(participant_id, (year_week))

weekly_scores_df$lc_means <- rowMeans(weekly_scores_df[, all_lc], na.rm = T)

first_last_df <- weekly_scores_df %>% group_by(participant_id) %>%
  summarise(
    first_mean = first(na.omit(lc_means)),
    last_mean = last(na.omit(lc_means)),
    first_eng_1_mean = first(na.omit(eng_1)),
    last_eng_1_mean = last(na.omit(eng_1)),
    first_week = first(year_week),
    last_week = last(year_week),
    code = first(code),
    n_weeks = n()
  ) %>%
  mutate(
    diff = last_mean - first_mean,
    diff_eng_1 = last_eng_1_mean - first_eng_1_mean )

# remove those who do not have measures for more than one week
first_last_df <- first_last_df %>% filter(n_weeks > 1)

# quickly check overall means
mean(first_last_df$diff, na.rm = T)
mean(first_last_df$diff_eng_1, na.rm = T)

class_diffs <- first_last_df %>% group_by(code) %>%
  summarise(
    mean_diff = mean(diff, na.rm = T), # this is the mean of all students lc differences within a class
    mean_diff_eng_1 = mean(diff_eng_1, na.rm = T), # this is the mean of all students engagement differences within a class
    mean_weeks = mean(n_weeks, na.rm = T),
    n = n()
  ) %>%
  filter(n >= 5)

View(class_diffs)


############################################################################################
# Question:
#(Leading impact) Percent of classrooms with improved learning conditions. That is, classrooms with a positive (unimputed) delta from first to last observation across all learning conditions averaged together across all of their students.


# remove NAs and compute percentage of classes with positive delta
# limit to classrooms of 5 or larger
class_diffs %>%
  filter(!is.na(mean_diff)) %>%
  summarise(
    proportion_positive = sum(mean_diff > 0, na.rm = T)/sum(!is.na(mean_diff))
  )


#Answer: 55% of all classrooms participating in 2017/18 had positive delta

# check if thse numbers depend on sample size and length of testing




############################################################################################
# Question:
#(Leading impact) Percent of classrooms with improved effort. That is, classrooms with a positive (unimputed) delta from first to last observation across the effort question (eng_1).


class_diffs %>%
  filter(!is.na(mean_diff_eng_1)) %>%
  summarise(
    proportion_positive = sum(mean_diff_eng_1 > 0, na.rm = T)/sum(!is.na(mean_diff_eng_1))
  )

# Additional checks:
# 36% ... that is surprisingly low
# I will check if it depends on how many weeks the survey has been running for


class_diffs$mean_weeks_int <-  class_diffs$mean_weeks %>% round(.,0)
class_diffs$pos_eng <- ifelse(class_diffs$mean_diff_eng_1 > 0, 1,0)
class_diffs$pos_eng[is.na(class_diffs$mean_diff_eng_1)] <- NA
class_diffs$pos_lc <- ifelse(class_diffs$mean_diff > 0, 1,0)
class_diffs$pos_lc[is.na(class_diffs$mean_diff)] <- NA


# check how proption varies by number of weeks surveyed
class_diffs %>% group_by(mean_weeks_int) %>%
  summarise(
    mean_diff = mean(mean_diff, na.rm = T),
    mean_diff_eng_1 = mean(mean_diff_eng_1, na.rm = T),
    pos_eng = sum(pos_eng) / n(),
    pos_lc = sum(pos_lc) / n(),
    n = n())

# check how proption varies by class size
x <- class_diffs %>% group_by(n) %>%
  summarise(
    mean_diff = mean(mean_diff, na.rm = T),
    mean_diff_eng_1 = mean(mean_diff_eng_1, na.rm = T),
    pos_eng = sum(pos_eng) / n(),
    pos_lc = sum(pos_lc) / n(),
    n_count = n())
View(x)


# it seems the answer is correct, in most classrooms the delta is negative for engagement
# and positive for learning conditions

# Answer: 36% show positive delta
############################################################################################




############################################################################################
# Question:
#(Leading impact) Students learning in more engaging conditions. That is, what is the total # of students in the classrooms with LC improvements that were reported above under #1?

# list classes with positive delta
pos_class_codes <- class_diffs$code[class_diffs$mean_diff > 0]
qc_df$participant_id[class_diffs$code %in% pos_class_codes] %>% unique %>% length
# Answer: 2183 unique students were in classes which showed positive delta
############################################################################################

############################################################################################
# Question:
# How many teachers surveyed students at least 3 times?
qc_df %>%
  group_by(code) %>%
  summarise(weeks_surveyed = length(unique(year_week))) %>%
  filter(weeks_surveyed >= 3) %>%
  nrow()

# Answer: 106
############################################################################################







########### OTHER TESTS ##############
# check if there is overall downward trend in time. I am still confuese why eng is less than .5
qc_df$lc_all <-  qc_df[, all_lc] %>% rowMeans(na.rm = T)
buff_df <- qc_df %>%
  group_by(code, year_week) %>%
  summarise(
    lc_all = mean(lc_all, na.rm = T),
    eng_1 = mean(eng_1, na.rm = T),
  )  %>%
  arrange(code, year_week) %>%
  group_by(code) %>%
  mutate(row_index = row_number(),
         n_weeks = n()) %>%
  filter(n_weeks >3) %>%
  filter(!is.na(eng_1)) %>%
  summarise(cor1 = cor.test(eng_1,row_index)$estimate,
            cor2 = cor.test(lc_all,row_index)$estimate,
            n_weeks = first(n_weeks)) %>%
  group_by(n_weeks) %>%
  summarise(mean_cor_eng = mean(cor1, na.rm = T),
            mean_cor_lc = mean(cor2, na.rm = T),
            n = n()) %>%
  as.data.frame()

# It seems that more sessions lead to stronger engagement over time




