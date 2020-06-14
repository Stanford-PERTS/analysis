##############################################################################
##  Engagement Diagnostic Metascript
##  Dave Paunesku & Sarah Gripshover, January 2017
##
##  This metascript loads and processes engagement diagnostic data
##  and calls the engagement diagnostic Rmd on each reporting unit.
##
##
##############################################################################


# For simplicity, transparency, and ease, structure the
# engagement diagnostic with drivers (e.g., growth mindset) and single-shot,
# face-valid questions under them (e.g., "my teachers helped me see how my
# classwork could help me grow"; "my classwork helps me grow smarter.")
#
# Then graphs could say, "Students who agree that: My classwork helps me
# get smarter."
#
# # driver
# ## description
# ## introduction
# ## questions
# ### question 1
# ### question 2
# ### question 3
# ## recommendations

##############################################################################
##  Paremeters
REPORT_DATE <- "2017-12-18" # this is the Monday when the reports are due
# if the reports have data for the week of "2017-09-11", then the REPORT_DATE
# should be "2017-09-18"
#AUTO_DOWNLOAD   <- FALSE      # downloads the most recent qualtrics data and over-
# writes the current qulatrics data file in the crytp folder. As a precaution, it
# renames the previos file with an _old suffix. CURRENTLY NOT WORKING
SINGLE_RUN      <- FALSE     # run just one report (otherwise all)
PDF             <- FALSE    # generate PDF output reports? (can be slow)
PRODUCTION_QUALITY <- FALSE # if TRUE it generates a production quality pdf,
# otherwise it generates a test version of the pdfs
ANONYMOUS <- FALSE # if true, the report name will be anonymous.html, and the
# the team and report unit ids will be fake
TEAM_ONLY <- FALSE # if true, creates reports for the teams, ignoring
# report_unitis
EXCLUDE_TEAMS <- TRUE # if true, it removes the teams descirbed in exclude_teams
# in the engagement_helpers.R file
RESTRICT_TO_ONE_TEAM <- FALSE # if true, cuts the data to "Hillcrest" team
RESTRICT_TO_ONE_RU <- FALSE # if true, cuts the data to reporting unit "Classroom_O7MEUBhsGAVnwhxm"

# disaggregation groups
# collapse together these disaggregation groups if needed for cell size
SUBSETS <- c("gender", "race_cat", "ELL_status")

max_weeks_missing <- 7
# this controls what is the longest strike of missing values which we will accept
# before removing the participant. Any participants having missing data on all metrics
# for longer than the parameter will be removed from the dataset.

survey_file_names <- list(
  "engagement" = "Engagement_Project_OFFICIAL_1.csv" # if you change the name, chnage it in also qualtrics_api.R
)
items_file_name <- list(
  i = "items_new_data_v6.csv"
)

##############################################################################
## Load Libraries and Paths

options(stringsAsFactors = FALSE)
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"

library(lubridate)
library(knitr)
library(markdown)
library(rmarkdown)
library(purrr)
library(tidyr)

tryCatch({
  source("~/Sites/gymnast/R/util.R")
  source("~/Sites/gymnast/R/util_data_summaries.R")
  source("~/Sites/gymnast/R/util_qualtrics_cleaning.R")
  source("~/Sites/gymnast/R/util_graphing.R")
  source("~/Sites/gymnast/R/util_scale_computation.R")
}, error = function(e){
  source(github_base_path %+% "R/util.R")
  source(github_base_path %+% "R/util_data_summaries.R")
  source(github_base_path %+% "R/util_qualtrics_cleaning.R")
  source(github_base_path %+% "R/util_graphing.R")
  source(github_base_path %+% "R/util_scale_computation.R")
})
RMD_BASE_PATH       <- "~/Sites/perts_analyses/engagement_diagnostic/"
REPORT_BASE_PATH    <- paste0(RMD_BASE_PATH, "reports/")
PDF_GENERATOR_PATH  <- "~/Sites/pdf-generator/"
SQL_CONNECT_FILE <- paste0(RMD_BASE_PATH, "sql_connect.R")
QLTR_API_FILE <- paste0(RMD_BASE_PATH, "qualtrics_api.R")

#find crypt and repository folders
home_dirs <- util.list_dirs('~')
REPOSITORY_FOLDER <- home_dirs[grep("engagement_diagnostic$", home_dirs)]
image_files <- list(teacher_caring = "teacher_caring.png",
                    feedback_for_growth = "feedback_for_growth.png",
                    meaningful_work = "meaningful_work.png")

image_paths<- util.find_crypt_paths(image_files)

if(!dir.exists(RMD_BASE_PATH %+% "reports")){ dir.create(RMD_BASE_PATH %+% "reports")}


# set working directory to repo
setwd(RMD_BASE_PATH)

# load helper functions and text
helper_path         <- RMD_BASE_PATH %+% "engagement_helpers.R"

source(helper_path)

source(SQL_CONNECT_FILE) # this runs the script for downlowading information from

# check if Monday
if(!is_Monday(REPORT_DATE)) {
  stop("The REPORT_DATE is not Monday!")
}
# Triton
##############################################################################
##  Load Data
##

# download fresh data from the qulatrics API
#if (AUTO_DOWNLOAD) {
#  source(QLTR_API_FILE)
#}



# find the survey file names and the item file names in the
# crypts. Then, read the files, clean the Qualtrics data,
# and rbind to a single data and items file.

item_path <- RMD_BASE_PATH %+% items_file_name
items <- read.csv(item_path)

survey_paths <- util.find_crypt_paths(survey_file_names)
data_list <- survey_paths %>%
  util.read_csv_files() %>%
  lapply(., function(df) repair_2nd_row(df)) %>% # repair the problem with pdds
  lapply(., function(df) qc.clean_qualtrics(df))

# if there's more than one survey file, rbind them
# together, keeping only the columns they share in common.
# otherwise, data is just the first element in the list.
if(length(data_list) > 1){
  data <- util.rbind_union(data_list)
} else{
  data <- data_list[[1]]
}

# delete records for whom we have no learning conditions information
data <- data[!util.is_blank(data$learning_conditions),]


# add information from Triton
# clean white spaces and non-alphanumeric characters first
triton_tbl[ ,c("team_id", "reporting_unit_id")] <-
  triton_tbl[ ,c("team_id", "reporting_unit_id")]  %>%
  lapply(., util.trim) %>% as.data.frame()
#lapply(., function(x) gsub("&", "and", x)) %>% as.data.frame()
# it seems that white spaces break the script
# @todo you might also consider adding non-ascii checks for names and titles

data <- merge(data, triton_tbl, by = "code", all.x = T, all.y = F)

# If data-restriction flags have been set, cut the data down appropriately and sanity-check
if(RESTRICT_TO_ONE_TEAM) {
  data <- data[data$team_id %in% "Hillcrest", ]
}
if(RESTRICT_TO_ONE_RU) {
  data <- data[data$classroom_id_triton %in% "Classroom_O7MEUBhsGAVnwhxm", ]
}
if(nrow(data) == 0) {
  stop("Stopping script - no data exist after restricting to one team/RU.")
}

#remove clasrooms which are not in Triton. A user can delete a classroom, yet the old
# data will still be in Qulatrics. We want to exclude Qulatrics data from deleted classrooms.
# I could also do this at the merge level, but this way is easier to check what is going on
data <- data[!is.na(data$classroom_id_triton),]



# Temporarily add study ID info and some items info
data$Study_ID <- "Study 1"
# names(items)[names(items) %in% "lausd_general_category"] <- "driver"
# items$driver[items$driver %in% "Belonging"] <- "belonging"
# items$driver[items$driver %in% "Relevance"] <- "relevance"
# items$driver[items$driver %in% c("GMS", "growth_mindset")] <- "growth_mindset_(general)"

# save a copy of the original data for debugging
data_orig <- data

##############################################################################
##  Fill in missing demographic information
##  Link over time by userID so that students don't have to keep filling it out
demographic_items <- items$question_code[items$demographics]
data <- data %>% rename(userID = participant_id)
user_demographics <- data[c("userID", demographic_items)] %>%
  unique

# get rid of all-blank demographic rows (don't want to propagate these values!)
user_demographics$all_blank <- apply(
  user_demographics[demographic_items],
  1,
  function(x) all(util.is_blank(x))
)
user_demographics <- user_demographics[!user_demographics$all_blank, ] %>%
  select(-all_blank)

# sometimes students will fill in inconsistent answers to demographic
# questions across surveys. When this happens, remove demographic info from data.
dup_demographics <- user_demographics$userID[duplicated(user_demographics$userID)] %>%
  unique
user_demographics <- user_demographics[!user_demographics$userID %in% dup_demographics, ]

# now, user_demographics contains authoritative demographic information about demographics
# for every user. So, we'll go ahead and merge it into the master dataset,
# and retain the user_demographics values as authoritative over whatever is there now
# be it NA or not

data_wdem <- merge(
  data,
  user_demographics,
  by = "userID",
  all = TRUE,
  suffixes = c("_origXXX", "")
) %>%
  select(-matches("_origXXX"))


# make sure the merge didn't drop any rows! (it shouldn't, but you never know.)
if(!nrow(data) == nrow(data_wdem)){
  stop("propagating demographic data resulted in added or dropped rows!")
}

data <- data_wdem
rm(data_wdem)

##############################################################################
##  Chunk Dates to Track Changes Over Time
##  Survey responses are grouped by day for tracking purposes

qualtrics_date_format <- "%Y-%m-%d %H:%M:%S"
stripped_time <- strptime( data$StartDate , qualtrics_date_format )
data$week_start <- floor_date(stripped_time, unit = "week") %>% as.Date()
data$day_start <- floor_date(stripped_time, unit = "week") %>% as.Date()


##############################################################################
##  Privacy & Disaggregation
##  Define fields used for disaggregation and how to group values if min_cell
##  size is not met.

min_cell <- 5   # no disaggregation if resulting cells would be smaller


##############################################################################
##  Identify the present_metrics
##  The present_metrics are the questions that will be reported on.
##  To qualify, data must be present and corresponding driver_desc must exist.
##  A second check will be run at each subsetting level to ensure that
##  reporting would not violate min_cell.

all_drivers  <- items$driver %>% unique
all_drivers <- all_drivers[!util.is_blank(all_drivers)]
all_metrics  <- items$question_code[!items$demographics] %>% unique
data_all_metrics <- data[,names(data) %in% all_metrics]

n_answers_by_column <- util.apply_columns( data_all_metrics, function(col){
  sum( !is.na(col) )
})

# calculate all present metrics
present_metrics <- names(n_answers_by_column)[n_answers_by_column > min_cell]
# drop metrics that are actually subsetting fields (demographics)
present_metrics <- present_metrics[present_metrics %in% all_metrics]

# drop drivers that don't appear in driver_desc, and warn
undescribed_drivers <- all_drivers[!all_drivers %in% names(driver_desc)]
if(length(undescribed_drivers) > 0){
  all_drivers <- setdiff(all_drivers, undescribed_drivers)
  util.warn("The following drivers do not match drivers in " %+%
              "driver_desc (an object sourced in from " %+%
              paste0(undescribed_drivers, collapse = ", ") %+%
              ". These drivers will NOT appear in the reports, " %+%
              "unless you either (1) add a description to driver_desc, " %+%
              "or (2) change the driver label in the items .csv " %+%
              "to match one of the existing drivers in driver_desc.")
}

##############################################################################
## Handle duplicate & blank users
## Whenever users are duplicated within week/reporting unit, retain the LAST response



data_dups_handled <- data %>%
  filter(!util.is_blank(userID)) %>%
  group_by(userID, reporting_unit_id, week_start) %>%
  arrange(StartDate) %>%
  mutate(
    id_instance = 1:length(userID),
    id_instances = n()
  ) %>%
  filter(id_instances == 1 | id_instance == id_instances) %>%
  as.data.frame


##############################################################################
##  Convert Categorical Survey Data to Strings

data_cols_not_in_items <- c()
data_cat <- data_dups_handled # data_cat - categorical data

# @SG note: this checks whether each variable is categorical
# (per its designation in the `items` file).
# If an item is labeled as "categorical" in `items`, then
# the item_options field in `items` is used to recode the
# values in the data. In `items`, the recoded values are
# contained in a single cell and delimited with a ';'.

for(col in names(data_dups_handled)){
  item_index <- which(items$question_code %in% col)
  if(length(item_index) > 1){
    util.warn("Too many matches for " %+% col)
  } else if(length(item_index) == 0){
    data_cols_not_in_items <- c(data_cols_not_in_items, col)
  } else if(items[item_index,]$categorical){
    item_options <- strsplit(items[item_index,]$response_options,";")[[1]]
    data_cat[,col] <- as.character(data_cat[,col])
    data_cat[,col] <-
      util.recode(data_cat[,col], 1:length(item_options), item_options)
  }
}

data_cat <- util.apply_columns(data_cat, util.trim) %>%
  util.apply_columns(util.as_numeric_if_number)


##############################################################################
##  Create Super-Sets of Disaggregation Groups
##  To prevent small groups from killing displays

# race: make a new "race_cat" broken up into advantaged vs. not
advantaged_races <- c("White","Asian or Asian American")
disadvantaged_races <- c("Black or African American",
                         "Hispanic or Latino")

data_cat$race_cat <- "Blk, Lat, & Nat"
data_cat$race_cat[data_cat$race_1 %in% advantaged_races] <- "White & Asian"

# gender: make everyone who is not male female
# @to-do generalize the labeling of gender and race variables!
data_cat$gender <- data_cat$gender_1
data_cat$gender[! data_cat$gender %in% c("Male") & !is.na(data_cat$gender)] <- "Female"

# Language: compute ELL_status
# If a student chooses "Another language" in ell_1 and is bellow 4 on ell_2, it is considered an ELL
data_cat$ELL_status <- NA
data_cat$ELL_status <- ifelse(data_cat$ell1_1 == "English", "Eng. Speaker", data_cat$ELL_status )
#table(data_cat$ELL_status, data_cat$ell1_1, exclude = NULL)
data_cat$ELL_status <- ifelse(data_cat$ell1_1 == "Another language" & data_cat$ell2_1 > 3 , "Eng. Speaker", data_cat$ELL_status )
data_cat$ELL_status <- ifelse(data_cat$ell1_1 == "Another language" & data_cat$ell2_1 <= 3 , "Eng. Learner", data_cat$ELL_status )
#table(data_cat$ELL_status, data_cat$ell1_1, exclude = NULL)
#table(data_cat$ELL_status, data_cat$ell2_1, exclude = NULL)

##############################################################################
##  Subdivide Reports
##  Break out all combinations of team and study
##  make a separate report for each team x study x reporting unit

team_study_df <- data_cat[,c("team_id","Study_ID")]
team_study_df <- unique(team_study_df)
team_study_reporting_unit_df_list <- list() # contains dfs of all Team x Study x Reporting Unit

# all observations must have a reporting unit parameter
data_cat$reporting_unit_id[
  util.is_blank(data_cat$reporting_unit_id)
  ] <- "Not reporting"


# SG note: For each Team/Study combo, pull out the reporting units that were
# included. Generates `team_study_reporting_unit_df_list`, which is a list
# of data.frames with team_id, Study_ID, and all unique reporting units
# found for that team_id/Study_ID combination in the data.
for( i in 1:nrow(team_study_df) ){
  # what reporting_units were included in the Team x Study?
  reporting_units <- data_cat$reporting_unit_id[
    # team column row i
    data_cat$team_id %in% team_study_df$team_id[i] &
      # study column row i
      data_cat$Study_ID %in% team_study_df$Study_ID[i]
    ]
  unique_reporting_units <- strsplit(reporting_units,",") %>% unlist %>% unique
  team_study_reporting_unit_df_list[[i]] <- expand.grid(
    Team_ID=team_study_df$team_id[i],
    Study_ID=team_study_df$Study_ID[i],
    reporting_unit=unique_reporting_units,
    stringsAsFactors = FALSE
  )
}

# Rbind them into a single data.frame with one row per Team/study/reporting unit
# combination present in the data
if(length(team_study_reporting_unit_df_list) == 1){
  team_study_reporting_unit_df <- team_study_reporting_unit_df_list[[1]]
} else{
  team_study_reporting_unit_df <- util.rbind_many(team_study_reporting_unit_df_list)
}

team_study_reporting_unit_df <- team_study_reporting_unit_df[!util.is_blank(team_study_reporting_unit_df$reporting_unit),]
# remove demo and test teams
if (EXCLUDE_TEAMS) {
  team_study_reporting_unit_df <-
    team_study_reporting_unit_df[
      !team_study_reporting_unit_df$Team_ID %in% excluded_teams,]
}

if( SINGLE_RUN ){
  # just run the report on the first reporting unit
  team_study_reporting_unit_df <- team_study_reporting_unit_df[1,]

  # used for targeting subsets of interest
  target_subset_feature <- ""  # e.g., race
  target_subset_level   <- ""  # e.g., "Latino"
}

#### DATA IMPUTATION #############
data_cat_not_imputed <- data_cat #keep it for printing summary table


# If the script is not in debug-mode or otherwise restricting the data set,
# then save a copy of the non-imputed data.
if(!RESTRICT_TO_ONE_RU & !RESTRICT_TO_ONE_TEAM) {
  full_file_path <- util.find_crypt_paths(list(unimputed =
                                                 'DEPRECATED v0.1 EP metascript processed data/EP metascript processed data unimputed.csv'))
  directory <- sub("/EP metascript processed data unimputed.csv", "", full_file_path[[1]])
  setwd(directory)
  write.csv(data_cat_not_imputed, "EP metascript processed data unimputed.csv")
}


#### Impute missing values

# We will impute missing values, as far as the imputation sequence is not longer
# than the parameter max_weeks_missing (e.g. no more than 5 weeks)

# at the end the data frame will  have the follwing additional columns
# ..._imp is a group of columns which has imputed values
# ..._imp_log is a group of columns which contains information if a corresponding
# column has been imputed
# imputed_row is a single column, indicating if the whole row was imputed. This could
# happen if a participant does not have a record for a particular week.
# Note that we will also keep the original columns, which will be used for the
# summary table.

# The imputation will have two main parts. In the first one, we will insert
# missing rows for those who did not participate a particular week. In the
# second part, we will use the last non-missing value, to replace missing values
####


##### COMMENTS ON THIS IMPUTATION METHOD - 12/20/17

# This method has two primary bugs: It leaves NA rows that should be cut, and it mislabels some rows as imputed.

# The core problem was this set of lines:
#
#   # for records with missing data, replace time lag with the maximum distance
#   d_missing_users_removed$time_lag[!d_missing_users_removed$at_least_one_metric] <-
#   time_span
#
# This code takes every real survey response that is "blank" - i.e. the student logged in but never answered
# any questions - and marks it as having the maximum possible "time lag", i.e. it is the maximum possible number
# of weeks away from the present.
#
# The intention was to do this so that these lines would get filtered out later, when we cut all the students
# whose most recent data is older than max_weeks_missing.
#
# The problem is that the "time lag" trick marks individual rows, but later code filters STUDENTS,
# and students have multiple rows. So the code often fails to remove the blank rows.
# The ideal behavior is to simply cut blank rows before doing any time-lag stuff.
# For example, imagine that there have been 10 weeks of school, and max-weeks-missing is 7.
# You have a student with two rows of real data - a blank row (they logged in and logged out)
# and a real row that is 3 weeks old. The correct behavior is to just cut the blank row,
# but this code marks the blank row as 10 weeks old and then cuts all students whose most recent data
# is less than 7 weeks old. But in this case, their most recent data is 3 weeks old, so it never cuts the blank row.
#
# Two things can happen when the blank row hangs around:
# If it came before any non-blank real data, then it never gets imputed and just stays as a ghost row of NAs.
# (Example: Participant_5T7m1lnu7gBt97P4)
#
# If it came after any non-blank real data, then it gets imputed, but it is incorrectly considered as NON-imputed.
# (Example: Participant_8GqaiO8eUHJ2YnmY)





col_names_to_impute <- present_metrics[present_metrics %in% colnames(data_cat)]
# present_metrics comes from the metascript

# remove participants who have missing values on all metrics for more than
# max_weeks_missing weeks
d_missing_users_removed <- data_cat

d_missing_users_removed$comb_id <-
  paste(d_missing_users_removed$Study_ID,
        d_missing_users_removed$userID,
        d_missing_users_removed$team_id,
        d_missing_users_removed$reporting_unit_id,
        sep = "_")


# In the next part I will measure the distance to the date of the last non-missing value

#d_missing_users_removed$week_start_ord <- d_missing_users_removed$week_start %>%
#  compute_ordinal() %>% as.numeric

# compute ordinal week, starting from the first available data point. If we have
# data from the first and the fourth week of a given month, the result will be c(1,4) not c(1,2)
d_missing_users_removed$week_start_ord  <- difftime(
  as.Date(d_missing_users_removed$week_start),
  min(as.Date(d_missing_users_removed$week_start)),
  units = "weeks"
) %>% as.numeric()
# add one, so there the first week will be 1
d_missing_users_removed$week_start_ord  <- d_missing_users_removed$week_start_ord + 1


most_current_week <- d_missing_users_removed$week_start_ord %>% max
oldest_week <- d_missing_users_removed$week_start_ord %>% min
time_span <- most_current_week - oldest_week

d_missing_users_removed$at_least_one_metric <-
  apply(d_missing_users_removed[, col_names_to_impute], 1, any_non_na)

# compute time difference between each row and most recent week
d_missing_users_removed$time_lag <-
  most_current_week - d_missing_users_removed$week_start_ord
# for records with missing data, replace time lag with the maximum distance
d_missing_users_removed$time_lag[!d_missing_users_removed$at_least_one_metric] <-
  time_span

# find users who don't have recent enough records
users_recency_df <- d_missing_users_removed %>%
  group_by(comb_id) %>%
  summarise(most_recent = min(time_lag))

#remove users who do not have recent enough records
users_to_remove <- users_recency_df$comb_id[users_recency_df$most_recent > max_weeks_missing]

d_missing_users_removed <- d_missing_users_removed[
  !(d_missing_users_removed$comb_id %in% users_to_remove),]


# remove unnecssary variables
users_recency_df <- NULL
d_missing_users_removed <- d_missing_users_removed[
  !names(d_missing_users_removed) %in% c(
    "comb_id",
    "at_least_one_metric",
    "time_lag",
    "week_start_ord"
  )
  ]
most_current_week <-NULL
oldest_week <-NULL
time_span <- NULL

### Impute missing data
# For any users who are missing data during a given week,
# we will impute a value for them by carrying forward their
# observation on the same question in the most recent week that
# they answered it.

d_not_imputed <- d_missing_users_removed

# add rows with missing values, so they can be filled in
d_to_impute <- d_not_imputed
d_to_impute$comb_id <-
  paste(
    d_to_impute$Study_ID,
    d_to_impute$team_id,
    d_to_impute$reporting_unit_id,
    d_to_impute$userID,
    sep = "@"
  )

# record which is the first week on record
first_week_df <- d_to_impute %>%
  group_by(comb_id) %>%
  summarise(
    first_week_in_record = min(week_start)
  )
d_to_impute <- merge(
  d_to_impute,
  first_week_df,
  by = "comb_id",
  all.x = TRUE,
  all.y = FALSE
)

d_to_impute$comb_time <- d_to_impute$week_start
# remove duplicates within the same week (keep the last record)
d_to_impute <- d_to_impute %>% arrange(desc(EndDate))
d_to_impute <- d_to_impute[!duplicated(d_to_impute[,c("comb_id", "comb_time")]),]



unique_comb_id <- d_to_impute$comb_id %>% unique
unique_comb_time <- d_to_impute$comb_time %>% unique
# for unique time, insert weeks that are missing
unique_comb_time <- unique_comb_time %>% insert_missing_weeks()


# create df with all combinations of id and time
time_vec <- c()
id_vec <- c()
for (id_comb in unique_comb_id) {
  for (time_comb in unique_comb_time) {
    time_vec <- c(time_vec, time_comb)
    id_vec <- c(id_vec, id_comb)
  }
}
df_all_id_and_time <- data.frame(comb_id = id_vec, comb_time = time_vec)

d_to_impute$orig_row <- TRUE

d_to_impute <- merge(
  d_to_impute,
  df_all_id_and_time,
  by = c("comb_id", "comb_time"),
  all = TRUE
)
if (nrow(df_all_id_and_time) != nrow(d_to_impute)) {
  stop("There was a problem with the imputation procedure. The combination data frame has different length than expected.")
}

d_to_impute$orig_row[is.na(d_to_impute$orig_row)] <- FALSE

# for missing time period for each subject, we will copy the row from above
# note, that in some cases the row above will be blank too (all NAs)
d_to_impute <- d_to_impute %>% arrange(comb_id, comb_time)
d_to_impute$imputed_row <- FALSE
for (row_ind in 2:nrow(d_to_impute)) {
  str_split <- strsplit(d_to_impute$comb_id[row_ind],"@") %>%
    unlist %>%
    unname
  if (is.na(d_to_impute$userID[row_ind])) {
    d_to_impute$team_id[row_ind] <- str_split[1]
    d_to_impute$reporting_unit_id[row_ind] <- str_split[2]
    d_to_impute$userID[row_ind] <- str_split[3]
  }
  if (is.na(d_to_impute$week_start[row_ind])) {
    if (d_to_impute$comb_id[row_ind] == d_to_impute$comb_id[row_ind-1]) {
      non_missing_values <- !is.na(d_to_impute[row_ind-1,])
      current_comb_time <- d_to_impute$comb_time[row_ind]
      d_to_impute[row_ind, non_missing_values] <- d_to_impute[row_ind-1,non_missing_values]
      d_to_impute$comb_time[row_ind] <- current_comb_time
      d_to_impute$week_start[row_ind] <- d_to_impute$comb_time[row_ind]
      d_to_impute$imputed_row[row_ind] <- TRUE
    }
  }
}

# remove those who still have NAs as week starts (these are pre-imputation rows)
# the abscence of ResponseID indicates that the missing values come form inserting
# empty row, and no qualtrics record has been associated with that row
d_to_impute <- d_to_impute[!is.na(d_to_impute$ResponseID),]

d_to_impute$imputed_row <- d_to_impute$imputed_row %>% as.numeric()

# check for duplicates
test <- duplicated(
  d_to_impute[,c("Study_ID","week_start", "userID", "team_id", "reporting_unit_id")]
) %>% sum
if (test > 0) {
  stop("The row imputation procedure resulted in duplciated records")
}


# drop garbage
first_week_df <- NULL
d_to_impute$orig_row <- NULL
d_to_impute$comb_id <- NULL
d_to_impute$comb_time <- NULL
df_all_id_and_time <- NULL


# The function impute_values only subsitutes  missing values in otherwise existing rows.
# It does not add new rows.
d_imputed <- impute_values(
  input_df = d_to_impute,
  id_cols = c("team_id", "reporting_unit_id", "userID"),
  time_col = "EndDate",
  columns_to_impute = col_names_to_impute
) %>% as.data.frame()

# remove original metrics, and replace with the imputed variables
imputed_col_names <- paste(col_names_to_impute, "_imp", sep ="")
d_imputed[,col_names_to_impute] <- d_imputed[,imputed_col_names]
d_imputed <- d_imputed[!names(d_imputed) %in% imputed_col_names]

data_cat <- d_imputed #note that this name of a data frame was used earlier
# in the script too. Here we just diverged to run the imputation, and now
# we are going back to the previous data structure, which now has the imputed values
# instead of the original values


# If the script is not in debug-mode or otherwise restricting the data set,
# then save a copy of the imputed data.
if(!RESTRICT_TO_ONE_RU & !RESTRICT_TO_ONE_TEAM) {
  full_file_path <- util.find_crypt_paths(list(imputed =
                                                 'DEPRECATED v0.1 EP metascript processed data/EP metascript processed data imputed.csv'))
  directory <- sub("/EP metascript processed data imputed.csv", "", full_file_path[[1]])
  setwd(directory)
  write.csv(d_imputed, "EP metascript processed data imputed.csv")
}




############ BUG FIX ##############
# create different team_study_reporting_unit_df based on triton_tbl
# it seems that we can have different classrooms using the same name within a team
# this currently breaks the script, so I will switch to different looping structure
# @todo clean previous team_study_reporting_unit_df
team_study_reporting_unit_df <- triton_tbl[,
                                           c("team_id",
                                             "reporting_unit_id",
                                             "code",
                                             "classroom_id_triton"
                                           )]
team_study_reporting_unit_df$Study_ID <- "Study 1"
team_study_reporting_unit_df <- team_study_reporting_unit_df %>%
  rename(
    Team_ID = team_id,
    reporting_unit = reporting_unit_id,
    triton_id = classroom_id_triton
  )
# exclude teams, if needed
if (EXCLUDE_TEAMS) {
  team_study_reporting_unit_df <-
    team_study_reporting_unit_df[
      !team_study_reporting_unit_df$Team_ID %in% excluded_teams,]
}

# exclude teams for which we do not have any data
unique_codes <- data_cat$code %>% unique
team_study_reporting_unit_df  <- team_study_reporting_unit_df[team_study_reporting_unit_df$code %in% unique_codes,]
############# END BUG FIX ###########


#create log files to track what is happening during rendering
basic_log <- team_study_reporting_unit_df[, c("Team_ID", "reporting_unit", "Study_ID", "code", "triton_id")]
basic_log <- basic_log %>%
  rename(
    team_id = Team_ID,
    reporting_unit_id = reporting_unit,
    study_id = Study_ID
  )
basic_log$`Eng. Speaker` <- NA
basic_log$`Eng. Learner` <- NA
basic_log$`White & Asian` <- NA
basic_log$`Blk, Lat, & Nat` <- NA
basic_log$Male <- NA
basic_log$Female <- NA
basic_log$`All Students` <- NA
basic_log$file_name <- NA
basic_log$file_present <- FALSE
basic_log$error_msg <- NA # currently not working
basic_log$team_only <- NA
basic_log$most_recent_week_n <- NA

detailed_log <- data.frame()


##############################################################################
#   Render Engagement Diagnostics
#

# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}

if (TEAM_ONLY) {
  team_study_reporting_unit_df$reporting_unit <- "TEAM"
  team_study_reporting_unit_df <- team_study_reporting_unit_df[!duplicated(team_study_reporting_unit_df),]
}


orig_TEAM_ONLY <- TEAM_ONLY # I have to do this because I'm switching the values in
# the Rmd files
#team_study_reporting_unit_df <- team_study_reporting_unit_df[team_study_reporting_unit_df$Team_ID == "Equity Leader Lab",]
for(i in 1:nrow(team_study_reporting_unit_df)) {
  cat("Starting classroom: ", i)
  report_name <- NA
  TEAM_ONLY <- orig_TEAM_ONLY

  team_id             <- team_study_reporting_unit_df[i,"Team_ID"]
  study_id            <- team_study_reporting_unit_df[i,"Study_ID"]
  reporting_unit_id    <- team_study_reporting_unit_df[i,"reporting_unit"]
  code <- team_study_reporting_unit_df[i,"code"]

  reporting_unit_id_triton <-
    triton_tbl$classroom_id_triton[
      triton_tbl$code == code]

  team_id_triton <-   triton_tbl$team_id_triton[
    triton_tbl$team_id == team_id] %>% unique

  # classroom was not found, replace with NONE
  if(length(reporting_unit_id_triton) == 0 )  reporting_unit_id_triton <- "NONE" %+% (sample(1:10000,1) %>% as.character)
  if(util.is_blank(reporting_unit_id_triton))  reporting_unit_id_triton <- "NONE" %+% (sample(1:10000,1) %>% as.character)
  if(reporting_unit_id_triton == "TEAM")  reporting_unit_id_triton <- "TEAM" %+% (sample(1:10000,1) %>% as.character)
  team_id_triton <-
    triton_tbl$team_id_triton[
      triton_tbl$team_id == team_id] %>% unique

  rmd_path     <- RMD_BASE_PATH %+% "engagement_diagnostic.Rmd"
  report_name  <- reporting_unit_id_triton %+% "." %+% REPORT_DATE %+% ".html" %>%
    gsub(" ", "", .)

  if (TEAM_ONLY) {
    report_name  <- team_id %+% "." %+% REPORT_DATE %+% ".html" %>%
      gsub(" ", "", .)
  }

  if (ANONYMOUS) {
    report_name = "anonymous.html"
  }
  report_base_path <- REPORT_BASE_PATH
  report_path   <- report_base_path %+%
    report_name

  paste0("Running ",report_name ) %>%
    message
  error_msg <- NA
  # this step renders the .Rmd file. Note that the .Rmd file
  # is not configured to be renderable on its own, and relies on
  # variables from the metascript global namespace. The main
  # input to the .Rmd file is the object `data_cat` which is
  # the Qualtrics data that's had categorical variables recoded.
  basic_log$file_name[basic_log$triton_id == reporting_unit_id_triton] <- report_name
  possible_error <-
    tryCatch(
      render(rmd_path,
             "html_document",
             report_path ),
      error = function(e){
        paste0("Error in ", report_name, ": ", e) %>%
          message
        return(e)
      }
    )
  # convert the HTMLS from the previous step to PDFs
  # I will have to change the workind directory, so I will keep a copy of the
  # original path

  orig_wd <- getwd()
  if(PDF){
    tryCatch({
      #set a new working directory so you can reach the .py script
      setwd(PDF_GENERATOR_PATH)
      #clean inbox from existing files with the same name
      file_name_basic <- gsub("\\.html", "", report_name)

      #account for possible white spaces in the file name
      file_name_basic <- gsub(" ", "\\\\ ",file_name_basic)
      file_path_in <- paste0(PDF_GENERATOR_PATH, "inbox/",file_name_basic, ".html")
      if (file.exists(file_path_in)){
        paste0("rm ", file_path_in) %>% system()
      }
      #clean outbox from existing files with the same name
      file_path_out <- paste0(PDF_GENERATOR_PATH, "outbox/",file_name_basic, ".pdf")
      if (file.exists(file_path_out)){
        paste0("rm ", file_path_out) %>% system()
      }

      #copy html output to inbox
      report_path_escaped <- gsub(" ", "\\\\ ",report_path)
      paste0("cp ", report_path_escaped, " ", PDF_GENERATOR_PATH %+% "inbox") %>%
        system()

      if (PRODUCTION_QUALITY) {
        paste0("/usr/local/bin/python ",
               PDF_GENERATOR_PATH,
               "generate.py ",
               "--production") %>%
          system()
        report_output_path <- "~/Sites/report/switchboard/"
      } else {
        paste0("/usr/local/bin/python ",
               PDF_GENERATOR_PATH,
               "generate.py ") %>%
          system()
        report_output_path <- report_base_path
      }

      #copy pdf from outbox to /reports
      paste0("cp ", file_path_out, " ", report_output_path) %>%
        system()
      #clean outbox
      paste0("rm ", file_path_out) %>% system()
      # clean inbox
      paste0("rm ", file_path_in)  %>% system()

    },
    error = function(e){
      paste0("Error during PDF conversion.", e) %>%
        message

    }
    )
    #return to the original working directory
    setwd(orig_wd)
  }

}


# turn off logging
closeAllConnections()

# Set back to regular interactive mode settings
interactive <- base::interactive


# Check if files are properly saved

for (i in 1:nrow(basic_log)){
  f_name =  RMD_BASE_PATH %+% 'reports/'  %+% basic_log$file_name[i]
  if(file.exists(f_name)) {
    basic_log$file_present[i] <- TRUE
    print(i)
  }
}


# total code mentions in the data file
code_counts <- data_cat %>% group_by(code) %>%
  summarise (total_code_mentions = n())


basic_log <- basic_log %>% merge(., code_counts, by = "code", all.x = T, all.y = F)

code_counts <- data %>% group_by(code) %>%
  summarise (total_code_mentions_raw = n())
basic_log <- basic_log %>% merge(., code_counts, by = "code", all.x = T, all.y = F)



#basic_log <- basic_log[!duplicated(basic_log),]
detailed_log <- detailed_log[!duplicated(detailed_log),]
write.csv(basic_log, RMD_BASE_PATH %+% "basic_log.csv", row.names = FALSE)
write.csv(detailed_log, RMD_BASE_PATH %+% "detailed_log.csv", row.names = FALSE)
#@todo
# remove the pdd correction (it works fine with, but there is no need any more)

