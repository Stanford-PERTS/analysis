##############################################################################
##  Engagement Diagnostic Metascript
##  Dave Paunesku & Sarah Gripshover, January 2017
##
##  This metascript loads and processes engagement diagnostic data
##  and calls the engagement diagnostic Rmd on each reporting unit.
##
##
##############################################################################

#Run this with modifications on line 219

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


############### SET CONTROLLABLE PARAMETERS #################################

REPORT_DATE <- "2018-06-04" # this is the Monday when the reports are due


# if the reports have data for the week of "2017-09-11", then the REPORT_DATE
# should be "2017-09-18"
#AUTO_DOWNLOAD   <- FALSE      # downloads the most recent qualtrics data and over-
# writes the current qualtrics data file in the crypt folder. As a precaution, it
# renames the previous file with an _old suffix. CURRENTLY NOT WORKING
SINGLE_RUN      <- TRUE     # run just one report (otherwise all)

# This section is for restricting the dataset for testing purposes.
# The RESTRICT_DATA_FOR_TESTING flag prevents downstream datasets from being saved.
RESTRICT_DATA_FOR_TESTING <- TRUE

PDF             <- FALSE    # generate PDF output reports? (can be slow)
PRODUCTION_QUALITY <- FALSE # if TRUE it generates a production quality pdf,
# otherwise it generates a test version of the pdfs
ANONYMOUS <- FALSE # if true, the report name will be anonymous.html, and the
# the team and report unit ids will be fake
TEAM_ONLY <- FALSE # if true, creates reports for the teams only, ignoring classrooms
EXCLUDE_TEAMS <- TRUE # if true, it removes the teams described in excluded_team_names
# in the engagement_helpers.R file

TIME_LAG_THRESHOLD <- 9 # Teams who haven't participated for more days than the threshold are excluded
# from the report generating part.
# I choose 9 rather than 7 days because the weekend could be confusing (it could add 2 days)

# disaggregation groups
# collapse together these disaggregation groups if needed for cell size
SUBSET_TYPES <- c("gender", "race_cat", "ELL_status")

# reporting unit id types
REPORTING_UNIT_ID_TYPES <- c("class_id", "team_id")

# open response question IDs being used
active_or_question_ids <- c("or_5", "or_fg1_1", "or_fg2_1", "or_fg3_1", "or_mw1_1", "or_mw2_1",
                            "or_mw3_1","or_tc1_1", "or_tc2_1", "or_tc4_1")

max_weeks_missing <- 70
# this controls what is the longest strike of missing values which we will accept
# before removing the participant. Any participants having missing data on all metrics
# for longer than the parameter will be removed from the dataset.

survey_file_names <- list(
  "engagement" = "Engagement_Project_OFFICIAL_1.csv" # if you change the name, chnage it in also qualtrics_api.R
)
items_file_name <- list(
    i = "items_new_data_v6.csv"
)

# We aggregate data by different "reporting units", such as class or team.
# This var specifies the id variables that we use for aggregation. Currently just class and team.
reporting_unit_id_var_types <- c("class", "team")

############### LOAD LIBRARIES AND PATHS #####################################

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
    source("~/Sites/gymnast/R/util_dfc.R")
}, error = function(e){
    source(github_base_path %+% "R/util.R")
    source(github_base_path %+% "R/util_data_summaries.R")
    source(github_base_path %+% "R/util_qualtrics_cleaning.R")
    source(github_base_path %+% "R/util_graphing.R")
    source(github_base_path %+% "R/util_scale_computation.R")
    source(github_base_path %+% "R/util_dfc.R")
})
RMD_BASE_PATH       <- "~/Sites/analysis/engagement_diagnostic/"
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

report_list_path  <- RMD_BASE_PATH %+% "report_list.R"
source(report_list_path)

source(SQL_CONNECT_FILE) # this runs the script for downloading information from Triton

# check if Monday
if(!is_Monday(REPORT_DATE)) {
  stop("The REPORT_DATE is not Monday!")
}

############### LOAD DATA ####################################


# download fresh data from the qualtrics API
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
triton_tbl[ ,c("team_id", "class_name")] <-
  triton_tbl[ ,c("team_id", "class_name")]  %>%
  lapply(., util.trim) %>% as.data.frame()
  #lapply(., function(x) gsub("&", "and", x)) %>% as.data.frame()
# it seems that white spaces break the script
# @todo you might also consider adding non-ascii checks for names and titles

data <- merge(data, triton_tbl, by = "code", all.x = T, all.y = F)

# add in teacher information:
# data$class_id linked to class_tbl$contact_id via class_tbl$class_id
data <- merge(data,
               class_tbl[, c("class_id", "contact_id")],
               by = "class_id",
               all.x = T,
               all.y = F)
# then linked to user_tbl$name and user_tbl$email via user_tbl$user_id
data <- rename(data, user_id = contact_id)
data <- merge(data,
              user_tbl[, c("email", "name", "user_id")],
              by = "user_id",
              all.x = T,
              all.y = F)
data <- rename(data,
               teacher_id = user_id,
               teacher_name = name,
               teacher_email = email)

if(nrow(data) == 0) {
  stop("Stopping script - no data exist after restricting to one team/class.")
}

#remove classes which are not in Triton. A user can delete a class, yet the old
# data will still be in Qualtrics. We want to exclude Qualtrics. data from deleted classes.
# I could also do this at the merge level, but this way is easier to check what is going on
data <- data[!is.na(data$class_id),]

# Remove responses that have embedded data "testing" set to "true".
# These are teacher survey previews from Copilot.
if(!is_null(data$testing)) {
  data <- data[!data$testing %in% "true", ]
}

# If there is no open-response data, add open-response questions listed in items.csv
# with all NA responses so script can function.
all_possible_open_response_q_names <- items$question_code[grep("^or_", items$question_code)]
for(open_response_q in all_possible_open_response_q_names) {
  if(!open_response_q %in% names(data)) {
    data[, open_response_q] <- NA
  }
}

# Temporarily add study ID info and some items info
data$Study_ID <- "Study 1"
# names(items)[names(items) %in% "lausd_general_category"] <- "driver"
# items$driver[items$driver %in% "Belonging"] <- "belonging"
# items$driver[items$driver %in% "Relevance"] <- "relevance"
# items$driver[items$driver %in% c("GMS", "growth_mindset")] <- "growth_mindset_(general)"



if(RESTRICT_DATA_FOR_TESTING) {
  # data <- data[data$class_id %in% c("Classroom_pDMNO7PV9KErbB9A"), ]
  # data <- data[data$team_name %in% c("Pritzker Jaguars"), ]
  # data <- data[data$team_name %in% c("NYC Feedback for Growth"), ]
   data <- data[data$team_id %in% c("Team_1SLg8wI833my3Qk5"), ]
  # data <- data[data$team_name %in% c("Ms. Johnson's HR"), ]
  # data <- data[data$team_name %in% c("NYC Feedback for Growth"), ]
  # For testing currently-active teams:
  # active_team_ids <- c("Team_hhYhEmpuqyZuTbUw", "Team_mD3pQNTuoEcJ8080",
  #                      "Team_nqsW6sUgatFBFPy0", "Team_RWQd2UKj2gI1p0T9",
  #                      "Team_UNBA0j2Qvl9WZ0Vr", "Team_VEaC0oDNh8vMPXuc",
  #                      "Team_zXEjOmzZWzVPgyvv")
  # data <- data[data$team_id %in% active_team_ids, ]
}


# save a copy of the original data for debugging
data_orig <- data


############### FILL IN MISSING DEMOGRAPHICS ################################
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

############### CHUNK DATES ###################################
##  Survey responses are grouped by day for tracking purposes

qualtrics_date_format <- "%Y-%m-%d %H:%M:%S"
stripped_time <- strptime( data$StartDate , qualtrics_date_format )
data$week_start <- floor_date(stripped_time, unit = "week") %>% as.Date()
data$day_start <- floor_date(stripped_time, unit = "week") %>% as.Date()


############### DEFINE PRIVACY & DISAGGREGATION FIELDS ####################################
##  Define fields used for disaggregation and how to group values if min_cell
##  size is not met.

min_cell <- 5   # no disaggregation if resulting cells would be smaller


############### IDENTIFY THE PRESENT_METRICS #############################
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

############### HANDLE DUPLICATE AND BLANK USERS #############################
## Whenever users are duplicated within team/week/reporting unit, retain the LAST response



data_dups_handled <- data %>%
    filter(!util.is_blank(userID)) %>%
    group_by(userID, class_id, week_start) %>%
    arrange(StartDate) %>%
    mutate(
        id_instance = 1:length(userID),
        id_instances = n()
    ) %>%
    filter(id_instances == 1 | id_instance == id_instances) %>%
    as.data.frame


############### CONVERT CATEGORICAL DATA TO STRINGS #############################

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


############### CREATE SUPERSETS OF DISAGGREGATION GROUPS ###########################
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

############### SUBDIVIDE REPORTS  ###############################
##  Make data frames listing info about teams and classes

team_study_df <- data_cat[,c("team_id", "team_name", "Study_ID")]
team_study_df <- unique(team_study_df)
team_study_class_df_list <- list() # contains dfs of all Team x Study x Class

# all observations must have a class name parameter
data_cat$class_name[
    util.is_blank(data_cat$class_name)
] <- "Not reporting"


# SG note: For each Team/Study combo, pull out the classes that were
# included. Generates `team_study_class_df_list`, which is a list
# of data.frames with team_id, Study_ID, and all unique classes
# found for that team_id/Study_ID combination in the data.
for( i in 1:nrow(team_study_df) ){
    # what classes were included in the Team x Study?
    classes <- data_cat$class_name[
        # team column row i
        data_cat$team_id %in% team_study_df$team_id[i] &
            # study column row i
            data_cat$Study_ID %in% team_study_df$Study_ID[i]
    ]
    unique_classes <- strsplit(classes,",") %>% unlist %>% unique
    team_study_class_df_list[[i]] <- expand.grid(
        team_id=team_study_df$team_id[i],
        team_name=team_study_df$team_name[i],
        Study_ID=team_study_df$Study_ID[i],
        class_name=unique_classes,
        stringsAsFactors = FALSE
    )
}

# Rbind them into a single data.frame with one row per Team/study/class
# combination present in the data
if(length(team_study_class_df_list) == 1){
  team_study_class_df <- team_study_class_df_list[[1]]
} else{
  team_study_class_df <- util.rbind_many(team_study_class_df_list)
}

team_study_class_df <- team_study_class_df[!util.is_blank(team_study_class_df$class_name),]
# remove demo and test teams
if (EXCLUDE_TEAMS) {
  team_study_class_df <-
    team_study_class_df[
      !team_study_class_df$team_name %in% excluded_team_names,]
}

if( SINGLE_RUN ){
    # just run the report on the first class
  team_study_class_df <- team_study_class_df[1,]

    # used for targeting subsets of interest
    target_subset_feature <- ""  # e.g., race
    target_subset_level   <- ""  # e.g., "Latino"
}

data_cat_not_imputed <- data_cat #keep it for printing participation table


############### DATA IMPUTATION #####


# The workflow for data imputation is as follows:

## Save a copy of the pre-imputation data
## Set up important variables and cut nonsense rows (blank rows, same-week duplicates)
## Add all possible user-time rows, and flag them as such
## Remove rows that should not be kept for imputation:
### rows that are before anyone ever started
### entire students who have been missing longer than max_weeks_missing
## impute NAs downward to fill in vars of interest within subjects
## Save a copy of the post-imputation data


########## Save a copy of the pre-imputation data

# If the script is not in debug-mode or otherwise restricting the data set,
# then save a copy of the non-imputed data.
if(!RESTRICT_DATA_FOR_TESTING) {
  full_file_path <- util.find_crypt_paths(list(unimputed =
                                                 'EP metascript processed data/EP metascript processed data unimputed.csv'))
  directory <- sub("/EP metascript processed data unimputed.csv", "", full_file_path[[1]])
  setwd(directory)
  write.csv(data_cat, "EP metascript processed data unimputed.csv")
}


########## Set up important variables and cut duplicates within week

# Choose columns you want to impute
col_names_to_impute <- present_metrics[present_metrics %in% colnames(data_cat)]

# Define combinatorial ID that uniquely identifies students nested within their groups:
# Study, team, class, user
data_cat$comb_id <-
  paste(data_cat$Study_ID,
        data_cat$team_id,
        data_cat$class_id,
        data_cat$userID,
        sep = "@")

# cut nonsense rows: duplicates within the same week (keep the last record)
data_cat <- data_cat %>% arrange(desc(EndDate))
data_cat <- data_cat[!duplicated(data_cat[,c("comb_id", "week_start")]),]

# cut nonsense rows: rows where the student logged in but never answered one LC item
data_cat$at_least_one_metric <-
  apply(data_cat[, col_names_to_impute], 1, any_non_na)
data_cat <- data_cat[data_cat$at_least_one_metric, ]

# record which is the first week on record for each student, and merge that info back in
first_week_df <- data_cat %>%
  group_by(comb_id) %>%
  summarise(first_week_in_record = min(week_start))
data_cat <- merge(
  data_cat,
  first_week_df,
  by = "comb_id",
  all.x = TRUE,
  all.y = FALSE)

# tag the rows here as real originals
data_cat$imputed_row <- FALSE


########## Add all possible user-time rows, and flag them as such

# create df with all combinations of id and POSSIBLE weeks (from first week to last, with no gaps)
unique_comb_ids <- data_cat$comb_id %>% unique
unique_weeks <- data_cat$week_start %>% unique %>% insert_missing_weeks()
df_all_id_and_time <- expand.grid(comb_id = unique_comb_ids,
                                  week_start = unique_weeks) %>%
  util.apply_columns(as.character) %>%
  arrange(comb_id, week_start)

# merge the real data with the all-combinations df to create blank rows for unobserved weeks,
# flag the blank rows as not originals,
# and merge it with first_week_df to add info about the first week of real data.
data_cat_w_blanks <- merge(
  data_cat,
  df_all_id_and_time,
  by = c("comb_id", "week_start"),
  all = TRUE
) %>%
  mutate(imputed_row = ifelse(is.na(imputed_row), TRUE, imputed_row)) %>%
  select(-first_week_in_record) %>%
  merge(.,
        first_week_df,
        by = "comb_id",
        all.x = T,
        all.y = F)

# Fill in missing identity values in the blank weeks using the comb_id
str_split_matrix <- strsplit(data_cat_w_blanks$comb_id, "@") %>%
  unlist %>%
  matrix(ncol = 4, byrow = T)
data_cat_w_blanks[, c("Study_ID", "team_id", "class_id", "userID")] <- str_split_matrix


# clean up
rm(first_week_df, str_split_matrix, df_all_id_and_time)

########## Remove rows that should not be kept for imputation:

#################### Remove rows that are before anyone ever started

# Each student has a first week that they showed up - this is stored in first_week_in_record.
# compare week_start to first week, and only keep rows where week_start is before or during first week.
data_cat_trim_early_non_data <- data_cat_w_blanks[data_cat_w_blanks$first_week_in_record <= data_cat_w_blanks$week_start, ]

#################### Remove entire students who have been missing longer than max_weeks_missing

# Each student is now represented by a block of contiguous ordered rows - one for each possible week
# that they COULD have data from their first real week onward.
# Fake rows are identified by imputed_row == TRUE.
# And they can have a mix of real and fake rows.
# So within each student, if you count upward from the last row until you hit a real row,
# and the number of fake rows is greater than max_weeks_missing,
# then this student has been gone too long and we drop them entirely.
# Accomplish this by sorting BACKWARDS in time within students.

# helper function: number of entries in a vector before a value
num_entries_before_val <- function(vec, val) {
  if(val %in% vec) {
    return(which(vec %in% val)[1] - 1)
  } else {
    stop("Error - target value not found in vector")
  }
}

# find missing students
students_missing_too_long <- data_cat_trim_early_non_data %>%
  arrange(comb_id, desc(week_start)) %>%
  group_by(comb_id) %>%
  summarise(missing_too_long = num_entries_before_val(imputed_row, FALSE) > max_weeks_missing) %>%
  filter(missing_too_long == TRUE)

# drop missing students
data_cat_trimmed <- data_cat_trim_early_non_data[!data_cat_trim_early_non_data$comb_id %in%
                                                     students_missing_too_long$comb_id, ]

# clean up
# rm(students_missing_too_long)

########## impute NAs downward to fill in vars of interest within subjects

#add SUBSET_TYPES to col_names_to_impute, otherwise the current imputation does not
# impute demographic vars (saved in SUBSET_TYPES)
col_names_to_impute <- c(col_names_to_impute, SUBSET_TYPES)
# add team_id to cols_to_impute. In the previous version we did not have to do this
# but now we have use team_id as part of the index, so we have to add the team_id manually
col_names_to_impute <- c(col_names_to_impute, "team_id", "class_name", "team_name",
                         "code", "expected_n", "teacher_name", "teacher_email")

# Within each comb_id, impute in all NA values below a non-NA value of col_names_to_impute.
data_cat_imputed <- by(data_cat_trimmed,
                        data_cat_trimmed$comb_id,
                   fill,
                   one_of(col_names_to_impute),
                   simplify = FALSE) %>%
  lapply(., as.data.frame) %>%
  util.rbind_intersection()
row.names(data_cat_imputed) <- NULL

#### Imputation complete!

# overwrite data_cat
data_cat <- data_cat_imputed

# If the script is not in debug-mode or otherwise restricting the data set,
# then save a copy of the imputed data.
if(!RESTRICT_DATA_FOR_TESTING) {
  full_file_path <- util.find_crypt_paths(list(imputed =
                                                 'EP metascript processed data/EP metascript processed data imputed.csv'))
  directory <- sub("/EP metascript processed data imputed.csv", "", full_file_path[[1]])
  setwd(directory)
  write.csv(data_cat, "EP metascript processed data imputed.csv")
}


############### BUILD COMPLETE PARTICIPATION TABLE (LONG FORMAT) ###############

# Inputs:
#  data_cat [team_id x class_id x student_ID x week_start]
#  data_cat_not_imputed [team_id x class_id x student_ID x week_start]
# Outputs:
#  part_data_long_exp [Week]: Participation data, long format, expanded (includes ALL possible weeks)

# First, create a vector of ID var names for any reporting units that we want to aggregate.
reporting_unit_id_var_id_names <- reporting_unit_id_var_types %+% "_id"

#  We already have expected_ns for each class in the Triton table -
#  group and unique/sum them to make expected_team_ns and expected_classroom_ns tables.

# This is a little helper function to get the expected ns for each value of a given type
# of reporting unit ID. It assumes that the data table argument has a column called "expected_n"
# and another one that is the id_var.
get_expected_ns_table <- function(id_var, data_table) {

  # sanity checks
  if(! "expected_n" %in% names(data_table)){
    stop("In get_expected_ns_table, data_table requires a column called 'expected_n'")
  }
  if(! id_var %in% names(data_table)){
    stop("In get_expected_ns_table, id_var column " %+% id_var %+% " not found in data_table")
  }

  my_table <- data_table %>%
    group_by_at(vars(one_of(id_var))) %>%
    summarise(expected_n = sum(expected_n, na.rm = T)) %>%
    as.data.frame()
  my_table$reporting_unit_id <- as.vector(my_table[, id_var])
  my_table[, id_var] <- NULL
  return(my_table)
}

# Use the helper function above to create one giant expected_ns table for all reporting units.
expected_ns <- lapply(reporting_unit_id_var_id_names, get_expected_ns_table, triton_tbl) %>%
  util.rbind_intersection()

# Melt all the data to get long participation data -
# Each row is defined by a reporting_unit_id/week_start combination.
part_data_long <- data_cat_not_imputed %>%
  melt(id.vars = c("week_start"),
       measure.vars = reporting_unit_id_var_id_names,
       variable.name = "reporting_unit_type",
       value.name = "reporting_unit_id") %>%
  group_by(reporting_unit_id, week_start) %>%
  summarise(n = n())

# Expand.grid to fill in all POSSIBLE weeks from the very first week of anyone submitting data.
# Then fill in missing N values with 0.
all_RUs <- unique(part_data_long$reporting_unit_id)
all_observed_weeks <- unique(part_data_long$week_start)
all_possible_weeks <- insert_missing_weeks(all_observed_weeks)
all_possible_RU_weeks <- expand.grid(week_start = all_possible_weeks,
                                     reporting_unit_id = all_RUs)
part_data_long_exp <- merge(part_data_long,
                            all_possible_RU_weeks,
                            by = c("reporting_unit_id", "week_start"),
                            all = T)
part_data_long_exp$n[is.na(part_data_long_exp$n)] <- 0

#  Calculate percentages at scale for every cell and format to char as needed
part_data_long_exp <- merge(part_data_long_exp,
                            expected_ns,
                            by = "reporting_unit_id",
                            all.x = T)
part_data_long_exp$n_formatted <- ((100 * part_data_long_exp$n) / part_data_long_exp$expected_n ) %>%
  round(., 0)
part_data_long_exp$n_formatted <- part_data_long_exp$n %+% " (" %+% part_data_long_exp$n_formatted %+% "%)"



############### CREATING AGG_METRICS: MAKE SUPER-MELTED RECENT DATA ###############

# Inputs:
#   data_cat [team_id x class_id x student_ID x week]
#   REPORTING_UNIT_ID_TYPES ("team_id", "classroom_id", etc.)
#   subset_types ("race", "gender", etc.)
#   All_metrics (“mw1_1”, mw1_2”, etc.)
# In the middle:
#   Filter data_cat to teams that are currently active, because they are the ones for whom we will make reports
#   Melt all_metrics columns down into rows, creating new columns “metric” and “value”
#   Melt subset_types columns down into rows, creating new columns “subset_type” and “subset_value”
#   Filter to most recent week per team (for calculating p-values later)
# Outputs:
#   d_super_melted_active [team_id x classroom_id x student_ID x week x metric x subset_type x subset_value]
#   d_super_melted_active_recent [team_id x classroom_id x student_ID x week x metric x subset_type x subset_value]

# Identify active teams and filter data_cat to active teams
days_since_last_vist_per_team_df <- compute_days_since_last_visit(
    grouping_var = data_cat$team_id,
    date_var = date(data_cat$StartDate),
    current_date = as.Date(REPORT_DATE)
    )
  active_teams <-
    days_since_last_vist_per_team_df[days_since_last_vist_per_team_df$min_lag <= TIME_LAG_THRESHOLD, "grouping_var"]
data_cat_active_teams <- data_cat[data_cat$team_id %in% active_teams, ]

# Identify and merge in most recent weeks for each team
most_recent_weeks_per_team <- data_cat_not_imputed %>%
  arrange(team_id, week_start) %>%
  group_by(team_id) %>%
  summarise(most_recent_observed_week = last(week_start))
data_cat_active_teams <-  merge(data_cat_active_teams, most_recent_weeks_per_team, by = "team_id", all.x = TRUE)

# Melt all_metrics columns down into rows
d_melted_active <- data_cat_active_teams %>%
  melt(id.vars = c(REPORTING_UNIT_ID_TYPES, "userID", "week_start", SUBSET_TYPES,
                   "most_recent_observed_week", "imputed_row", "team_name", "class_name"),
       measure.vars = all_metrics,
       variable.name = "metric") %>%
  rename(metric_value = value)

# Add in metric-level information and calculate if the response is in the good range
d_melted_active <- d_melted_active %>%
  merge(., items[,c("question_code","min_good","max_good")],
        by.x = "metric", by.y = "question_code",
        all.x = T) %>%
  mutate(good_range = (metric_value >= min_good &
           metric_value <= max_good))

# Now melt subsets down into rows
d_super_melted_active <- d_melted_active %>%
  melt(id.vars = c(REPORTING_UNIT_ID_TYPES, "userID", "week_start", "metric", "metric_value",
                   "most_recent_observed_week", "good_range", "imputed_row",  "team_name", "class_name"),
       measure.vars = SUBSET_TYPES,
       variable.name = "subset_type") %>%
  rename(subset_value = value)

# Some people have NA for some subset values (e.g. no gender recorded), even after imputation.
# Since we have melted subset values down into the rows, and we are going to calculate
# aggregate statistics for subset types and subset values, we need to remove the rows with
# NA subset values. This will not affect the rest of each participant's data.
d_super_melted_active <- d_super_melted_active[!is.na(d_super_melted_active$subset_value), ]

# Filter super-melted data to most recent observed week per team
d_super_melted_active_recent <- d_super_melted_active[d_super_melted_active$week_start %in%
                                                        d_super_melted_active$most_recent_observed_week, ]


############### CREATING AGG_METRICS: GET P-VALS FOR RECENT DATA ###############

# Inputs:
#   d_super_melted_active_recent [team_id x classroom_id x student_ID x week x metric x subset_type x subset_value]
#   REPORTING_UNIT_ID_TYPES ("team_id", "classroom_id", etc.)
# In the middle:
#   For each reporting_unit_type "r"...
#     group_by [r x week x metric x subset_type] (across people and subset_values) and summarize to calculate
#     p-values in column “p” using the “value” and “subset_type” columns.
#     Rename column r to “reporting_unit_id”.
#   Save all resulting data frames in a list.
#   Also rbind the list together to make All_pvals_df!
# Outputs:
#   Pvals_dfs:
#     A list of data frames of the form [reporting_unit_id x week x metric x subset_type]
#     As many data frames as there are reporting_unit_types
#   All_pvals_df [reporting_unit_id x week x metric x subset_type]

pvals_dfs <- list()

for(r in REPORTING_UNIT_ID_TYPES) {
  ru_pval_data <- d_super_melted_active_recent %>%
    group_by(.dots = c(r, "week_start", "metric", "subset_type")) %>%
    summarise(p = p_chi_sq(good_range, subset_value))
  names(ru_pval_data)[names(ru_pval_data) %in% r] <- "reporting_unit_id"
  pvals_dfs[[r]] <- ru_pval_data
}

all_pvals_df <- util.rbind_intersection(pvals_dfs)


############### CREATING AGG_METRICS: GET METRIC RESULTS ###############

# Inputs:
#   d_super_melted_active [team_id x classroom_id x student_ID x week x metric x subset_type x subset_value]
#   reporting_unit_types ("team_id", "classroom_id", etc.)
#   Agg_group_col_sets: list( c(“subset_type”, “subset_value”), NULL)
#     Note: this object allows us to group_by subset values to get metric results for each one,
#     but also to reuse the same code to get results across subsets using the NULL value.
# In the middle:
## BUILD THE METRIC-RESULT DATA FRAMES FOR SUBSETS
#   for each reporting_unit_type "r"...
#       index_cols <- c(r, subset_type, subset_value, week, metric)
#       new_df <- group_by(data, .dots = index_cols) %>% summarize metric results and save reporting_unit_type
#       as “r” and reporting_unit_label as the human_readable name.
#       Rename column r to “reporting_unit_id”.
#       Save to list.
## BUILD THE METRIC-RESULT DATA FRAMES FOR ALL STUDENTS (using the "medium-melted" data with subsets in columns)
#   for each reporting_unit_type "r"...
#       index_cols <- c(r, week, metric)
#       new_df <- group_by(data, .dots = index_cols) %>% summarize metric results and save reporting_unit_type
#       as “r” and reporting_unit_label as the human_readable name.
#       Rename column r to “reporting_unit_id”.
#       Set subset_type and subset_value to be "All Students"
#       Save to list.
# Finally rbind the two lists together to make All_metric_results_df!
# Outputs:
#   subset_metric_results_dfs:
#     A list of data frames of the form [reporting_unit_id x week x metric x subset_type x subset_value]
#     As many data frames as there are reporting_unit_types
#   all_students_metric_results_dfs:
#     A list of data frames of the form [reporting_unit_id x week x metric]
#     As many data frames as there are reporting_unit_types
#   All_metric_results_df [reporting_unit_id x week x metric x subset_type x subset_value]

# setup
subset_metric_results_dfs <- list()
all_students_metric_results_dfs <- list()

# Loop 1 of 2: Build subset_metric_results_dfs
for(r in REPORTING_UNIT_ID_TYPES) {
  # set index columns
  index_cols <- c(r, "subset_type", "subset_value", "week_start", "metric")
  # group and summarise to get metric results
  metric_results_df <- d_super_melted_active %>%
    group_by(.dots = index_cols) %>%
    summarise(pct_imputed = mean(imputed_row, na.rm=T),
              pct_good = mean(good_range, na.rm=T) ,
              mean_value = mean(metric_value, na.rm=T),
              se = se(good_range),
              n = length(good_range),
              class_name = first(class_name),
              team_name = first(team_name))
  # relabel reporting unit identifying information for proper rbinding and future access
  metric_results_df$reporting_unit_type <- r
  names(metric_results_df)[names(metric_results_df) %in% r] <- "reporting_unit_id"
  # and reporting_unit_name as the human_readable name.
  # NOTE: this is hacky and not a design that generalizes across RU types.
  metric_results_df$reporting_unit_name <- ifelse(metric_results_df$reporting_unit_type %in% "class_id",
                                                  metric_results_df$class_name,
                                                  ifelse(metric_results_df$reporting_unit_type %in% "team_id",
                                                         metric_results_df$team_name, NA))
  if(any(is.na(metric_results_df$reporting_unit_name))) {
    stop("Error - reporting unit names not properly mapped while getting metric results.")
  }
  # Add a column to denote the grand mean group (needed by Rmd)
  metric_results_df$grand_mean <- "Subset"
  # save to list metric_results_dfs
  subset_metric_results_dfs[[r]] <- metric_results_df
}

# Loop 2 of 2: Build all_students_metric_results_dfs
for(r in REPORTING_UNIT_ID_TYPES) {
  # set index columns
  index_cols <- c(r, "week_start", "metric")
  # group and summarise to get metric results
  metric_results_df <- d_melted_active %>%
    group_by(.dots = index_cols) %>%
    summarise(pct_imputed = mean(imputed_row, na.rm=T),
              pct_good = mean(good_range, na.rm=T) ,
              mean_value = mean(metric_value, na.rm=T),
              se = se(good_range),
              n = length(good_range),
              class_name = first(class_name),
              team_name = first(team_name))
  # relabel reporting unit identifying information for proper rbinding and future access
  metric_results_df$reporting_unit_type <- r
  names(metric_results_df)[names(metric_results_df) %in% r] <- "reporting_unit_id"
  # and reporting_unit_name as the human_readable name.
  # NOTE: this is hacky and not a design that generalizes across RU types.
  metric_results_df$reporting_unit_name <- ifelse(metric_results_df$reporting_unit_type %in% "class_id",
                                                  metric_results_df$class_name,
                                                  ifelse(metric_results_df$reporting_unit_type %in% "team_id",
                                                         metric_results_df$team_name, NA))
  if(any(is.na(metric_results_df$reporting_unit_name))) {
    stop("Error - reporting unit names not properly mapped while getting metric results.")
  }
  # Add a column to denote the grand mean group (needed by Rmd)
  metric_results_df$grand_mean <- "All Students"
  # Add columns for subset_type and subset_value (simply "All Students" for everyone)
  metric_results_df$subset_type <- "All Students"
  metric_results_df$subset_value <- "All Students"
  # save to list metric_results_dfs
  all_students_metric_results_dfs[[r]] <- metric_results_df
}


# Finally, rbind the lists together to make all_metric_results_df!
all_metric_results_df <- util.rbind_intersection(c(subset_metric_results_dfs, all_students_metric_results_dfs))

############### CREATING AGG_METRICS: MERGE P-VALS WITH METRIC RESULTS TO GET AGG_METRICS ###############

# Inputs:
#   All_pvals_df [reporting_unit_id x week x metric x subset_type]
#   All_metric_results_df [reporting_unit_id x week x metric x subset_type x subset_value]
# In the middle:
#   Merge by [reporting_unit_id x week x metric x subset_type], keeping all elements of both frames
#   Should expect to have the same number of rows as All_metric_results_df, but with some repeated p-values because those were calculated at a higher level of aggregation
#   Should have NAs for p-value for “All Students” rows - that’s good!
# Outputs:
#   Agg_metrics [reporting_unit_id x week x metric x subset_type x subset_value]

agg_metrics <- merge(all_pvals_df,
                     all_metric_results_df,
                     by = c("reporting_unit_id", "week_start", "metric", "subset_type"),
                     all = TRUE)

# Also need to make a subset_label column
agg_metrics$subset_label <- ifelse(agg_metrics$subset_value %in% "All Students", "All Students",
                                   agg_metrics$subset_value %+% " Students")


############ BUG FIX ##############
# create different team_study_class_df based on triton_tbl
# it seems that we can have different classes using the same name within a team
# this currently breaks the script, so I will switch to different looping structure
# @todo clean previous team_study_class_df
team_study_class_df <- triton_tbl[,
                                           c("team_id",
                                             "team_name",
                                             "class_name",
                                             "code",
                                             "class_id"
                                           )]
team_study_class_df$Study_ID <- "Study 1"
# exclude teams, if needed
if (EXCLUDE_TEAMS) {
  team_study_class_df <-
    team_study_class_df[
      !team_study_class_df$team_name %in% excluded_team_names,]
}

# exclude teams for which we do not have any data
unique_codes <- data_cat$code %>% unique
team_study_class_df  <- team_study_class_df[team_study_class_df$code %in% unique_codes,]


############### CREATE LOG FILES ###########

#create log files to track what is happening during rendering
basic_log <- team_study_class_df[, c("team_id", "class_name", "Study_ID", "code", "class_id")]
basic_log <- basic_log %>%
  rename(study_id = Study_ID)
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


############### CUT INACTIVE TEAMS FROM REPORTING ###############################

## compute days since last visit on team level
days_since_last_vist_per_team_df <- compute_days_since_last_visit(
  grouping_var = data_cat$team_id,
  date_var = date(data_cat$StartDate),
  current_date = as.Date(REPORT_DATE)
)
## find all classes which are members for teams which violate the time lag threshold,
## and remove those classes from the DF of classes to run
teams_to_exclude <-
  days_since_last_vist_per_team_df$grouping_var[days_since_last_vist_per_team_df$min_lag > TIME_LAG_THRESHOLD]
team_study_class_df <- team_study_class_df[!team_study_class_df$team_id %in% teams_to_exclude, ]


############### RENDER ENGAGEMENT DIAGNOSTICS ###############################


### SETUP:
# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}
# If team-only, pass in "TEAM" as name for all class-level reports
if (TEAM_ONLY) {
  team_study_class_df$class_name <- "TEAM"
}
# Save current value of TEAM_ONLY because it will get modified in the Rmd
orig_TEAM_ONLY <- TEAM_ONLY
# Create error-tracking column for use when running script
team_study_class_df$error <- NA



#### CLASS-LEVEL REPORTS:
for(i in 1:nrow(team_study_class_df)) {

    # Setup
    cat("Starting class: ", i)
    report_name <- NA

    # Retrieve the original value of TEAM_ONLY from outside the loop
    TEAM_ONLY <- orig_TEAM_ONLY

    # Declare local variable names for this report
    team_name           <- team_study_class_df[i,"team_name"]
    study_id            <- team_study_class_df[i,"Study_ID"]
    class_id        <- team_study_class_df[i,"class_id"]
    code                <- team_study_class_df[i,"code"]
    class_name      <- team_study_class_df[i,"class_name"]
    team_id             <- team_study_class_df[i,"team_id"]

    # if class was not found, replace with NONE
    if(length(class_id) == 0 )  class_id <- "NONE" %+% (sample(1:10000,1) %>% as.character)
    if(util.is_blank(class_id))  class_id <- "NONE" %+% (sample(1:10000,1) %>% as.character)


    # filter the big agg_metrics df to this particular report
    agg_metrics_small <- agg_metrics[agg_metrics$reporting_unit_id %in% c(team_id, class_id), ]

    # if class_id is TEAM (why??) rename
    if(class_id == "TEAM")  class_id <- "TEAM" %+% (sample(1:10000,1) %>% as.character)


    # Cut down the big participation table to the relevant reporting unit(s):
    # If the report is team-only, cut it to team, otherwise class and its team.
    if(TEAM_ONLY) {
        ru_ids_for_part_table <- c(team_id)
        ru_names_for_part_table <- c(team_name)
        } else {
        ru_ids_for_part_table <- c(team_id, class_id)
        ru_names_for_part_table <- c(team_name, class_name)
        }
    # make participation table for these RUs and fix column names
    participation_table_df <- part_data_long_exp %>%
      filter(reporting_unit_id %in% ru_ids_for_part_table) %>%
      select(-n, -expected_n) %>%
      dcast(week_start ~ reporting_unit_id, value.var = "n_formatted") %>%
      arrange(week_start)
    # add an order variable to confirm later cutting of this df in the Rmd
    participation_table_df$week_order <- 1:nrow(participation_table_df)
    # fix week label (note: I do this here because simplified labels cannot be easily sorted after dcasting)
    participation_table_df$week_start <- participation_table_df$week_start %>%
      gsub("20[0-9]{2}-[0]*", " ", .)  %>%
      gsub("-", "/", .)
    # order the columns so team is always before classroom
    team_column_name <- names(participation_table_df)[grep("Team_", names(participation_table_df))]
    class_column_name <- names(participation_table_df)[grep("Classroom_", names(participation_table_df))]
    participation_table_df <- participation_table_df[, c("week_start", team_column_name,
                                                         class_column_name, "week_order")]
    names(participation_table_df) <- c("Week of", ru_names_for_part_table, "week_order")


    # Assemble the open responses for this class and most recent week
    class_raw_data <- data_cat_not_imputed[data_cat_not_imputed$class_id %in% class_id, ]
    class_most_recent_week <- class_raw_data$week_start %>% unique %>% sort %>% last
    class_recent_open_responses <- class_raw_data[class_raw_data$week_start %in% class_most_recent_week,
                                                  active_or_question_ids]

    # Set up report name and path
    rmd_path     <- RMD_BASE_PATH %+% "engagement_diagnostic.Rmd"
    report_name  <- class_id %+% "." %+% REPORT_DATE %+% ".html" %>% gsub(" ", "", .)
    if (TEAM_ONLY) { report_name  <- team_id %+% "." %+% REPORT_DATE %+% ".html" %>% gsub(" ", "", .) }
    if (ANONYMOUS) { report_name = "anonymous.html" }
    report_base_path <- REPORT_BASE_PATH
    report_path   <- report_base_path %+% report_name

    # Get ready to run!
    paste0("Running ",report_name ) %>% message
    error_msg <- NA
    basic_log$file_name[basic_log$class_id == class_id] <- report_name

    # This step renders the .Rmd file. Note that the .Rmd file
    # is not configured to be renderable on its own, and relies on
    # variables from the metascript global namespace.
    possible_error <- tryCatch(
      ls_out <- create_report_list(),
      error = function(e){
        paste0("Error in ", report_name, ": ", e) %>%
          message
        return(e)
      }
    )


    # check the length of classes datasets for which we create or failed to create a report
    x <- data_cat[(data_cat$class_id %in% class_id) &
                    (data_cat$team_id %in% team_id),]
    team_study_class_df$error[i] <- paste0(possible_error, " nrow: ", nrow(x))
    # convert the HTMLS from the previous step to PDFs
    # I will have to change the working directory, so I will keep a copy of the
    # original path
    orig_wd <- getwd()
}


######### TEAM ONLY REPORTS ###############

# create team reports for each of the teams

# create team-only for-loop control data frame
team_study_df <- team_study_class_df[!duplicated(team_study_class_df$team_id),]

team_study_df$error <- NA # save potential errors for debugging
for(i in 1:nrow(team_study_df)) {

  # Setup
  cat("Starting team: ", i)
  report_name <- NA
  TEAM_ONLY <- TRUE

  # Declare local variables (why declare class info when these are team reports?)
  team_name           <- team_study_df[i,"team_name"]
  study_id            <- team_study_df[i,"Study_ID"]
  class_id        <- team_study_df[i,"class_id"]
  code                <- team_study_df[i,"code"]
  class_name      <- team_study_df[i,"class_name"]
  team_id             <- team_study_df[i,"team_id"]


  # filter the big agg_metrics df to this particular report:
  agg_metrics_small <- agg_metrics[agg_metrics$reporting_unit_id %in% c(team_id), ]

  # Set up report name and path
  rmd_path     <- RMD_BASE_PATH %+% "engagement_diagnostic.Rmd"
  report_name  <- team_id %+% "." %+% REPORT_DATE %+% ".html" %>%
    gsub(" ", "", .)
  if (ANONYMOUS) {
    report_name = "anonymous.html"
  }

  # Cut down the big participation table to the team,
  # and make participation table and fix column names
  # In this case, it is the team.
  ru_ids_for_part_table <- c(team_id)
  ru_names_for_part_table <- c(team_name)
  participation_table_df <- part_data_long_exp %>%
    filter(reporting_unit_id %in% ru_ids_for_part_table) %>%
    select(-n, -expected_n) %>%
    dcast(week_start ~ reporting_unit_id, value.var = "n_formatted") %>%
    arrange(week_start)
  # fix week label (note: I do this here because simplified labels cannot be easily sorted after dcasting)
  participation_table_df$week_start <- participation_table_df$week_start %>%
    gsub("20[0-9]{2}-[0]*", " ", .)  %>%
    gsub("-", "/", .)
  names(participation_table_df) <- c("Week of", ru_names_for_part_table)


  report_base_path <- REPORT_BASE_PATH
  report_path   <- report_base_path %+%
    report_name

  # Get ready to run!
  paste0("Running ",report_name ) %>%
    message
  error_msg <- NA

  # this step renders the .Rmd file. Note that the .Rmd file
  # is not configured to be renderable on its own, and relies on
  # variables from the metascript global namespace. The main
  # input to the .Rmd file is the object `data_cat` which is
  # the Qualtrics data that's had categorical variables recoded.
  basic_log$file_name[basic_log$class_id == class_id] <- report_name
  possible_error <-
    tryCatch(
      ls_out <- create_report_list(),
      error = function(e){
        paste0("Error in ", report_name, ": ", e) %>%
          message
        return(e)
      }
    )
  print(dim(ls_out))
  #check the length of classes datasets for which we create or failed to create a report
  x <- data_cat[(data_cat$team_id %in% team_id),]
  team_study_df$error[i] <- paste0(possible_error, " nrow: ", nrow(x))
  # currently we do not have option for team-only pdf reports, if we need one
  # we can easily model it by the class pdfs

  # compare it with previously captured output for a single team
  file_name <- "ls_out_Team_1SLg8wI833my3Qk5.txt"
  #dput(ls_out, file_name)
  ls_out_old = dget(file_name)
  if(!identical(ls_out, ls_out_old)) {
    stop("The old output for team Team_1SLg8wI833my3Qk5 is different from the new output")
  }
}

# turn off logging
closeAllConnections()

# Set back to regular interactive mode settings
interactive <- base::interactive


