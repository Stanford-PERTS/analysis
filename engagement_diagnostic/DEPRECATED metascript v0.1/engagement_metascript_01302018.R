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


############### SET CONTROLLABLE PARAMETERS #################################


REPORT_DATE <- "2018-02-12" # this is the Monday when the reports are due
#REPORT_DATE <- "2018-02-05" # this is the Monday when the reports are due


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

TIME_LAG_THRESHOLD <- 9 # Teams who haven't participated for more days than the threshold are excluded
# from the report generating part.
# I choose 9 rather than 7 days because the weekend could be confusing (it could add 2 days)

# disaggregation groups
# collapse together these disaggregation groups if needed for cell size
SUBSETS <- c("gender", "race_cat", "ELL_status")

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

############### LOAD LIBRARIES AND PATHS #####################################
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
############### LOAD DATA ####################################


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

triton_tbl$reporting_unit_id_triton <- triton_tbl$classroom_id_triton

# merge indormation from triton to the qualtrics dataframe
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
data <- data[data$code %in% triton_tbl$code,]


# Temporarily add study ID info and some items info
data$Study_ID <- "Study 1"
# names(items)[names(items) %in% "lausd_general_category"] <- "driver"
# items$driver[items$driver %in% "Belonging"] <- "belonging"
# items$driver[items$driver %in% "Relevance"] <- "relevance"
# items$driver[items$driver %in% c("GMS", "growth_mindset")] <- "growth_mindset_(general)"

#for debugging (@todo remove later)
# data <- data[data$classroom_id_triton %in% c("Classroom_RgilBMbB14iwkyPe"), ]
#data <- data[data$team_id %in% c("FuelEd Alum"), ]
#data <- data[data$team_id_triton %in% c("Team_zXEjOmzZWzVPgyvv"), ]
#data <- data[data$team_id %in% c("NYC Feedback for Growth"), ]


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

data_cat_not_imputed <- data_cat #keep it for printing summary table


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
if(!RESTRICT_TO_ONE_RU & !RESTRICT_TO_ONE_TEAM) {
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
# Study, team, reporting unit, user
data_cat$comb_id <-
  paste(data_cat$Study_ID,
        #data_cat$team_id,
        data_cat$team_id_triton,
        data_cat$reporting_unit_id_triton,
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
data_cat_w_blanks[, c("Study_ID", "team_id_triton", "reporting_unit_id_triton", "userID")] <- str_split_matrix


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

#add SUBSETS to col_names_to_impute, otherwise the current imputation does not
# impute demographic vars (saved in SUBSETS)
col_names_to_impute <- c(col_names_to_impute, SUBSETS)
# add team_id to cols_to_impute. In the previous version we did not have to do this
# but now we have use team_id_triton as part of the index, so we have to add the team_id manually
col_names_to_impute <- c(col_names_to_impute, "team_id", "reporting_unit_id")

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
if(!RESTRICT_TO_ONE_RU & !RESTRICT_TO_ONE_TEAM) {
  full_file_path <- util.find_crypt_paths(list(imputed =
                                                 'EP metascript processed data/EP metascript processed data imputed.csv'))
  directory <- sub("/EP metascript processed data imputed.csv", "", full_file_path[[1]])
  setwd(directory)
  write.csv(data_cat, "EP metascript processed data imputed.csv")
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
                                             "classroom_id_triton",
                                             "team_id_triton"
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


############### CREATE LOG FILES ###########


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


############### RENDER ENGAGEMENT DIAGNOSTICS ###############################


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

# do not generate reports for teams for whom we don't have data for more than 9 days

# compute days since last visit on team level
days_since_last_vist_per_team_df <- compute_days_since_last_visit(
  grouping_var = data_cat$team_id_triton,
  date_var = date(data_cat$StartDate),
  current_date = as.Date(REPORT_DATE)
)

# find all classrooms which are members for teams which violate the time lag threshold
teams_to_exclude <-
  days_since_last_vist_per_team_df$grouping_var[
    days_since_last_vist_per_team_df$min_lag > TIME_LAG_THRESHOLD
    ]

classrooms_to_exclude <- classroom_tbl$uid[classroom_tbl$team_id %in% teams_to_exclude]
# remove those classrooms from the for loop
team_study_reporting_unit_df <-
  team_study_reporting_unit_df[
    !team_study_reporting_unit_df$triton_id %in% classrooms_to_exclude,
    ]


team_study_reporting_unit_df$error <- NA
# keep a copy of the data sent to the Rmd for debugging purposes (@todo remove later)
data_cat_new <- data_cat
for(i in 1:nrow(team_study_reporting_unit_df)) {
  cat("Starting classroom: ", i)
  report_name <- NA
  TEAM_ONLY <- orig_TEAM_ONLY

  team_id             <- team_study_reporting_unit_df[i,"Team_ID"]
  study_id            <- team_study_reporting_unit_df[i,"Study_ID"]
  reporting_unit_id    <- team_study_reporting_unit_df[i,"reporting_unit"]
  code <- team_study_reporting_unit_df[i,"code"]
  reporting_unit_id_triton <- team_study_reporting_unit_df[i,"triton_id"]
  team_id_triton <- team_study_reporting_unit_df[i,"team_id_triton"]

  # classroom was not found, replace with NONE
  if(length(reporting_unit_id_triton) == 0 )  reporting_unit_id_triton <- "NONE" %+% (sample(1:10000,1) %>% as.character)
  if(util.is_blank(reporting_unit_id_triton))  reporting_unit_id_triton <- "NONE" %+% (sample(1:10000,1) %>% as.character)
  if(reporting_unit_id_triton == "TEAM")  reporting_unit_id_triton <- "TEAM" %+% (sample(1:10000,1) %>% as.character)


  rmd_path     <- RMD_BASE_PATH %+% "engagement_diagnostic_01302018.Rmd"
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
  #check the length of classrooms datasets for which we create or failed to create a report
  x <- data_cat[(data_cat$reporting_unit_id %in% reporting_unit_id) &
                  (data_cat$team_id_triton %in% team_id_triton),]
  team_study_reporting_unit_df$error[i] <- paste0(possible_error, " nrow: ", nrow(x))
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

# Check if class files are properly saved

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



######### TEAM ONLY REPORTS ###############

# create team reports for each of the teams

# create team-only for-loop control data frame
team_study_df <- team_study_reporting_unit_df[!duplicated(team_study_reporting_unit_df$team_id_triton),]

team_study_df$error <- NA

for(i in 1:nrow(team_study_df)) {
  cat("Starting team: ", i)
  report_name <- NA
  TEAM_ONLY <- TRUE

  team_id             <- team_study_df[i,"Team_ID"]
  study_id            <- team_study_df[i,"Study_ID"]
  reporting_unit_id    <- team_study_df[i,"reporting_unit"]
  code <- team_study_df[i,"code"]
  reporting_unit_id_triton <- team_study_df[i,"triton_id"]
  team_id_triton <- team_study_df[i,"team_id_triton"]

  rmd_path     <- RMD_BASE_PATH %+% "engagement_diagnostic_01302018.Rmd"
  report_name  <- team_id_triton %+% "." %+% REPORT_DATE %+% ".html" %>%
    gsub(" ", "", .)

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

  #check the length of classrooms datasets for which we create or failed to create a report
  x <- data_cat[(data_cat$team_id_triton %in% team_id_triton),]
  team_study_df$error[i] <- paste0(possible_error, " nrow: ", nrow(x))
  # currently we do not have option for team-only pdf reports, if we need one
  # we can easily model it by the class pdfs

}

# turn off logging
closeAllConnections()

# Set back to regular interactive mode settings
interactive <- base::interactive

