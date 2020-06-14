# 1 - 2019 EP data pre-k-anon cleaning.R
# Daniel Greene
# 6/20/2019

# The purpose of this script is to take the raw EP data from Qualtrics and Triton and prepare it for k-anonymization.
# See this doc for a full description of steps:
# https://docs.google.com/document/d/1kbXWg5tIp0H_BBoby-AFnxJhD2loH9bSMOXCkc5IAXY/edit


############### USER-DEFINED INPUTS #####################################

general_data_folder_crypt_path <- "Data for 2018-2019 EP analysis"
qualtrics_crypt_data_path <- "Data for 2018-2019 EP analysis/1 - 2018-2019 raw Qualtrics data.csv"
triton_participant_tbl_crypt_data_path <- "Data for 2018-2019 EP analysis/participant_tbl.csv"
triton_classroom_tbl_crypt_data_path <- "Data for 2018-2019 EP analysis/classroom_tbl.csv"
triton_team_tbl_crypt_data_path <- "Data for 2018-2019 EP analysis/team_tbl.csv"
items_tbl_crypt_path <- "Data for 2018-2019 EP analysis/survey_items.csv"
elementary_teachers_crypt_path <- "Data for 2018-2019 EP analysis/elementary_teachers.csv"
test_teams_crypt_path <- "Data for 2018-2019 EP analysis/test_teams.csv"
or_data_folder_crypt_path <- "Data for 2018-2019 EP analysis/open response data w anonymization"



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

repair_pdds <- function(in_cell) {
  # it seems that in some cases the pdds are wrong
  # instead of __pdd__ we have _pdd__
  out_cell <- gsub("([^_])_pdd", "\\1__pdd", in_cell)
  return (out_cell)
}

repair_2nd_row <- function(in_df) {
  # it seems that in some cases the pdds are wrong
  # instead of __pdd__ we have _pdd__
  in_df[1,] <- in_df[1,] %>% apply (., 1, function(x) repair_pdds(x))
  return (in_df)
}


############### LOAD DATA #####################################

#### Qualtrics
# load Qualtrics
q <- util.find_crypt_paths(list(a = qualtrics_crypt_data_path))$a %>%
  read.csv()
# When Qualtrics imports data, it adds extra columns starting with "DO.Q" to indicate
# the order in which randomized response options were displayed for each respondent.
# But the DO.Q column name contains the name of the question itself, so the code can
# mix up the actual question with the DO.Q info. So remove all DO.Q info here.
q[grep("DO.Q", names(q))] <- NULL
# initial Qualtrics cleaning
q <- q %>%
  repair_2nd_row() %>%
  qc.clean_qualtrics()

  
### Triton classroom data
# load
classroom_tbl <- util.find_crypt_paths(list(a = triton_classroom_tbl_crypt_data_path))$a %>%
  read.csv()

### Triton team data
# load
team_tbl <- util.find_crypt_paths(list(a = triton_team_tbl_crypt_data_path))$a %>%
  read.csv() %>%
  rename(team_id = uid,
         team_name = name) %>%
  select(team_id, team_name)

### Triton participant data
# load
participant_tbl <- util.find_crypt_paths(list(a = triton_participant_tbl_crypt_data_path))$a %>%
  read.csv() %>%
  rename(student_id = uid,
         raw_id = name)

### Survey items info
items <- util.find_crypt_paths(list(a = items_tbl_crypt_path))$a %>%
  read.csv()

### Elementary teachers info
elementary_teachers <- util.find_crypt_paths(list(a = elementary_teachers_crypt_path))$a %>%
  read.csv()

### Test teams info
test_teams <- util.find_crypt_paths(list(a = test_teams_crypt_path))$a %>%
  read.csv()

############### MERGE DATA #####################################

# Merge classroom_tbl with q by code, only keeping records that can be matched
d <- merge(q,
           classroom_tbl[, c("code", "team_id", "uid", "contact_id", "grade_level")],
           by = "code",
           all.x = FALSE,
           all.y = FALSE) %>%
  rename(class_id = uid,
         teacher_id = contact_id,
         student_id = participant_id)

# sanity check: no missing values for any of the core identifiers
any(is.na(d[, c("student_id", "class_id", "team_id", "teacher_id")])) # should be FALSE

# Merge in team names for later filtering of test teams
d <- merge(d,
           team_tbl,
           by = "team_id",
           all.x = TRUE,
           all.y = FALSE)

# Merge in triton raw IDs from participant data (we need this to fix some incorrect student IDs)
d <- merge(d,
           participant_tbl[, c("student_id", "raw_id")],
           by = "student_id",
           all.x = TRUE,
           all.y = FALSE)


############### FIX IDS #####################################

# According to Chris, there was a database bug early on that prevented some raw IDs before 9/9 from being saved.
# People who showed up w no raw ID got a new participant_id every time,
# and there's no way to track them over time, so they are basically one-off anomaly students in the data.
# To reflect how these students are seen by the system, we replace the blank raw IDs in these responses
# with a unique random value. The participant ID of the response will do fine.
# NOTE: test data may also have a blank raw_id if it never showed up in Neptune.
d$raw_id[is.na(d$raw_id)] <- d$student_id[is.na(d$raw_id)]
# now we can create a "universal ID" (raw ID + team) with these fixed raw IDs
d$universal_id <- d$raw_id %+% "__" %+% d$team_id


# In addition, about 500 universal IDs (raw ID + team ID) have TWO student IDs associated.
# Same raw ID on the same team, but two student IDs. This should be impossible.
# According to Chris, and supported by Dan's experimentation, this bug is restricted to Josh's team.
# Josh's team started early, and students were originally added to "Triton" organization, then to "Team_...".
# This resulted in the same raw ID getting assigned two different student IDs,
# all under "Team_..." in the Qualtrics data. The same people show up twice!
# To fix this, we recode student IDs in the Qualtrics data to be the temporally FIRST student ID
# that was seen to be associated with that universal ID. This merges two "students" into one.
# Note: students might now have multiple distinct demographic records associated with one student ID.
# But a later section only propagates the FIRST demographic response. Should be fine.

# Get the first student id seen for each universal ID
universal_id_to_first_student_id <- d %>%
  arrange(universal_id, StartDate) %>%
  group_by(universal_id) %>%
  summarise(first_student_id = first(student_id))
# Merge in
d <- merge(d,
              universal_id_to_first_student_id,
              by = "universal_id",
              all.x = TRUE,
              all.y = FALSE)
# Overwrite and clean up
d$student_id <- d$first_student_id
rm(universal_id_to_first_student_id)


############### CHUNK WEEKS #####################################

qualtrics_date_format <- "%Y-%m-%d %H:%M:%S"
stripped_time <- strptime( d$StartDate , qualtrics_date_format )
d$week_start <- lubridate::floor_date(stripped_time, unit = "week") %>% as.Date()
d$day_start <- lubridate::floor_date(stripped_time, unit = "week") %>% as.Date()


############### PROPAGATE DEMOGRAPHIC INFO #####################################

# Identify demographic items
demographic_items <- c("ba_gpa_1",
                       "gender_v2",
                       names(d)[grep("race_chk\\.", names(d))])

# Get first recorded value for each demographic item for each student
# This is the authoritative student demographics
dem <- d %>%
  arrange(student_id, StartDate) %>%
  group_by(student_id) %>%
  filter(StartDate %in% first(StartDate))
dem <- dem[, c("student_id", demographic_items)]

# Delete original demographic columns and merge in authoritative version.
# Now every row has the authoritative demographics for that student.
d[, demographic_items] <- NULL
d <- merge(d,
           dem,
           by = "student_id",
           all = TRUE)


############### HANDLE DUPLICATE RESPONSES #####################################

# Whenever there are multiple rows with the same team/student/week, retain the LAST one.

d <- d %>%
  arrange(team_id, student_id, StartDate) %>%
  group_by(team_id, student_id, week_start) %>%
  filter(StartDate %in% last(StartDate))


############### CLEAN GRADE LEVEL VALUES #####################################

# Grade level values are messy... do some common-sense cleaning.
d$grade_level[grepl("(Physics)|(CP)|(PE)", d$grade_level)] <- NA

############### AGGREGATE RACE CHECKBOX CATEGORIES #####################################


## Clean checkbox categories for easier working:
# Setup
race_chk_qs <- names(d)[grep("race_chk\\.", names(d))]
race_chk_data <- d[, race_chk_qs]
# If there's anything in the race checkbox data except 1 or NA, throw an error
unique_vals_in_race_chk_data <- unique(unlist(race_chk_data))
if(length(setdiff(unique_vals_in_race_chk_data, c(1, NA))) > 0) {
  stop("Error - something in the race checkbox data is not 1 or NA.")
}
# Turn them into nice Booleans and rename them
fix_boolean <- function(vec) {ifelse(is.na(vec), 0, vec) %>% as.logical()}
race_chk_qs_nice_names <- grep("race_chk_", items$variable, value = TRUE)
d[, race_chk_qs_nice_names] <- util.apply_columns(d[, race_chk_qs], fix_boolean)


## Reduce cleaned checkboxes to a 7-checkbox system:
d$race_partially_NatAm <- d$race_chk_NatAmAlaskan
d$race_partially_Asian <- d$race_chk_EastAsian | d$race_chk_SoutheastAsian |
  d$race_chk_SouthAsian | d$race_chk_OtherAsian
d$race_partially_HispLat <- d$race_chk_MexChicano | d$race_chk_PuertoRican |
  d$race_chk_CentAm | d$race_chk_OtherHispLat
d$race_partially_Black <- d$race_chk_AfAmBlack | d$race_chk_African |
  d$race_chk_Carib | d$race_chk_OtherBlack
d$race_partially_White <- d$race_chk_Euro | d$race_chk_MidEast |
  d$race_chk_OtherWhite
d$race_partially_PacIsl <- d$race_chk_PacIsl
d$race_partially_Other <- d$race_chk_Other


## Reduce 7-checkbox system to 6-category conventional system, using assumption of
## assigning people to most marginalized group based on test scores
d$race6 <- NA
d$race6 <- ifelse(d$race_partially_Other, "Other", d$race6)
d$race6 <- ifelse(d$race_partially_White, "White", d$race6)
d$race6 <- ifelse(d$race_partially_Asian | d$race_partially_PacIsl, "Asian or PacIsl", d$race6)
d$race6 <- ifelse(d$race_partially_HispLat, "Hispanic or Latino", d$race6)
d$race6 <- ifelse(d$race_partially_NatAm, "Native American", d$race6)
d$race6 <- ifelse(d$race_partially_Black, "Black or African American", d$race6)


############### PREP FOR K-ANONYMIZATION #####################################

# transform gender_v2 1, 2, 3 into Male, Female, Non-Binary/Other and rename to gender
d$gender <- util.recode(d$gender_v2,
                         c(1, 2, 3),
                         c("Male", "Female", "Non-Binary/Other"))
# rename race6 to race
d$race <- d$race6

############### REMOVE NONSENSE ROWS #####################################

# Remove rows that don't have an associated student, classroom, teacher, or team
d <- filter(d,
            !is.na(student_id),
            !is.na(class_id),
            !is.na(teacher_id),
            !is.na(team_id))

# Remove rows where our internal “testing” variable is not empty
d <- filter(d, testing %in% "")

# Remove rows where the team name is on an internal list of test teams (e.g. “Dan’s Fake Team”)
d <- filter(d, !team_name %in% test_teams$team_name)

# Remove rows where the teacher is an elementary school teacher
d <- filter(d, !teacher_id %in% elementary_teachers$teacher_id)

# Remove ~1200 rows where students say that they don’t feel comfortable answering honestly or that they are rushing
d <- filter(d,
            !honest_comfort %in% 1,
            !rushing %in% c(4, 5))


############### CUT UNNECESSARY OR REVEALING COLUMNS #####################################

columns_to_keep <- c("ResponseID", "student_id", "StartDate", "Finished", "Q_TotalDuration",
                     names(d)[grep("tc[1-4]_2", names(d))],
                     names(d)[grep("fg[1-4]_2", names(d))],
                     names(d)[grep("mw[1-4]_2", names(d))], "or_5",
                     "eng_1", "attn", "rushing", "va_grade_1",
                     "gms_2.0", "gms_3.0", "belong_4", "belong_3.1",
                     "mastery1", "mastery2", "teacher_use_qs", "honest_comfort",
                     "class_id", "teacher_id", "grade_level", "team_id",
                     "week_start", "ba_gpa_1", "gender", "race")

# dt for d trimmed
dt <- d[, columns_to_keep]


############### CLEAN OPEN RESPONSES #####################################

# First, set the empties to NA (used this one time to save off data)
or_vars <- c("or_tc1_2", "or_tc2_2", "or_tc4_2",
             "or_fg1_2", "or_fg2_2", "or_fg3_2",
             "or_mw1_2", "or_mw2_2", "or_mw3_2", "or_5")
blank_to_na <- function(vec) {ifelse(vec %in% "", NA, vec)}
dt[, or_vars] <- util.apply_columns(dt[, or_vars], blank_to_na)
dt$row <- 1:nrow(dt)

# Delete the original OR columns (we will replace w anon versions below)
dt[, or_vars] <- NULL


## Import anonymized versions of open response data
# For each var...
for(var_name in or_vars) {
  # Import the corresponding df
  or_df <- util.find_crypt_paths(list(a = or_data_folder_crypt_path %+% "/" %+% var_name %+% ".csv"))$a %>%
    read.csv()
  # Strip it to just the anonymized values and the responseIDs
  anon_column_name <- var_name %+% "_anonymized_by_dan"
  or_df <- or_df[, c("ResponseID", anon_column_name)]
  # Merge with d by ResponseID
  dt <- merge(dt,
             or_df,
             by = "ResponseID",
             all.x = TRUE,
             all.y = FALSE)
}

# rename anonymized columns
names(dt) <- gsub("_by_dan", "", names(dt))


############### SAVE #####################################

generic_save_path <- util.find_crypt_paths(list(a = general_data_folder_crypt_path))$a
write.csv(dt, generic_save_path %+% "/2 - 2018-2019 pre-k-anon cleaned data.csv")

