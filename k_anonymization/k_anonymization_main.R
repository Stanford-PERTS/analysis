# In this script we will k-anonymize the cb17 script
# the general flow could be found here: https://www.lucidchart.com/documents/edit/621172f2-5385-4c36-88ed-c7ef69e33017
# notes from the cb17 round could be found here: https://docs.google.com/document/d/1qWNd1wtchikQs8bDolr8UIdAWMasvoN0XnakGPdlbcY/edit#heading=h.38rfg5srt49k
# for manual check here: https://docs.google.com/document/d/15QNAQqVj4BGabLawhZlkdorqodgwVpsmawuH8sLBRP4/edit#

# Update:
# The original plans has changed. The current version of the script executes the following steps

# 1. For each key variable it creates multiple variables with different levels of resolution. For example,
# if race has 8 levels (7 content levels plus NA), then we create 8 race variables, containing between 1
# and 8 levels. The higher the suffix, the higher the resolution is. Suffix 1 is the same as deleting the variable,
# suffix 2 contains information only if a level is missing or not. Suffix 3 is a binary split (three levels since we have NAs), etc.

# 2. The merging of different levels is controlled by a google worksheet. The merges are always binary, which means
# that there are as many merges as there are non-missing levels. Further, by contolling the order of merges, we can
# control not only which levels are combined in a superordinate categories, but also  which variables are excluded. For
# example, we can merge 3 levels of race, then exclude gender (by running all gender merges),
# then merge 2 more levels of race, and then exclude parents education. Describing the merging order is not very easy. I
# suggest building the trees in lucid charts before defining them in the google worksheet.

# 3. After creating the variable resolution versions of the  key variables, for each group we check if cross-tabulation
# violates the minimum cell threshold. We start from the highest resolution (e.g. race_8), and if we find violation we move
# one level up (means lower resolition), as defined in the tree_spec_df unitll we reach resolution for which we can pass the min_cell threshold. All
# higher level resolutions are masked. Notice that when check for a min_cell threshold, we disregard NA levels, and we disregard
# 0 frequencies.

# 3.1 @todo add testing for min_cell for single variables instead than for cross-tabs. Since we exclude "n/a" from
# the threshold test, it is theoretically possible to miss a violation which comes from a single variable, not from
# a cross-tab. For example, if there 4 Native Americans, and their par_ed is "n/a", the current algorithm will miss this part
# I checked and the cb17 data doesn't have this problem, but in theory it is possible to happen, so in the future this should be
# repaired

# 4. The final database contains all version of the key variables for which at least one college has a value. Most colleges, however,
# wil have only low resolition key variables due to  sample size, and for the high resolution variables they will have NAs.

# Note: The description here assumes that we run within college. It should be straightforward, however, to adjust it to work
# on levels higher or lower than college. One suggestion from Dave, which we might want to implement in the future, is to exclude
# large categories first, so within a college different subjects will have different resolution levels.



# input files:
## input_data_k_anonym.csv - comes from the report generation script
## helper_functions.csv -

# output files:
## cb17_anonymized.csv
## token_map.csv
## basic_log_df.csv
## log_df.csv

# DEFINE CONSTANTS
MIN_CELL <- 5 # controls what is the minimum cell size for masking a category

# LOAD LIBRARIES
library(dplyr)

# source gymnast functions
tryCatch({
  source("~/Sites/gymnast/R/util.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_qualtrics_cleaning.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_graphing.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_scale_computation.R", chdir = TRUE)
}, error = function(e){
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_qualtrics_cleaning.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_graphing.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/scale_computation.R")
})

# DEFINE PATHS
REPO_PARENT <- "~/Sites/"
RMD_BASE_PATH <- REPO_PARENT %+% "analysis/k_anonymization"
DROPBOX_DIR <- "~/Dropbox (PERTS)/"

SALT_FILE_NAME <- "ep_salt.R"
INPUT_FILE_NAME <-  "2018_ep_summary_data_not_anon.csv"
OUTPUT_FILE_NAME <- "k_anonymized_data.csv"
# some input file options:
# "ep_anonymization_input_df.csv"
# "single_team_test_ep_input_df.csv"
# "k_anon_ep_test_input.csv"
# "2018_ep_summary_data_not_anon.csv"   <--- this is the 2017-2018 data





#source(RMD_BASE_PATH %+% "perts_analyses/cb17_reports/cb17_helpers.R")
source(RMD_BASE_PATH %+% "/helper_functions.R")

library(rmarkdown)
library(tidyr)
library(reshape2)
library(tools)
library(jsonlite)
library(googlesheets)
library(RCurl)
library(digest)

# set stringsAsFactors to FALSE, and add any other global R settings
options(stringsAsFactors = FALSE)
options(scipen=999) #avoids scientific notations
###########################

# READ DATA
# the data input comes from the report generation script. This is the data that will
# be anonymized.
input_path <- util.find_crypt_paths(list(f1 = INPUT_FILE_NAME)) %>%  unlist
input_df <- read.csv(input_path, stringsAsFactors = FALSE)

# read salt file

SALT_PATH <- util.find_crypt_paths(list(f1 = SALT_FILE_NAME)) %>%  unlist
source(SALT_PATH)


# DATASET SPECIFIC ADJUSTMENTS
# currently we will have options for CB17 and EP. If this list grows, we might
# think about structural changes in the script.

# comment or uncomment depending on the pariticular dataset

############# CB17 ##################
#
#
# key_var_cats <- c("report_race", "report_par_ed")
#
# #input_df$report_gender[input_df$report_gender %in% "Other"] <- NA
# med_age <- input_df$t1__demog__english_age %>% median(., na.rm = T) #not sure what variable this is
# input_df$age_median <- ifelse(input_df$t1__demog__english_age >= med_age, "High", "Low")
# demogr_vars <- c("disadv_race", "report_gender", "disadv_educ", "age_median")
# input_df$org_id <- input_df$school_name # this is the organization identifier
#
#
#
# # recode variables
# input_df$report_par_ed <- input_df$report_par_ed %>%
#   util.recode(
#     c("Continuing-Generation \n (At least one parent has a four-year\n college degree)",
#       "First-Generation\n (Neither parent has a\n four-year college degree)" ),
#     c("Continuing",
#       "First-Generation" )
#   )
#
# input_df$report_yob <- NA
# input_df$report_yob[input_df$t1__demog__yob < 1999] <- "<1999"
# input_df$report_yob[input_df$t1__demog__yob >= 1999] <- ">=1999"
#
# input_df[, key_var_cats] <- input_df[,key_var_cats] %>% lapply(.,function(x) replace_na(x,"n/a"))
#
#
#
# survey_cols_vec <- names(input_df) %>% grepl("_dv_",.)
# survey_cols <-  names(input_df)[survey_cols_vec]
# # remove survey cols containg text (they are marked with _TEXT_)
# survey_cols <- survey_cols[!grepl("_TEXT_",survey_cols)]

######### end CB17 #############



########## EP #########

# ### TO DO: move these transformations out of the k-anon script into a separate module
#   # transform gender_v2 1, 2, 3 into Male, Female, Non-Binary/Other and rename to gender
#   input_df$gender <- util.recode(input_df$gender_v2,
#                                  c(1, 2, 3),
#                                  c("Male", "Female", "Non-Binary/Other"))
#   # rename race6 to race
#   input_df$race <- input_df$race6
# 
# 
# demogr_vars <- c("race", "gender") # these are for report tables only, not used in anonymization
# key_var_cats <- c("race", "gender")
# input_df$org_id <- input_df$team_id
# input_df$participant_id <- input_df$userID
# survey_cols <- c("va_grade_1", "fg1_1", "fg2_1","fg3_1","mw1_1","mw2_1","mw3_1","tc1_1","tc2_1","tc4_1",
#                  "fg1_2","fg2_2","fg3_2","mw1_2","mw2_2","mw3_2","tc1_2",
#                  "tc2_2","tc4_2", "ba_gpa_1", "StartDate", "learning_conditions") # ? should I add in_target group
# survey_cols <- survey_cols[survey_cols %in% names(input_df)]
# 
# # replace blanks with "n/a" for key vars
# for(var in key_var_cats) {
#   input_df[util.is_blank(input_df[, var]), var] <- "n/a"
# }
# 
# # since EP might have multiple entries per participant, check if there is contradictory key_var values
# for(var in key_var_cats) {
#   count_df <- input_df %>%
#     group_by(participant_id) %>%
#     summarise(n_entries = length(unique(get(var))))
#   if(length(setdiff(count_df$n_entries, c(1))) > 0) {
#     stop("Some participants have more than one value for " %+% var %+% "!")
#   }
# }


########## end EP #########


########## TEST EP DATA #########

# This is a little setup section for the EP test input data, k_anon_ep_test_input.csv.
# clone team should have no k-anon masking needed at all.
# one gender outlier team should have regular gender masked and gender_3 totally unmasked, 
# and all race unmasked (using only step 1)
# one race outlier team should have regular gender masked, gender_3 unmasked "Male/nonbinary", 
# regular race masked, and race_6 unmasked (using only step 2)
# gender and race outlier team should be SAME as one race outlier team (using steps 1 and 2)

# demogr_vars <- c("race", "gender") # these are for report tables only, not used in anonymization
# key_var_cats <- c("race", "gender")
# survey_cols <- c("survey_data")

########## end test EP data #########


########## 2018 EP DATA #########

# rename old vars
input_df$race <- input_df$race_cat
input_df$org_id <- input_df$team_id
input_df$participant_id <- input_df$userID

# define var sets for k-anon
demogr_vars <- c("race", "gender") # these are for report tables only, not used in anonymization
key_var_cats <- c("race", "gender")
survey_cols <- setdiff(names(input_df), key_var_cats)

# replace blanks with "n/a" for key vars
for(var in key_var_cats) {
  input_df[util.is_blank(input_df[, var]), var] <- "n/a"
}

# since EP might have multiple entries per participant, check if there is contradictory key_var values
for(var in key_var_cats) {
  count_df <- input_df %>%
    group_by(participant_id) %>%
    summarise(n_entries = length(unique(get(var))))
  if(length(setdiff(count_df$n_entries, c(1))) > 0) {
    stop("Some participants have more than one value for " %+% var %+% "!")
  }
}


########## end 2018 EP data #########


# @todo: It will be nice to have a check if any of the survey columns contain text.



cols_to_keep <- c(key_var_cats, survey_cols, "org_id", "hashed_id")

input_df$hashed_id <- salt_n_hash(input_df$participant_id, salt = salt)
# save a token map
TOKEN_PATH <- gsub(SALT_FILE_NAME, "token_map.csv", SALT_PATH)
write.csv(
  input_df[, c("participant_id", "hashed_id", "org_id")],
  TOKEN_PATH,
  row.names=FALSE
)




melted_df <- data.frame()

for (demogr_var in demogr_vars) {
  buff_df <- input_df[!duplicated(input_df$participant_id),]
  buff_df$variable <- buff_df[,demogr_var]
  current_df <- buff_df %>%
    group_by(org_id, variable) %>%
    summarise(count_n = length(unique(participant_id)))
  current_df <- current_df[!current_df$variable %in% c("<NA>", "NA", NA),]
  current_df$variable <- paste0(demogr_var, "_", current_df$variable)
  melted_df <- bind_rows(melted_df, current_df)
}
casted_df <- melted_df %>% dcast(org_id ~ variable)
BASIC_LOG_PATH <- gsub(SALT_FILE_NAME, "basic_log.csv", SALT_PATH)
write.csv(casted_df, BASIC_LOG_PATH, row.names = F)

##### BUILDING A HIEARARCHICAL CATEGORY MERGE

# read specs from google worksheet
# specs are defined here: https://docs.google.com/spreadsheets/d/1m1V7aLKT-nHTf0nuPFh2zpcobw66rZty-HUZPRhmIcU/edit#gid=0
# a graphic example here:https://www.lucidchart.com/documents/edit/f433464a-8260-4988-8a3b-b85e01f3a189#


# Choose which specs to load
# main worksheet (for testing only)
#tree_specs_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTq1gMoN1pMvCDYOhsn7ieXIevw-_n32r_z3DvjSmddiH6dquzyihQ-txIXcDpO8MSG5ikalvRVECbk/pub?gid=0&single=true&output=csv"
# CB17 tree
#tree_specs_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTq1gMoN1pMvCDYOhsn7ieXIevw-_n32r_z3DvjSmddiH6dquzyihQ-txIXcDpO8MSG5ikalvRVECbk/pub?gid=1755769169&single=true&output=csv"
# EP TREE
tree_specs_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTq1gMoN1pMvCDYOhsn7ieXIevw-_n32r_z3DvjSmddiH6dquzyihQ-txIXcDpO8MSG5ikalvRVECbk/pub?gid=1424362082&single=true&output=csv"

tree_specs_df <- read.csv(tree_specs_url, na.strings = c("", " "), stringsAsFactors = F)

# clear white spaces
tree_specs_df <- tree_specs_df %>% lapply(., function(x) util.trim(x)) %>%
  as.data.frame(., stringsAsFactors = F)
tree_specs_df$global_order <- tree_specs_df$global_order  %>% as.numeric()

# Create all levels of resolution for a category, moving from n to 1
# where n is the number of original category levels and contains maximum information
processed_df <- input_df
processed_df <- processed_df[,cols_to_keep]

# check if all remaing survey columns are numeric
#apply(processed_df[,survey_cols],2, function(x) is.numeric(x)) %>% table()

# create local order variable, within each category
tree_specs_df <-  tree_specs_df %>% create_local_order()

# add indexed category name for different levels of resolution
tree_specs_df <- tree_specs_df  %>%
  group_by(category) %>%
  mutate (rev_loc_order = (max(local_order, na.rm = T) + 1) - local_order,
          category_indexed = paste0(category, "___", rev_loc_order),
          category_indexed_to_mask =paste0(category, "___", (rev_loc_order + 1)) ) %>%
  as.data.frame() %>%
  select(-rev_loc_order)
tree_specs_df$category_indexed[is.na(tree_specs_df$local_order)] <- NA
tree_specs_df$category_indexed_to_mask[is.na(tree_specs_df$local_order)] <- NA
# category_indexed will contain the names of the different resolution levels for each category
# category_indexed_to_mask is the category which will be masked after the merge described in
# category_indexed. For example category_indexed = race_3 means that there are 3 levels in this category
# and that race_4 will be masked if we want to use race_3



# CHECKS


# 1. category levels in data should be present in specs
cat_levels_in_specs <- tree_specs_df$category_level_label[is.na(tree_specs_df$global_order)]
cat_levels_in_data <- c()
for (cat in key_var_cats) {
  cat_levels_in_data <- c(cat_levels_in_data, processed_df[,cat]) %>% unique
}

if (!all(cat_levels_in_data %in% cat_levels_in_specs)) {
  stop("The category levels defined in settings are not the same as the ones found in the data!")
}


# 2. category merges should equal to the number of levels minus the number of categories
# For example, a category with initial 8 levels will need 7 binary merges to be reduced to 1 level
cat_merges_in_specs <- tree_specs_df$category_level_code[!is.na(tree_specs_df$global_order)]
n_categories <- unique(tree_specs_df$category) %>% length
if (length(cat_merges_in_specs) !=  (length(cat_levels_in_specs) - n_categories)) {
  stop("The category levels defined in settings are not the same as the ones found in the data!")
}


# 3. the order values equal to the number of levels minus 1
vec_x <- 1:length(cat_merges_in_specs)
vec_y <- min(tree_specs_df$global_order, na.rm = T):max(tree_specs_df$global_order, na.rm = T)
if (!all(vec_x == vec_y)) {
  stop("The the order values in the settings are wrong.")
}


# 4. all merged category levels should be defined as parents
if (length(setdiff(cat_merges_in_specs, unique(tree_specs_df$category_level_parent))) > 0) {
  stop("Discrepancy betweeen merged category codes and parent description codes")
}


# 5. There should be exactly two repetitions of the same parent
buff_df <- tree_specs_df %>% group_by(category, category_level_parent) %>% summarise(count_n = n())
buff_df <- buff_df[!is.na(buff_df$category_level_parent),]
allcounts <- buff_df$count_n %>% unique
if (allcounts != 2 | length(allcounts) != 1) {
  stop("The number of parent levels is different from two. Only binary merges are allowed.")
}


# 6. There should be no repetitions of category codes
buff_df <- tree_specs_df %>% group_by(category, category_level_code) %>% summarise(count_n = n())
if (sum(buff_df$count_n) != nrow(buff_df)) {
  stop("There are repetitions in the category level codes in the three scpecs.")
}


# 7. There should be no repetitions of category labels
buff_df <- tree_specs_df %>% group_by(category, category_level_label) %>% summarise(count_n = n())
if (sum(buff_df$count_n) != nrow(buff_df)) {
  stop("There are repetitions in the category level labels in the three scpecs.")
}



# ADD DIFFERENT RESOLUTIONS
#create indexed category names and keep a vector with the new names
old_names <- names(processed_df)
processed_df <-  created_indexed_categories(processed_df, tree_specs_df, key_var_cats)
indexed_cat_name <- setdiff(names(processed_df), old_names)



### MASKING
# keep a dataframe which shows which category needs to be masked

rbound_df <- data.frame()

for (school in unique(processed_df$org_id)) {
  # compute current resoultion for each of the key variables (demographics)
  buff_df <- processed_df[processed_df$org_id %in% school,]

  # remove duplciated entries from the data frame, so the counts will be based on
  # unique participants only. Notice that this assumes that there are single values
  # for each of the key variables for a given participant (this assumption is tested
  # at earlier stage). If repeated ids are not an issue, you can use buff_df instead of buff_df_unique

  buff_df_unique <- buff_df[!duplicated(buff_df$hashed_id),]

  indexed_categories_to_mask <- compute_excluded_categories(buff_df_unique, tree_specs_df, key_var_cats, MIN_CELL, "n/a")

  # mask category resolutions which are beyond the threshold
  cats_to_mask <- c(key_var_cats, indexed_categories_to_mask)
  buff_df[,cats_to_mask] <- "__masked__"
  rbound_df <- bind_rows(rbound_df, buff_df)
}

# sort alphabetically key variable columns
ordered_cols <- c(key_var_cats, indexed_cat_name)
rbound_df <-  reorder_cols(rbound_df, ordered_cols)
#View(rbound_df)

# replace NA with "n/a" for consistency
cats_to_replace <- ordered_cols
rbound_df[, cats_to_replace] <- rbound_df[,cats_to_replace] %>% lapply(.,function(x) replace_na(x,"n/a"))

# NO NEED TO DO THIS - in fact, probably clearer if we don't:
# remove columns with single values (e.g. NAs or "ALL"), so high resolution
# masked columns will not be present
# constant_cols <- rbound_df %>% apply(., 2, function(x) (length(unique(x)) == 1) )
# non_constant_cols <- names(rbound_df)[!constant_cols]
# # add survey cols in case they were excluded
# cols_to_include <- c(non_constant_cols, survey_cols) %>% unique
# rbound_df <- rbound_df[,cols_to_include]

# remove original key columns, because they are totally masked no matter what
rbound_df <- select(rbound_df, -key_var_cats)

# create a log file
cats_for_log_file <- ordered_cols[ordered_cols %in% names(rbound_df)]
cats_for_log_file <- c(cats_for_log_file, "org_id")

buff_df <- rbound_df[!duplicated(rbound_df$hashed_id),cats_for_log_file]
buff_df$constant <- 1

melted_buff_df <- buff_df %>% melt(., id.vars = c("org_id"), value = constant)
melted_buff_df$value[melted_buff_df$value != "n/a"] <- "non-missing"
melted_buff_df$value[melted_buff_df$value == "n/a"] <- "missing"
log_df <- melted_buff_df %>%
  group_by(org_id, variable, value) %>%
  summarise(count_n = n())
log_df$comb_name <- paste0(log_df$variable, ":", log_df$value)
log_df <- log_df %>% dcast(org_id ~ comb_name, value.var = "count_n")
LOG_PATH <- gsub(SALT_FILE_NAME, "log_df.csv", SALT_PATH)
write.csv(log_df, LOG_PATH, row.names = F)

# remove "report_" from var names
names(rbound_df) <- names(rbound_df) %>% gsub("report_", "",.)


OUTPUT_PATH <- gsub(SALT_FILE_NAME, OUTPUT_FILE_NAME, SALT_PATH)
write.csv(
  rbound_df,
  OUTPUT_PATH,
  row.names=FALSE
)



##### REVIEWING RESULTS OF K-ANONYMIZATION WITH EP DATA

# which granularities do I choose? what do I do with the result?
# race___3 and gender___3 are the standard two-bucket vars (with NA)
table(rbound_df$gender___3)
table(rbound_df$race___3)
table(rbound_df$gender___3, rbound_df$race___3)
# Lots of data still masked! Why?
# Because many teams have a few tiny demographic subgroups that force extensive masking.

# Example:
# table(rbound_df$org_id, rbound_df$gender___3)
test_df <- filter(processed_df, org_id %in% "Team_ihJh6vK9sursQEYr") %>%
           group_by(hashed_id) %>%
           filter(StartDate %in% min(StartDate))
table(test_df$gender, test_df$race)
# This team only has a few tiny demog subgroups that force more extensive anonymization.
# Second example:
test_df <- filter(processed_df, org_id %in% "Team_eMJCXlafjT8romUS") %>%
  group_by(hashed_id) %>%
  filter(StartDate %in% min(StartDate))
table(test_df$gender, test_df$race)
# This team is the same, but it just mostly has NA demogs.

# How many teams are smaller than 12 people and will be thus masked by two-bucket vars regardless?
# input_df %>%
#   group_by(team_id) %>%
#   summarise(n_people = length(unique(participant_id))) %>%
#   group_by(n_people) %>%
#   summarise(n_teams = n())
# only 5 teams with a total of 8 people.


# Proposed filter: for step 8, if the step is actually needed then just drop the <5 group.
# By this point, gender is totally masked anyway.
# in other words, if you tag each student as marg/non-marg race and ignore gender,
# and there's <5 of a group in a team, then you drop that group so that you know the rest instead of masking.
# How much data will this save? Ignore NA people.
test <- input_df %>%
  group_by(userID) %>%
  summarise(race = first(race),
            team_id = first(team_id)) %>%
  filter(!race %in% "n/a") %>%
  mutate(marg = !race %in% c("White", "Asian or PacIsl"))
groups <- test %>%
  group_by(team_id, marg) %>%
  summarise(n_students = n(),
            too_small = n_students <= 5)
teams_w_too_small_groups <- groups[groups$too_small, "team_id"] %>% unique()
# How many people would we drop?
sum(groups[groups$too_small, "n_students"]) # 14 people
# How many people would we get to preserve marg information?
# i.e. people in teams w too-small groups who are not themselves in a too-small group
sum(groups[(groups$team_id %in% teams_w_too_small_groups$team_id) & (!groups$too_small), "n_students"]) # 266 people
sum(groups$n_students) # out of how many? 5104. Maybe not worth it for the complication of explaining/defending.



