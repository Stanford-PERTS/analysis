# In this script we will k-anonymize the EP 2018-2019 data

# The current version of the script executes the following steps

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
## 2 - 2018-2019 pre-k-anon cleaned data.csv - comes from a crypt folder
## helper_functions.csv - comes from the k_anonymization folder in analysis

# output files:
## 3 - 2018-2019 k-anon cleaned data.csv
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
INPUT_FILE_NAME <-  "2 - 2018-2019 pre-k-anon cleaned data.csv"
OUTPUT_FILE_NAME <- "3 - 2018-2019 k-anon cleaned data.csv"


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

############################ READ DATA

# the data input comes from the report generation script. This is the data that will
# be anonymized.
input_path <- util.find_crypt_paths(list(f1 = INPUT_FILE_NAME)) %>%  unlist
input_df <- read.csv(input_path, stringsAsFactors = FALSE)

# read salt file
SALT_PATH <- util.find_crypt_paths(list(f1 = SALT_FILE_NAME)) %>%  unlist
source(SALT_PATH)


############################ DATASET SPECIFIC ADJUSTMENTS

demogr_vars <- c("race", "gender") # these are for report tables only, not used in anonymization
key_var_cats <- demogr_vars
input_df$org_id <- input_df$team_id
input_df$participant_id <- input_df$student_id
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

# change "n/a" back to NA for all granularities of k-anon vars
for(var in cats_for_log_file) {
  rbound_df[rbound_df[, var] %in% "n/a", var] <- NA
}

# drop unneeded columns
rbound_df <- select(rbound_df,
                    -org_id.1,
                    -hashed_id,
                    -participant_id,
                    -org_id,
                    -row,
                    -X)

# save
OUTPUT_PATH <- gsub(SALT_FILE_NAME, OUTPUT_FILE_NAME, SALT_PATH)
write.csv(
  rbound_df,
  OUTPUT_PATH,
  row.names=FALSE
)



##### REVIEWING RESULTS OF K-ANONYMIZATION

# which granularities do I choose? what do I do with the result?
# race___3 and gender___3 are the standard two-bucket vars (with NA)
# table(rbound_df$gender___3)
# table(rbound_df$race___3)
# table(rbound_df$gender___3, rbound_df$race___3)
