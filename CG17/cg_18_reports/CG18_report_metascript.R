# WARNING!!! This script is working, but it is hard to work with. There was a mistake with
# participation codes, so some schools participated in CG18 Qualtrics survey using their 2017 code
# (pure 2017), others with 2018 code (pure 2018), and a third type participated with both codes (mixed group)
# In addtion, the pure 2017 group has two subtypes, one subtype are those who registered for 2018, and another subtype
# are those who did not not register for 2018, and had only their 2017 account. For the second subtype we are not doing
# anything, unless Nash says something different. To handle these issues, the code is interupted at several places
# by chunks which deal with survey_id and project_cohort_id reassignment. The general pattern is that for the cg17 codes
# we replace survey_id and project_cohort_id with the cg18 values for the same organization.
#
#There is additional issue which you might want to solve if running the code again.The metascript creates a ggplot
# object which is then printed in the Rmd. It came out that this is not a graphica object, but just a code, which
# then is executed in the Rmd. This adds about 30s per school, which could be a lot of time. The way to fix this is
# to save the graph as png, and then print it in the Rmd for each call.
SINGLE_RUN <- FALSE
SAMPLE_RUN <- FALSE
PDF <- FALSE
PRODUCTION_QUALITY <- FALSE
READ_DATE_NEPTUNE <- Sys.Date() # this is the date you want to append to the path for the survey data when you're reading it in.
READ_DATE_QUALTRICS <- Sys.Date()
# Feel free to change it if it's not today.
# for data pulls prior to 2018, set READ_DATE to empty string and delete the underscores from the path labels.
# (complete = "CG17_OFFICIAL_1_" %+% READ_DATE %+% ".csv") becomes complete = "CG17_OFFICIAL_1" %+% READ_DATE %+% ".csv"
#READ_DATE <- as.Date("2019-04-12")
WRITE_DATE <- Sys.Date() # this is the date you want appended to saved, processed data.
# Feel free to change it if it's not today.
#WRITE_DATE <- as.Date("2019-04-15")
YEAR <- "2019 - 2020"
REPORT_COHORT_LABEL <- "Spring 2020" # this goes on the cover page of the report
PLATFORM_COHORT_LABEL <- "2019"

PREVIOUS_REPORT_DATE <-  "2019-10-15" # colleges which do not have participants
# after this date will not be included

ANONYMOUS <- FALSE # creates anonymized reports only
ANONYMOUS_NAME <- "Name Redacted"

MIN_GOOD_GMS <- 4.1 #(it should be higher than 4)
N_COMPLETERS_THRESHOLD <- 30 # colleges with fewer participants will not get results section



# use the crypt called CG17_large.vc
library("stringi")
library("knitr")
library("tidyr")
library("lubridate")
library('readr')
library('stats')
library('dplyr')
library('stringr')
library('reshape2')
library('ggplot2')
library('lubridate')
library('metaviz')
library('xtable')

#'%>%' <- dplyr::'%>%'

# source gymnast functions
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"
source(paste0(github_base_path,"R/util_legacy.R"))
source(paste0(github_base_path,"R/util_qualtrics_cleaning.R"))
source(paste0(github_base_path,"R/util_graphing.R"))
#source(paste0(github_base_path,"R/util_scale_computation.R"))

#source(paste0(github_base_path,"R/util_data_summaries.R"), local = TRUE)
# Note — this script will only work properly using the crypt called CG17_large.vc




REPO_PARENT <- "~/Sites/"
#RMD_BASE_PATH <- REPO_PARENT %+% "analysis/CG17/site_reports/"
RMD_BASE_PATH <- REPO_PARENT %+% "analysis/CG17/cg_18_reports/"
REPORT_BASE_PATH <- RMD_BASE_PATH %+% "reports"
DROPBOX_DIR <- "~/Dropbox (PERTS)/"
MIN_CELL <- 5

DRIVER_GRAPH_COLORS <- c("#155994","#95D4F6")
OUTCOME_COLORS <- c("#155994", "#3faeeb")
subset_groups <- c("gender_simplified", "race_simplified")

# source helper functions from the engagement project
EP_HELPER_PATH <- REPO_PARENT %+% "analysis/engagement_diagnostic/engagement_helpers.R"
tryCatch({
    source(EP_HELPER_PATH)
    rm(driver_desc)
}, error = function(e){
    stop("Could not load engagement helpers. Check " %+%
             EP_HELPER_PATH %+% " and try again.")
})
source(RMD_BASE_PATH %+% "CG18_helpers.R")
#source(RMD_BASE_PATH %+% "CG18_dynamic_text.R")
source(REPO_PARENT %+% "analysis/common/render_jurassic_pdf.R")
source(REPO_PARENT %+% "gymnast/R/util_scale_computation.R")


# probability threshold (what p-value do you want to use as a cutoff for "it worked!" language?)
PROBABILITY_THRESHOLD <- .05

##############################################################################
##############################################################################
#################### Clean & process data ####################################


# set stringsAsFactors to FALSE, and add any other global R settings
options(stringsAsFactors = FALSE)

# set results to 'asis' for all chunks
# (can also add any global rmarkdown settings here)
opts_chunk$set(results = 'asis', warning = FALSE, message = FALSE)

## Local helper functions
# lh.helper_function <- function()
wrap_text <- function(text, width=35) {
    wtext <- paste( strwrap(text,width=width), collapse=" \n ")
    return(wtext)
}

## Load data

s_raw_paths <- util.find_crypt_paths(
    list(
        # survey data come from Qualtrics. You have to update the year values each Fall.
        complete = "CG19_OFFICIAL_1_" %+% READ_DATE_QUALTRICS %+% ".csv",
        partial = "CG19_OFFICIAL_1-Responses in Progress_" %+% READ_DATE_QUALTRICS %+% ".csv",

        # quotes must be generated partially by hand
        quotes = "edited_quotes.csv",
        new_quotes = "new_quotes_edited.csv",

        # this is deprecated I believe
        test_cutoffs = "test_date_cutoffs.csv",

        # the project_cohort and survey_ tables both come from the Neptune system
        pc = "project_cohort_" %+% READ_DATE_NEPTUNE %+% ".csv",
        sids = "survey_" %+% READ_DATE_NEPTUNE %+% ".csv",
        # pc = "project_cohort_2019-04-12.csv",
        # sids = "survey_2019-04-03.csv",

        # I assume this hasn't changed since 2018, so no need to update the year here
        all_items = "CG18_variable_descriptions.csv"
    )
)

# check for missing files
missing_files <- lapply(s_raw_paths, function (x) length(x) == 0) %>% unlist
if(any(missing_files)){
  stop("The following files could not be loaded: " %+% paste0(names(s_raw_paths)[missing_files], collapse = ", "))
}

s_complete <- read.csv(s_raw_paths$complete) %>%
  mutate(in_progress = NA)
s_partial <- read.csv(s_raw_paths$partial)

d_initial <- qc.clean_qualtrics(s_complete) %>%
    qc.rbind_inprogress(s_partial, .)

# note that the test_cutoffs thing is deprecated. Basically in 2018, Nash gave schools a "testing window"
# where they could review the survey. So we wanted to delete those resposnes from those schools from the
# final report. Now, all those responses will have dates in 2018 so the expected behavior is that nothing
# will get filtered.
test_cutoffs <- read.csv(s_raw_paths$test_cutoffs)
pc <- read.csv(s_raw_paths$pc)
sids <- read.csv(s_raw_paths$sids)

# in 2018 we needed to filter to include. But in 2019 we're not doing that so it's commented out
d_quotes_old <- read.csv(s_raw_paths$quotes, stringsAsFactors = FALSE)
sids_for_quotes <- dplyr::filter(sids, cohort_label %in% "2019", program_label %in% "cg17") %>%
  select(survey_id, organization_id)
d_quotes_new <- read.csv(s_raw_paths$new_quotes, stringsAsFactors = FALSE) %>%
  dplyr::filter(included == "1") %>%
  rename(value = quotes)
d_quotes_new_merged <- d_quotes_new %>%
  left_join(., sids_for_quotes, by = "organization_id")
nrow(d_quotes_new) == nrow(d_quotes_new_merged)

d_quotes <- util.rbind_intersection(list(d_quotes_old, d_quotes_new_merged))



REPO <- "~/Sites/analysis/"
all_items <- read.csv(s_raw_paths$all_items, sep = "\t")

## (note that we will skip de-identification because there is no data output
# in this script, and no individual identifiable data in the dataset (except
# maybe student survey responses))


### Clean columns

#### Standardize column names

# define d_rn (will stand for d, renamed)
d_rn <- d_initial




#### Standardize column values
# d_rc will stand for "d, recoded"
d_rc <- d_rn


# !!! More than one name per organization: The name of organization is mutable,
# which leads to multiple names recorded by qulatrics corresponding to single organization.
# There should be one name per organization_id

test_df <- d_initial %>%
  group_by(organization_id) %>%
  summarise(
    ids = paste0(unique(organization_name), collapse = "; "),
    ids_n = length(unique(organization_name)),
    n =n()
  ) %>%
  dplyr::filter(ids_n > 1)
nrow(test_df) # if the number of rows is greater than zero, then there are multiple org names per organization_id,
# and you may have to fix things by hand; e.g., by doing the following:
# recode_df_codes = data.frame(
#   organization_id = c("Organization_1tBXorB5", "Organization_Fqns4Ali", "Organization_khZz1lHf" ),
#   organization_name = c("Pima Community College", "Delaware Technical Community College", "Berea College")
# )
# id <- match(d_rc$organization_id, recode_df_codes$organization_id)
# recode_df <- recode_df_codes[id,]
#
# d_rc$new_names <- ifelse(!is.na(recode_df$organization_id), recode_df$organization_name, d_rc$organization_name)


# check for errors
# with(d_rc %>% dplyr::filter(organization_id %in% recode_df_codes$organization_id),
#      table(organization_name, new_names))
#d_rc$organization_name[d_rc$organization_name != d_rc$new_names] %>% table(., exclude = NULL)

# looks good
# d_rc$organization_name <- d_rc$new_names
# d_rc$new_names <- NULL

# check if there are still schools that need renaming
test_df <- d_rc %>% group_by(organization_id) %>%
  summarise(ids = paste0(unique(organization_name), collapse = "; "),
            ids_n = length(unique(organization_name)),
            n =n())
if(max(test_df$ids_n) > 1) {
    stop("There are organizations with more than one name!")
} else{
    util.passed("Just one organization name per organization_id. Nice!!")
}




# In this section, we will also determine for each participant
# which race question they answered, and keep their response to that question
# in order to combine both versions into a single variable about race.

race_values_revised <- c("AA", "Asian", "Lat", "ME", "Nat", "PI", "Eu", "Oth")


old_race_cols_revised <- names(d_rc)[grep("race_revised\\.[0-9]+$", names(d_rc))]
new_race_cols_revised <- gsub("\\.[0-9]+", "", old_race_cols_revised) %+% "_" %+% race_values_revised

names(d_rc) <- util.recode(names(d_rc), old_race_cols_revised, new_race_cols_revised)



# compute the simplified categories
d_rc$race_simplified <- NA
d_rc$race_simplified[d_rc$race_revised_Asian | d_rc$race_revised_Eu] <- "White/Asian"
d_rc$race_simplified[d_rc$race_revised_AA | d_rc$race_revised_Lat | d_rc$race_revised_Nat | d_rc$race_revised_Oth |
                       d_rc$race_revised_PI | d_rc$race_revised_ME] <- "Blk/Nat/Lat/Oth"
d_rc$race_disadv <- d_rc$race_simplified %in% "Blk/Nat/Lat/Oth"
d_rc$race_disadv[is.na(d_rc$race_simplified)] <- NA
#table(d_rc$race_simplified , exclude = NULL)


# recode treatment variable
# 1 is post treatment, so the directionality of the test will makes sense,
# where positive relationship between dv and treatment means increase in the dv
d_rc$post_treatment <- util.recode(d_rc$q_block_position, c("pre","post"), c(0,1))




new_student_key <- list(old = c(1,2,3), new = c("incoming", "new", "returning"))
d_rc$new_student <- util.recode(
    d_rc$new_student,
    new_student_key$old,
    new_student_key$new
)


gender_key <- list(old = c(1,2,3), new = c("Male", "Female", "Nonbinary"))
d_rc$gender <- util.recode(d_rc$gender, gender_key$old, gender_key$new)

d_rc$gender_simplified <- util.recode(d_rc$gender, "Nonbinary", "Female")
d_rc$female <- d_rc$gender_simplified %in% "Female"
d_rc$female[is.na(d_rc$gender)] <- NA
#table(d_rc$gender_simplified, exclude = NULL)
#table(d_rc$female, exclude = NULL)




activity2_key <- list(
    old = c(1, 2, 3, 4, 5),
    new = c("read out loud", "handout", "discussed only", "no instructions", "dont remember")
)

d_rc$activity_2 <- util.recode(d_rc$activity_2, activity2_key$old, activity2_key$new)

student_loc_key <- list(old = c(1,2,3), new = c("homework", "in class", "homework"))
d_rc$student_loc <- util.recode(d_rc$student_loc, student_loc_key$old, student_loc_key$new)



d_rc$organization_name_wrapped <- sapply(d_rc$organization_name, function(x) wrap_text(x, 15))

### Clean rows

#### Remove testers (d, no testers)
d_nt <- merge(
  d_rc,
  test_cutoffs,
  by.x = "organization_id",
  by.y = "Organization_ID",
  all.x = TRUE,
  all.y = FALSE)
if(!nrow(d_nt) == nrow(d_rc)){
    stop("Merging test cutoff dates resulted in rows being added or dropped from the data. " %+%
             "Investigate before proceeding.")
}
initial_d_nt_nrow <- nrow(d_nt)
# most values will be NA
d_nt <- d_nt[is.na(d_nt$last_testing_date) | d_nt$StartDate > d_nt$last_testing_date,]

# remove Arnrow's testers too
d_nt <- d_nt[!d_nt$organization_name %in% "Arnrow Test U", ]

# remove participants marked as "tester" in the Qualtrics data
d_nt <- d_nt[util.is_blank(d_nt$testing), ]

d_nt_including_incomplete <- d_nt # keep this for some of the attrition effect tests
# Remove incomplete cases
util.is_blank(d_nt$Q6.1) %>% table
(d_nt$progress != 100) %>% table
#d_nt <- d_nt %>% dplyr::filter(!util.is_blank(Q6.1)) # this question is at the end of survey
# if I use Q6.1 instead of progress, I looses about 2200 participants
d_nt <- d_nt %>% dplyr::filter(d_nt$progress %in% c(100)) # this is considered completed activity by design


#### Logic for handling duplicated index values

# the index in this dataset is participant_id
# 1. Remove all rows where participant ID is NA
# 2. Where IDs are duplicated, take each participant's most recent entry

# d_dh stands for d, duplicates handled
d_dh <- d_nt
pid_is_blank <- util.is_blank(d_dh$participant_id)
util.passed(sum(pid_is_blank) %+% " rows removed with blank participant ids. " %+%
                "These likely correspond to testers.")
d_dh <- d_dh[!pid_is_blank, ]

d_dh <- d_dh %>%
    group_by(participant_id) %>%
    arrange(StartDate) %>%
    mutate(id_instance = 1:n(),
           max_instances = n(),
           keep = id_instance == max_instances)

n_dropped <- sum(!d_dh$keep)
d_dh <- d_dh[d_dh$keep, ]

util.passed("Records corresponding to " %+% n_dropped %+% " rows " %+%
                "were removed as duplicates. (" %+%
                as.character(round(n_dropped/nrow(d_dh), 2)*100) %+%
                "% of data.) Where participant IDs were duplicated, " %+%
                "the chronically last entry was always kept.")

util.passed("After handling duplicates, " %+% nrow(d_dh) %+%
                " rows remain in the final data.frame.")

if(any(duplicated(d_dh$participant_id))){
    util.warn("duplicates remain in object d_dh after duplicates " %+%
                  "should have been cleaned. Investigate further.")
}

## compute scales &f change scores
d_scales <- d_dh
# the values for this variable need to be recoded, this should be 6-point scale, not 7
d_scales$challenge_seeking <- d_scales$challenge_seeking %>% util.recode(c(6,7), c(5,6))

names(d_scales)[grep("gms", names(d_scales))]

d_scales <- sc.append_scales(survey_df =d_scales,
                             scale_variables_table = all_items,
                             add_z_score = TRUE)

# create fixed, mixed, and growth variables
d_scales$gms_cat <- NA
d_scales$gms_cat[d_scales$gms <= 2] <- "fixed"
d_scales$gms_cat[d_scales$gms <= 4 & !d_scales$gms_cat %in% "fixed"] <- "mixed"
d_scales$gms_cat[d_scales$gms > 4] <- "growth"
d_scales$gms_growth <- d_scales$gms_cat == "growth"

### merge in project-cohort info from Triton. Note that we will begin by
### merging the survey table to the Qualtrics table on survey_id. Per the documentation
### here (https://docs.google.com/presentation/d/1tEZ_mlI11xyo8g9Kr8l7u8uXyZfJqB_mmnWdW52eJv8/edit#slide=id.g11ba45caf1_0_0),
### the survey field is the most precise way to match survey records with other info in the database.
### Note that using another variable, like organization_id, will get you in trouble because orgs
### can have multiple program_ids. Starting by merging in the survey table will never lead you astray.

# the 2018 version had different column names. These are commented out but left for posterity.
# sids_rn <- sids %>%
#   select(survey_id, project_cohort_id, program_label,survey.cohort_label,
#                            organization_id, cohort_label) %>%
#   rename(organization_id_sid = organization_id) %>%
#   dplyr::filter(program_label == "cg17") # this icludes both cg17 and cg18

# this renaming preserves the original column names from the 2018 data (even though I'd rather keep Chris's scheme going forward)
sids_rn <- sids %>%
    select(survey_id, project_cohort_id, program_label, cohort_label, organization_id) %>%
    rename(organization_id_sid = organization_id)
#table(sids_rn$cohort_label)
# 2017_fall 2017_spring        2018        2019        2020
# 66         785         953        1490         103


sids_rc <- sids_rn
#unique(d_scales$survey_id) %in% sids_rc$survey_id %>% table
# do a left join to keep rows from data, but drop rows from sids (probably schools that didn't "matriculate")

# looks like this object is used for error checking later on
sids_rc_2019_only <- sids_rc %>% dplyr::filter(cohort_label == PLATFORM_COHORT_LABEL)

d_merged <- merge(d_scales, sids_rc, by = "survey_id", all.x = TRUE, all.y = FALSE)
if(!nrow(d_merged) == nrow(d_scales)){
    stop("merging project-cohort info resulted in rows being added or dropped from data.")
}

# I'm going to go ahead and guess that it's ok to remove!

d_merged <- d_merged[!d_merged$organization_id %in% "Organization_fake", ]
# nrow(d_merged) # was 52866, now 52865. Only lost one row.

# How many correspond to the unmatched org?
d_merged %>%
  dplyr::filter(organization_id %in% "Organization_Qmy5k2qa") %>%
  nrow()
# in Spring 2020, I got the following:
# Error: The following organization_ids could not be matched to project_cohort_ids: Organization_Qmy5k2qa
# just one row. Get rid of it.
d_merged <- d_merged %>% dplyr::filter(!organization_id %in% "Organization_Qmy5k2qa")

if(any(util.is_blank(d_merged$project_cohort_id))){
    missing_orgs <- d_merged$organization_id[util.is_blank(d_merged$project_cohort_id)] %>% unique
    stop("The following organization_ids could not be matched to project_cohort_ids: " %+%
             paste0(missing_orgs, collapse = ", "))
}



# How many organization_id values from the survey are blank.

d_merged %>%
  dplyr::filter(util.is_blank(organization_id)) %>%
  nrow()

# 9 of them. Just remove them.
d_merged <- d_merged[!util.is_blank(d_merged$organization_id), ]

# now check that all the org_id values match across survey and platform data
check_orgs <- d_merged %>%
    select(organization_id, organization_id_sid) %>%
    mutate(matching_orgs = organization_id == organization_id_sid)

if(!all(check_orgs$matching_orgs)){
    stop("some organization_id values from the survey do not match the platform. Something's wrong.")
} else{
    util.passed("All organization values from the survey match those from the platform. Nice!")
}

# check for blank organization names
if(any(util.is_blank(d_merged$organization_name))){
    stop("Blank organization names were found. Something's wrong.")
} else{
    util.passed("No blank organization names were found. Nice!")
}


# # Create / Update quotes (only once, comment out when done)
# #
# # all_quotes_df <- sample_quotes(d_merged, "Q4.51", "organization_id") # this removes excluded orgs
# all_quotes_df <- sample_quotes(d_initial, "Q4.51", "organization_id")
# # use all orgs, in case you need quotes from the excluded ones
#
# # remove quotes for previously included orgs
# (unique(d_quotes$organization_id) %in% all_quotes_df$organization_id) %>% table
# new_quotes_df  <- all_quotes_df %>% dplyr::filter(!organization_id %in% d_quotes$organization_id)
# new_quotes_df$organization_id %>% unique
#
# f_path <- gsub("CG18_raw_quotes\\(cleaned\\).csv", "CG18_raw_quotes_new.csv", s_raw_paths$quotes)
# write.csv(new_quotes_df, f_path, row.names = FALSE)
#
#
#rbind old and new quotes (this should be done only once per generation)
# f_path_old <- "/Volumes/NO NAME/CG18/CG18_cleaned_quotes_old.csv"
# f_path_new <- "/Volumes/NO NAME/CG18/CG18_cleaned_quotes_new.csv"
# f_path_all <- "/Volumes/NO NAME/CG18/CG18_cleaned_quotes_all.csv"
# old_df <- read.csv(f_path_old)
#
# new_df <- read.csv(f_path_new)
#
# new_df <- new_df %>% dplyr::rename(
#   value = quotes,
#   row_number_index = row_numb,
#   include = included
# )
# all_quotes <- rbind(old_df, new_df)
# write.csv(all_quotes, f_path_all, row.names = FALSE)



# define final wide dataset, data_wide
data_wide <- d_merged

########################################################################
################## Create melted dataset for pre/post analysis #########


data_wide$pre_post_label <- factor(
  data_wide$post_treatment,
    levels = c("0", "1"),
    labels = c("Before Program", "After Program")
)

#table(data_wide$pre_post_label, useNA = "ifany")
# Fall 2019:
# # Before Program  After Program
# 26389          26466

#####################################################################
###### Write unit-level data to crypt for further analysis. #########

CRYPT <- gsub("CG19_OFFICIAL_1_" %+% READ_DATE_QUALTRICS %+% ".csv", "", s_raw_paths$complete)

if(dir.exists(CRYPT)){
write.csv(data_wide, CRYPT %+% "CG_data_wide_" %+% WRITE_DATE %+% ".csv")
} else{
    util.warn("Directory " %+% CRYPT %+% " does not exist or is not mounted. " %+%
                  "No data was saved to this crypt. If you want to save data, check your crypt " %+%
                  "settings and try again.")
}

#####################################################################
###### Identify present metrics, subset_groups, and outcomes ########



### Create wrapped question text
#all_items <- read.csv(items_path$items)
all_items$question_text_wrapped <- sapply(all_items$question_text, function(x) wrap_text(x))



report_sites <- unique(data_wide[c("organization_name", "organization_id",
                                   "organization_name_wrapped", "project_cohort_id", "survey_id")])

# check if there are duplciates
dupl_sites <- report_sites$survey_id[duplicated(report_sites$survey_id)]
if (length(dupl_sites) > 0) {
    stop("There are duplicated survey_ids.")
} else{
    util.passed("no duplicate survey_ids found in wide data. Nice!")
}
#report_sites[report_sites$survey_id %in% dupl_sites,] %>% head


#### Create stats and summaries objects for the Rmd
# OVERALL
overall_stats <- list()
overall_stats$n_overall <- nrow(data_wide)

overall_stats$overall_perc_good_pre <- data_wide %>%
  dplyr::filter(post_treatment == "0") %>%
  select(gms) %>%
  get_perc_good(.,min_good = MIN_GOOD_GMS, max_good = 6) %>%
  '[['(2) # this exgracts element in a list by index
overall_stats$overall_count_good_pre <- data_wide %>%
  dplyr::filter(post_treatment == "0") %>%
  select(gms) %>%
  get_perc_good(.,min_good = MIN_GOOD_GMS, max_good = 6) %>%
  '[['(1) # this exgracts element in a list by index

overall_stats$overall_perc_good_post <- data_wide %>%
  dplyr::filter(post_treatment == "1") %>%
  select(gms) %>%
  get_perc_good(.,min_good = MIN_GOOD_GMS, max_good = 6) %>%
  '[['(2) # this exgracts element in a list by index
overall_stats$overall_count_good_post <- data_wide %>%
  dplyr::filter(post_treatment == "1") %>%
  select(gms) %>%
  get_perc_good(.,min_good = MIN_GOOD_GMS, max_good = 6) %>%
  '[['(1) # this exgracts element in a list by index

overall_stats$lmer_stats <- get_lmer_stats(in_df = data_wide, dv_var = "gms", iv_var = "post_treatment")

# overall graph

#### attrition effects
overall_stats$attrition_stats <- list()

d_nt_including_incomplete$gms <- d_nt_including_incomplete %>%
  select(gms_1, gms_5) %>%
  rowMeans(., na.rm = TRUE)

df_perc_including_incomplete <- d_nt_including_incomplete %>% group_by(post_treatment) %>%
  summarise(
    count_present = sum(!is.na(gms)),
    percent_present = sum(!is.na(gms))/n()*100
  )  %>%
  dplyr::filter(!util.is_blank(post_treatment))
df_perc_complete <- data_wide %>% group_by(post_treatment) %>%
  summarise(
    count_present = sum(!is.na(gms)),
    percent_present = sum(!is.na(gms))/n()*100
  )  %>%
  dplyr::filter(!util.is_blank(post_treatment))


# complete and incomplete (raw)
overall_stats$attrition_stats$raw_pre_perc <-
  df_perc_including_incomplete %>%
  dplyr::filter(post_treatment == 0) %>%
  dplyr::select(percent_present) %>% unlist %>% unname %>% round(.,2)

overall_stats$attrition_stats$raw_post_perc <-
  df_perc_including_incomplete %>%
  dplyr::filter(post_treatment == 1) %>%
  dplyr::select(percent_present) %>% unlist %>% unname %>% round(.,2)

overall_stats$attrition_stats$raw_chi_sq_est <-
  df_perc_including_incomplete$count_present %>%
  chisq.test() %>%
  '[['("statistic") %>%
  round(.,2)

overall_stats$attrition_stats$raw_chi_sq_p.value <-
  df_perc_including_incomplete$count_present %>%
  chisq.test() %>%
  '[['("p.value") %>%
  round(.,3) %>%
  p_to_text



#### complete


overall_stats$attrition_stats$compl_pre_perc <-
  df_perc_complete %>%
  dplyr::filter(post_treatment == 0) %>%
  dplyr::select(percent_present) %>% unlist %>% unname %>% round(.,2)

overall_stats$attrition_stats$compl_post_perc <-
  df_perc_complete %>%
  dplyr::filter(post_treatment == 1) %>%
  dplyr::select(percent_present) %>% unlist %>% unname %>% round(.,2)

overall_stats$attrition_stats$compl_chi_sq_est <-
  df_perc_complete$count_present %>%
  chisq.test() %>%
  '[['("statistic") %>%
  round(.,2)

overall_stats$attrition_stats$compl_chi_sq_p.value <-
  df_perc_complete$count_present %>%
  chisq.test() %>%
  '[['("p.value") %>%
  round(.,3) %>%
  p_to_text

#####
# overall_stats
# specific_school_stats




#SCHOOL SPECIFIC
specific_school_stats_df <- data_wide %>%
  group_by(survey_id) %>%
  summarise(
      project_cohort_id = unique(project_cohort_id),
      school_name = unique(organization_name),
      organization_id = unique(organization_id),
      cohort_label = paste0(unique(cohort_label), collapse = ", "),
      n = n()
  ) %>%
  dplyr::filter(cohort_label %in% PLATFORM_COHORT_LABEL)

buff_df <- data_wide %>%
    group_by(survey_id, post_treatment) %>%
    summarise(perc_good = get_perc_good(gms, MIN_GOOD_GMS, 6)$perc_good) %>%
    dcast(survey_id ~ post_treatment) %>%
    rename(pre_perc_good = '0', post_perc_good = '1') %>%
    mutate(pct_good_change = abs(post_perc_good - pre_perc_good) %>% round(.,2))

specific_school_stats_df <- specific_school_stats_df %>%
  merge(
    .,
    buff_df,
    by = "survey_id",
    all.x = TRUE,
    all.y = FALSE
  )

buff_df <- data_wide %>%
  group_by(survey_id, post_treatment) %>%
  summarise(
    n_ = n()
  ) %>% dcast(survey_id ~ post_treatment) %>%
  rename(n_pre = '0', n_post = '1')

specific_school_stats_df <- specific_school_stats_df %>%
  merge(
    .,
    buff_df,
    by = "survey_id",
    all.x = TRUE,
    all.y = FALSE
  )

# test if the school differs from the other schools in overall effects
specific_school_stats_df$b_effect <- NA
specific_school_stats_df$p_effect <- NA
specific_school_stats_df$t_effect <- NA
specific_school_stats_df$df_effect <- NA
specific_school_stats_df$effect_different_from_overall_b <- NA
specific_school_stats_df$effect_different_from_overall_b_lm <- NA
specific_school_stats_df$effect_different_from_overall_df <- NA
specific_school_stats_df$effect_different_from_overall_p <- NA
specific_school_stats_df$effect_different_from_overall_p_lm <- NA
specific_school_stats_df$effect_different_from_overall_t <- NA
specific_school_stats_df$effect_different_from_overall_se <- NA

specific_school_stats_df$mean_gms_pre <- NA
specific_school_stats_df$mean_gms_post <- NA

for (i in 1:nrow(specific_school_stats_df)) {
  print(i)
  srv_id <- specific_school_stats_df$survey_id[i]
  current_df <- data_wide
  if (sum(current_df$survey_id %in% srv_id) < N_COMPLETERS_THRESHOLD) { next }

  # first compute if there is difference between the current schools and the
  # the overall effect; a significant interaction term suggests difference

  current_df$this_school <- ifelse(current_df$survey_id == srv_id, 1, 0)
  lm_fit <- lm(gms ~ post_treatment*this_school, data = current_df)
  lmer_fit <- lmerTest::lmer(gms ~ post_treatment*this_school + (1|survey_id), data = current_df)

  specific_school_stats_df$effect_different_from_overall_b[i] <- summary(lmer_fit)$coefficients[4,1]
  specific_school_stats_df$effect_different_from_overall_se[i] <- summary(lmer_fit)$coefficients[4,2]
  specific_school_stats_df$effect_different_from_overall_df[i] <- summary(lmer_fit)$coefficients[4,3] %>% round(.,0)
  specific_school_stats_df$effect_different_from_overall_t[i] <- summary(lmer_fit)$coefficients[4,4]
  specific_school_stats_df$effect_different_from_overall_p[i] <- summary(lmer_fit)$coefficients[4,5]

  # keep an lm version just for comparison to the lmer output
  specific_school_stats_df$effect_different_from_overall_b_lm[i] <- summary(lm_fit)$coefficients[4,1]
  specific_school_stats_df$effect_different_from_overall_p_lm[i] <- summary(lm_fit)$coefficients[4,4]


  current_df <- data_wide %>% dplyr::filter(survey_id == srv_id)

  # compute school specific stats
  specific_school_stats_df$mean_gms_pre[i] <- current_df$gms[current_df$post_treatment == "0"]  %>% mean(na.rm = T)
  specific_school_stats_df$mean_gms_post[i] <- current_df$gms[current_df$post_treatment == "1"]  %>% mean(na.rm = T)

  lm_fit <- lm(gms ~ post_treatment, data = current_df)

  specific_school_stats_df$b_effect[i] <- summary(lm_fit)$coefficients[2,1]
  specific_school_stats_df$se_effect[i] <- summary(lm_fit)$coefficients[2,2]
  specific_school_stats_df$p_effect[i] <- summary(lm_fit)$coefficients[2,4]
  specific_school_stats_df$t_effect[i] <- summary(lm_fit)$coefficients[2,3]
  specific_school_stats_df$df_effect[i] <- lm_fit$df.residual
}

# create participation progress table, based on complete and incomplete sample
buff_df <- d_nt_including_incomplete %>% group_by(survey_id) %>%
  summarise(
    opened_activity = sum(!util.is_blank(progress)),
    answered_survey_questions = sum(progress %in% c(33, 66, 100)),
    completed_activity = sum(progress %in% c(100)) # old criterion was c(66, 100)
    #completed_activity = sum(!util.is_blank(Q6.1)) # if based on progress 100 we get slightly lower number
  )
specific_school_stats_df <- specific_school_stats_df %>%
  merge(
    .,
    buff_df,
    by = "survey_id",
    all.x = TRUE,
    all.y = FALSE
  )
# there are slight difference between completed_activity and n, n will have priority
specific_school_stats_df$completed_activity <- specific_school_stats_df$n

# compute which branch the school is based on statistical significance
# this will determin what text will be printed in the report
specific_school_stats_df$results_branch <- NA

# because we did a bunch of comparisons where we had zero prior to see effects, we have a higher alpha requirement.
# (i.e., looking for differences from the overall trend)
# note that I added this because two schools with statistically significant effects in the right direction
# were getting a message making it sound like the intervention hadn't worked, which is madness.
# and schools that weren't really that much better were getting called out too.
adjusted_p <- .01


# Branch 4 — results significantly worse than overall effect]
# note — this comes first and so will get overridden if any of the other conditions are met.
# this matters when it comes to condition 1, where the effect is positive and statistically significant on its
# own. We don't want those people getting a disappointing message when in fact their results look great.
specific_school_stats_df$results_branch[
    specific_school_stats_df$b_effect < overall_stats$lmer_stats$est &
        specific_school_stats_df$effect_different_from_overall_p < adjusted_p
    ] <- 4
# Branch 2 — results indistinguishable from overall treatment effect & not statistically sig
# note — this includes positive AND negative results that are nonsignificant and not different than the overall effect
specific_school_stats_df$results_branch[
    specific_school_stats_df$p_effect >= .05 &
    specific_school_stats_df$effect_different_from_overall_p >= adjusted_p
] <- 2
# Branch 1 — results statistically significant on their own
# this comes after branches 4 and 2 because it overrides everything else if
# results are positive and statistically significant
# at a particular site
specific_school_stats_df$results_branch[
    specific_school_stats_df$b_effect > 0 &
        specific_school_stats_df$p_effect < .05
    ] <- 1
#Branch 3 — results significantly better than overall effect. This one comes last
specific_school_stats_df$results_branch[
    specific_school_stats_df$b_effect > overall_stats$lmer_stats$est &
    #specific_school_stats_df$p_effect <.05 &
    specific_school_stats_df$effect_different_from_overall_p < adjusted_p
] <- 3

#Branch 5 — results appear negative but are indistinguishable from overall effect.
specific_school_stats_df$results_branch[
    specific_school_stats_df$b_effect < 0 &
        #specific_school_stats_df$p_effect <.05 &
        specific_school_stats_df$effect_different_from_overall_p >= adjusted_p
    ] <- 5

# check out the results branches to make sure they make sense. Pay attention in particular
# to schools with negative direction of effects
# specific_school_stats_df %>%
#     select(results_branch, b_effect, p_effect, effect_different_from_overall_p, n, pre_perc_good, post_perc_good, pct_good_change) %>%
#     dplyr::filter(b_effect < 0) %>%
#     arrange(results_branch) %>%
#     View

## save some project-cohort IDs to see how those reports look and if I want to change

neg_effect_results_branch_5 <- specific_school_stats_df[specific_school_stats_df$results_branch %in% 5,
    c("project_cohort_id", "survey_id")
]

# some schools are bellow N_COMPLETERS_THRESHOLD but have stats which we don't need
specific_school_stats_df$effect_different_from_overall_p[specific_school_stats_df$n < N_COMPLETERS_THRESHOLD] <-NA
specific_school_stats_df$se_effect[specific_school_stats_df$n < N_COMPLETERS_THRESHOLD] <-NA

# round numbers as necessary
vars_to_round <- c("se_effect", "mean_gms_pre", "mean_gms_post", "b_effect", "t_effect",
                   "effect_different_from_overall_b", "effect_different_from_overall_t", "effect_different_from_overall_se")
specific_school_stats_df[,vars_to_round] <- specific_school_stats_df[,vars_to_round] %>% lapply(function(x) round(x,2))
p_to_text_vectorized <- Vectorize(p_to_text, SIMPLIFY = TRUE)
specific_school_stats_df$p_effect_txt <- specific_school_stats_df$p_effect %>% p_to_text_vectorized()
specific_school_stats_df$effect_different_from_overall_p_txt <- specific_school_stats_df$effect_different_from_overall_p %>% p_to_text_vectorized()


# check min and max dates for each report
buff_df <- data_wide %>%
  group_by(survey_id) %>%
  summarise(min_month = min(month(as.Date(StartDate)), na.rm = T),
            max_month = max(month(as.Date(StartDate)), na.rm = T),
            min_week = min(week(as.Date(StartDate)), na.rm = T),
            max_week = max(week(as.Date(StartDate)), na.rm = T),
            max_date = max((as.Date(StartDate)), na.rm = T)
  )

specific_school_stats_df <- specific_school_stats_df %>%
  merge(
    .,
    buff_df,
    by = "survey_id",
    all.x = TRUE,
    all.y = FALSE
  )

##### format some numbers
# round percentages to whole numbers
perc_var_names <- c("pre_perc_good", "post_perc_good", "pct_good_change")
specific_school_stats_df[,perc_var_names] <-
  specific_school_stats_df[,perc_var_names] %>% lapply(function(x) round(x,0))

# and two columns in the overall list
overall_stats[["overall_perc_good_pre"]] <- overall_stats[["overall_perc_good_pre"]] %>% round(.,0)
overall_stats[["overall_perc_good_post"]] <- overall_stats[["overall_perc_good_post"]] %>% round(.,0)
#overall_stats$attrition_stats[["compl_pre_perc"]] <- overall_stats$attrition_stats[["compl_pre_perc"]] %>% round(.,0)
#overall_stats$attrition_stats[["compl_post_perc"]] <- overall_stats$attrition_stats[["compl_post_perc"]] %>% round(.,0)

# thousands separator
overall_stats$n_overall<- format(overall_stats$n_overall, nsmall=0, big.mark=",")  # 1,000.6
overall_stats$overall_count_good_pre<- format(overall_stats$overall_count_good_pre, nsmall=0, big.mark=",")
overall_stats$overall_count_good_post<- format(overall_stats$overall_count_good_post, nsmall=0, big.mark=",")
overall_stats$lmer_stats$df <- overall_stats$lmer_stats$df %>% round (.,0) %>% format(., nsmall=0, big.mark=",")

specific_school_stats_df$n_numer <- specific_school_stats_df$n

n_var_names <- c("n", "n_pre", "n_post")
specific_school_stats_df[,n_var_names] <-
  specific_school_stats_df[,n_var_names] %>% lapply(function(x) format(x, nsmall=0, big.mark=","))


# check how many of the new records are present in 2018
specific_school_stats_df$organization_id_present_in_2018 <- ifelse(specific_school_stats_df$organization_id %in% sids_rc_2019_only$organization_id_sid, TRUE, FALSE)
#table(specific_school_stats_df$organization_id_present_in_2018)
specific_school_stats_df$survey_id_present_in_2018 <- ifelse(specific_school_stats_df$survey_id %in% sids_rc_2019_only$survey_id, TRUE, FALSE)
#table(specific_school_stats_df$survey_id_present_in_2018)
#specific_school_stats_df[,c("survey_id_present_in_2018", "organization_id_present_in_2018", "cohort_label")] %>% View

# check again whether there are any duplicated organization_ids
duplic_org_ids <- specific_school_stats_df$organization_id[duplicated(specific_school_stats_df$organization_id)] %>% unique
if (length(duplic_org_ids) > 0) {
    stop("There are duplicated organization_ids. Investigate!")
} else{
    util.passed("No duplicated org ids in the specific_school_stats_df! Nice!")
}

#specific_school_stats_df$dupl_org_id <- ifelse(specific_school_stats_df$organization_id %in% duplic_org_ids, 1,0)

#x <- specific_school_stats_df %>% dplyr::filter(dupl_org_id == 1) %>%
#  dplyr::select(survey_id, organization_id, n, completed_activity, min_month, max_month, cohort_label)
#View(x)
# there is a problems. The same organization might have participated under two different survey_ids
# This is related to cohort_labels in Neptune. The same organization was running under 2017 and 2018 survey_ids.


# create graph representing the overall effect
#range(data_wide$gms, na.rm = T)
data_wide$in_good_range <- ifelse(data_wide$gms >= MIN_GOOD_GMS, 1, 0)
data_wide$in_good_range[util.is_blank(data_wide$gms)] <- NA

overall_ggplot <- ggplot(data_wide, aes(pre_post_label, in_good_range, fill = pre_post_label)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  ug.se_error_bar +
  ggtitle("Growth Mindset Before and After Program") +
  scale_y_continuous(
    labels = percent,
    breaks = c(0,0.25, 0.5,0.75, 1.0),
    expand = c(.18, 0)) + # expand adds space top and bottom
  coord_cartesian(ylim = c(0, 1)) +
  ylab("% of Students Thinking with a Growth Mindset\nBefore and After Program") +
  xlab("") +
  scale_fill_manual(values = OUTCOME_COLORS, name = "") +
  revised_ht +
  theme(legend.position = "none")

# add significance stars (check visually to make sure the placement still works, since it's hard coded)
# obviously this will have to change if the overall effect is ever not significant
overall_ggplot <- overall_ggplot  + geom_text(aes(x = 1.5, y = 0.8, label = "***"))

# remove quotes from schools which are bellow the threshold
large_orgs <- specific_school_stats_df %>%
  dplyr::filter(n_numer >= N_COMPLETERS_THRESHOLD) %>%
  dplyr::select(survey_id) %>%
  unlist %>%
  unname

d_quotes_original <- d_quotes
d_quotes <- d_quotes %>% dplyr::filter(survey_id %in% large_orgs)


# add columns which will be used for logging


specific_school_stats_df$quotes_displayed <- NA
specific_school_stats_df$nrows_agm <- NA
specific_school_stats_df$nbars_bel <- NA
specific_school_stats_df$missing_subset_features_bel <- NA
specific_school_stats_df$agm_n <- NA
specific_school_stats_df$agm_n_WA <- NA
specific_school_stats_df$agm_n_BLO <- NA
specific_school_stats_df$agm_n_male <- NA
specific_school_stats_df$agm_n_female <- NA
specific_school_stats_df$max_ycoord <- NA



if( ANONYMOUS) {

  tryCatch({
    file_path <- util.find_crypt_paths(list(anon_file = "anonymous_sites.csv"))

    anon_df <- read.csv(file_path$anon_file)
  }, error = function(e){
    stop("cannot read/find " %+% file_path$anon_file)
  })

  # Check if any of the requested sites is missing
  if (any(!anon_df$site %in%  report_sites$project_cohort_id)) {
    sites <- anon_df$site[!anon_df$site %in%  report_sites_summary$project_cohort_id]
    (" The following sites set for anonymization were not found in the data: " %+%
        paste0(sites, collapse = ", ")
    ) %>% stop()
  }

  # leave only the sites to be anonymized
  report_sites <- report_sites[report_sites$project_cohort_id %in% anon_df$site,]
  report_sites$project_cohort_id_anon  <- report_sites$project_cohort_id %>%
    paste0(., "_anon")

}




all_report_sites <- report_sites # keep for debugging

if(SINGLE_RUN){
    report_sites <- all_report_sites[1,]
}

#report_sites_orig <- report_sites
# these are colleges from branches 1,2,3 and 4
sm <- "Survey_SRwfu6Oc" # add a small sample size school
br1 <- sample(specific_school_stats_df$survey_id[specific_school_stats_df$results_branch %in% 1], 1)
br2 <- sample(specific_school_stats_df$survey_id[specific_school_stats_df$results_branch %in% 2], 1)
br3 <- sample(specific_school_stats_df$survey_id[specific_school_stats_df$results_branch %in% 3], 1)
br4 <- sample(specific_school_stats_df$survey_id[specific_school_stats_df$results_branch %in% 4], 1)

berea_survey <- "Survey_sPPiexA1"
diverse_sample <- c(neg_effect_results_branch_5[,"survey_id"], sm, br1, br2, br2, br4, berea_survey)

if(SAMPLE_RUN){
    report_sites <- all_report_sites[all_report_sites$survey_id %in% diverse_sample, ]
}


all_lcs  <- all_items$learning_condition %>% unique
all_lcs <- all_lcs[!util.is_blank(all_lcs)]


# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}


# remove those for which we do not have a single participants since last participation date

recent_survey_ids <- d_merged %>% group_by(survey_id) %>%
  summarise(max_date = max(as.Date(StartDate))) %>%
  dplyr::filter(max_date > as.Date(PREVIOUS_REPORT_DATE)) %>%
  select(survey_id) %>%
  unlist %>%
  unname

report_sites <- report_sites %>% dplyr::filter(survey_id %in% recent_survey_ids)


for(i in 1:nrow(report_sites)){

    org_name <- report_sites[i, "organization_name"] %>% as.character()
    org_id <- report_sites[i, "organization_id"] %>% as.character()
    project_cohort_id <- report_sites[i, "project_cohort_id"] %>% as.character()
    org_name_wrapped <- report_sites[i, "organization_name_wrapped"] %>% as.character()
    survey_id <- report_sites[i, "survey_id"] %>% as.character()

    rmd_path <- RMD_BASE_PATH %+% "CG18_report.Rmd"
    report_name <- "Final Report " %+% project_cohort_id %+% ".html" %>% gsub(" ", "-", .)

    if (ANONYMOUS) {
      org_name <- ANONYMOUS_NAME
      org_name_wrapped <- wrap_text(org_name, 15)
      report_name <- "Final-Report-" %+% project_cohort_id %+% "_anon" %+% ".html" %>% gsub(" ", "-", .)

    }

    report_path   <- RMD_BASE_PATH %+% "cg19_reports/" %+% report_name

    paste0("Running ",report_name ) %>%
        message


    possible_error <-
        tryCatch(
          rmarkdown::render(rmd_path,
                   "html_document",
                   report_path,
                   output_options = list(template = REPO_PARENT %+%
                                             "analysis/common/jurassic.html")
            ),
            error = function(e){
                paste0("Error in ", report_name, ": ", e) %>%
                    message
            }
        )

    if(PDF){
        render_jurassic_pdf(
            report_path,
            production = PRODUCTION_QUALITY,
            new_pdf_path = "~/Documents/reports/"
        )
    }
}
# turn off logging
closeAllConnections()

# Set back to regular interactive mode settings
interactive <- base::interactive

survey_ids_data <- unique(data_wide$survey_id)
survey_ids_quotes <- unique(d_quotes$survey_id)

length(survey_ids_data)
length(survey_ids_quotes) # 86

old_quotes <- util.find_crypt_paths(list(a = "unedited_quotes.csv")) %>% util.read_csv_files()
old_quotes <- old_quotes$a

survey_ids_old_quotes <- unique(old_quotes$survey_id)
length(survey_ids_old_quotes) # 112

survey_ids_old_quotes[!survey_ids_old_quotes %in% survey_ids_quotes]




#check if expexted files are present
specific_school_stats_df$html_saved <- NA
specific_school_stats_df$html_size <- NA
specific_school_stats_df$pdf_saved <- NA
specific_school_stats_df$pdf_size <- NA
specific_school_stats_df$pdf_time <- NA
for(survey_id in specific_school_stats_df$survey_id) {
  project_cohort_id <- specific_school_stats_df$project_cohort_id[specific_school_stats_df$survey_id == survey_id]
  report_name <- "Final Report " %+% project_cohort_id %+% ".html" %>% gsub(" ", "-", .)
  report_path   <- RMD_BASE_PATH %+% "reports/" %+% report_name
  pdf_path <- "~/Documents/reports/"  %+% gsub("html", "pdf", report_name)
  specific_school_stats_df$html_saved[specific_school_stats_df$survey_id == survey_id] <- file.exists(report_path)
  specific_school_stats_df$html_size[specific_school_stats_df$survey_id == survey_id] <- file.info(report_path)$size/1000
  specific_school_stats_df$pdf_saved[specific_school_stats_df$survey_id == survey_id] <- file.exists(pdf_path)
  specific_school_stats_df$pdf_size[specific_school_stats_df$survey_id == survey_id] <- file.info(pdf_path)$size/1000
  specific_school_stats_df$pdf_time[specific_school_stats_df$survey_id == survey_id] <- file.info(pdf_path)$mtime %>% as.character()
}


buff_df <- d_quotes %>%
  group_by(survey_id) %>%
  summarise(
    n_quotes = n(),
    mean_chars_quotes = mean(nchar(value))
  )
specific_school_stats_df_quotes <- specific_school_stats_df %>%
  merge(
    .,
    buff_df,
    by = "survey_id",
    all.x = TRUE,
    all.y = FALSE)

# check the quotes
quotes_check <- specific_school_stats_df_quotes %>%
  dplyr::filter(util.is_blank(n_quotes)) %>%
  select(survey_id, project_cohort_id, completed_activity)

if(any(quotes_check$completed_activity > N_COMPLETERS_THRESHOLD)){
    not_enough_quotes <- quotes_check$survey_id[quotes_check$completed_activity > N_COMPLETERS_THRESHOLD]
    stop("oops, some quotes are missing. Check " %+% paste0(not_enough_quotes, collapse = ", "))
} else{
    util.passed("All surveys expected to have quotes have them. Nice!")
}

CRYPT <- gsub("CG19_OFFICIAL_1_2019-10-14.csv", "", s_raw_paths$complete)
utils::write.csv(data.frame(specific_school_stats_df_quotes),
          CRYPT %+% "specific_school_stats_df_" %+% WRITE_DATE %+%".csv")

# @todo chi_sq for aggregation after filtering is not present
# graph, table, quotes, title page, previous text, previous graphs