SINGLE_RUN <- FALSE
PDF <- TRUE
PRODUCTION_QUALITY <- FALSE
READ_DATE <- Sys.Date() # this is the date you want to append to the path for the survey data when you're reading it in.
# Feel free to change it if it's not today.
# for data pulls prior to 2018, set READ_DATE to empty string and delete the underscores from the path labels.
# (complete = "CG17_OFFICIAL_1_" %+% READ_DATE %+% ".csv") becomes complete = "CG17_OFFICIAL_1" %+% READ_DATE %+% ".csv"
READ_DATE <- as.Date("2018-03-19")
WRITE_DATE <- Sys.Date() # this is the date you want appended to saved, processed data.
# Feel free to change it if it's not today.
WRITE_DATE <- as.Date("2018-03-19")


PREVIOUS_REPORT_DATE <-  "2017-10-14" # colleges which do not have participants
# after this date will not be included

ANONYMOUS <- FALSE # creates anonymized reports only
ANONYMOUS_NAME <- "Name Redacted"


# use the crypt called CG17_large.vc

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

# Note — this script will only work properly using the crypt called CG17_large.vc

REPO_PARENT <- "~/Sites/"
RMD_BASE_PATH <- REPO_PARENT %+% "analysis/CG17/site_reports/"
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
source(RMD_BASE_PATH %+% "CG17_helpers.R")
source(RMD_BASE_PATH %+% "CG17_dynamic_text.R")
source(REPO_PARENT %+% "analysis/common/render_jurassic_pdf.R")

# probability threshold (what p-value do you want to use as a cutoff for "it worked!" language?)
PROBABILITY_THRESHOLD <- .05
N_COMPLETERS_THRESHOLD <- 20

##############################################################################
##############################################################################
#################### Clean & process data ####################################

library("tidyr")
library("lubridate")
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
        complete = "CG17_OFFICIAL_1_" %+% READ_DATE %+% ".csv",
        partial = "CG17_OFFICIAL_1-Responses in Progress_" %+% READ_DATE %+% ".csv",
        quotes = "CG17 Quotes Complete.csv",
        test_cutoffs = "test_date_cutoffs.csv",
        pc = "project_cohort_" %+% READ_DATE %+% ".csv",
        sids = "survey_" %+% READ_DATE %+% ".csv"
    )
)

s_complete <- read.csv(s_raw_paths$complete) %>%
    mutate(in_progress = NA)
s_partial <- read.csv(s_raw_paths$partial)

d_initial <- qc.clean_qualtrics(s_complete) %>%
    qc.rbind_inprogress(s_partial, .)

# add suffixes for repeated column names (these are there because Qualtrics
# checkboxes are not yet handled gracefully by qc.clean_qualtrics)
# long-term fix: we should make gymnast do this automatically.
names(d_initial)[names(d_initial) %in% "race"] <- paste0("race.", 1:length(names(d_initial)[names(d_initial) %in% "race"]))
names(d_initial)[names(d_initial) %in% "race_revised"] <- paste0("race_revised.", 1:length(names(d_initial)[names(d_initial) %in% "race_revised"]))

d_quotes <- read.csv(s_raw_paths$quotes)
test_cutoffs <- read.csv(s_raw_paths$test_cutoffs)
pc <- read.csv(s_raw_paths$pc)
sids <- read.csv(s_raw_paths$sids)

REPO <- "~/Sites/analysis/"
all_items <- read.csv(REPO %+% "CG17/CG17_variable_descriptions.csv")

## (note that we will skip de-identification because there is no data output
# in this script, and no individual identifiable data in the dataset (except
# maybe student survey responses))


### Clean columns

#### Standardize column names

# define d_rn (will stand for d, renamed)
d_rn <- d_initial

# Because Qualtrics automatically dummy-codes checkbox variables, but
# DOESN'T record what the dummy codes correspond to (sigh), we have to
# manually recode values for the race variable, which was administered
# as a checkbox.

# note that we also made a mistake with the qualtrics survey in which
# we ("we" = SG) omitted one of the categories (Hispanic/Latino).
# When this was discovered on Sep 21, the question was updated in the survey.
# However, this led to two versions of the race question that need to be combined
# for analysis. The old (broken) one was simply "race", and the new (repaired)
# one was "race_revised".

# note that order matters in these lists. They must be in the same order that they appear in the survey.
race_values_not_revised <- c("AA", "Asian", "Nat", "ME", "PI", "Eu", "Oth")
race_values_revised <- c("AA", "Asian", "Lat", "Nat", "ME", "PI", "Eu", "Oth")

# first, recode the column names to use the human-readable suffixes, rather than
# numeric ones
old_race_cols_not_revised <- names(d_rn)[grep("race\\.[0-9]+$", names(d_rn))]

# catch if the old race variable was not included in the data export:
if(length(old_race_cols_not_revised) == 0){
    stop("Old race data was not exported from Qualtrics. This is for dumb " %+%
             "reasons having to do with a mistake with the race question early " %+%
             "on in CG17. The mistake was fixed, and the old version of the question " %+%
             "was removed from the survey flow. But removing it from the flow hides " %+%
             "the old data from the data export. (sigh.) So add it back into the survey " %+%
             "flow for a moment and then re-export the data, and try again.")
}

new_race_cols_not_revised <- gsub("\\.[0-9]+", "", old_race_cols_not_revised) %+% "_" %+% race_values_not_revised

old_race_cols_revised <- names(d_rn)[grep("race_revised\\.[0-9]+$", names(d_rn))]
new_race_cols_revised <- gsub("\\.[0-9]+", "", old_race_cols_revised) %+% "_" %+% race_values_revised

names(d_rn) <- util.recode(names(d_rn), old_race_cols_not_revised, new_race_cols_not_revised)
names(d_rn) <- util.recode(names(d_rn), old_race_cols_revised, new_race_cols_revised)



#### Standardize column values
# d_rc will stand for "d, recoded"
d_rc <- d_rn


# In this section, we will also determine for each participant
# which race question they answered, and keep their response to that question
# in order to combine both versions into a single variable about race.

for(val in race_values_revised){

    not_revised_col <- "race_" %+% val
    revised_col <- "race_revised_" %+% val

    # The not-revised column should be missing for exactly one value: Lat.
    # any other warnings are unexpected.
    if(is.null(d_rc[[revised_col]])){
        stop(revised_col %+% " not found in data. This is unexpected.")
    }

    if(is.null(d_rc[[not_revised_col]])){
        final_values <- d_rc[[revised_col]]
        warning(not_revised_col %+% " not found in data. Using values from " %+% revised_col %+%
                    ". (This is expected for race_Lat column but not for others.)")
    } else{
        final_values <- ifelse(util.is_blank(d_rc[[revised_col]]), d_rc[[not_revised_col]], d_rc[[revised_col]])
    }
    d_rc[["race_final_" %+% val]] <- final_values
}

# remove the old versions of the race columns to avoid confusion:
d_rc <- d_rc[!names(d_rc) %in% paste0("race_", race_values_not_revised)]
d_rc <- d_rc[!names(d_rc) %in% paste0("race_revised_", race_values_revised)]
names(d_rc) <- gsub("race_final_", "race_", names(d_rc))

# compute the simplified categories
d_rc$race_simplified <- NA
d_rc$race_simplified[d_rc$race_Asian | d_rc$race_Eu] <- "White/Asian"
d_rc$race_simplified[d_rc$race_AA | d_rc$race_Lat | d_rc$race_Nat | d_rc$race_Oth | d_rc$race_PI | d_rc$race_ME] <- "Blk/Lat/Oth"
d_rc$race_disadv <- d_rc$race_simplified %in% "Blk/Nat/Lat/Oth"


new_student_key <- list(old = c(1,2,3), new = c("incoming", "new", "returning"))
d_rc$new_student <- util.recode(
    d_rc$new_student,
    new_student_key$old,
    new_student_key$new
)

gender_key <- list(old = c(1,2,3,4), new = c("Male", "Female", "Nonbinary", "Nonbinary"))
d_rc$gender <- util.recode(d_rc$gender, gender_key$old, gender_key$new)

# the non-binary people should be counted, but they will mess up the cell sizes.
# in other projects, we recode these people to female. I'm  not sure that's what
# we should do here...but it would be handy to have a simplified version of the variable
# on-hand
d_rc$gender_simplified <- util.recode(d_rc$gender, "Nonbinary", "Female")
d_rc$female <- d_rc$gender_simplified %in% "Female"

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
d_nt <- merge(d_rc, test_cutoffs, by.x = "organization_id", by.y = "Organization_ID", all.x = TRUE, all.y = FALSE)
if(!nrow(d_nt) == nrow(d_rc)){
    stop("Merging test cutoff dates resulted in rows being added or dropped from the data. " %+%
             "Investigate before proceeding.")
}
# most values will be NA
d_nt <- d_nt[is.na(d_nt$last_testing_date) | d_nt$StartDate > d_nt$last_testing_date,]

# check participation dates for different colleges
d_nt$StartDate_dt <- as.Date(d_nt$StartDate)
d_nt$StartDate_year <- year(d_nt$StartDate_dt)
d_nt$StartDate_week <- sprintf( "%02d", month(d_nt$StartDate_dt))
d_nt$year_month <- paste0(d_nt$StartDate_year, "_", d_nt$StartDate_week)
d_nt$after_PREVIOUS_REPORT_DATE <- d_nt$StartDate_dt > as.Date(PREVIOUS_REPORT_DATE)
#table(d_nt$organization_name, d_nt$year_month, exclude = NULL)
table(d_nt$organization_name, d_nt$after_PREVIOUS_REPORT_DATE, exclude = NULL)

summary_tbl <- d_nt %>% group_by(organization_id, after_PREVIOUS_REPORT_DATE) %>%
  summarise(count = n()) %>%
  dcast(., organization_id ~ after_PREVIOUS_REPORT_DATE) %>%
  rename(after_PREVIOUS_REPORT_DATE = 'TRUE',
         before_PREVIOUS_REPORT_DATE = 'FALSE')
summary_tbl$after_PREVIOUS_REPORT_DATE %>% table(., exclude = NULL)
summary_tbl$before_PREVIOUS_REPORT_DATE %>% table(., exclude = NULL)

# remove Arnrow's testers too
d_nt <- d_nt[!d_nt$organization_name %in% "Arnrow Test U", ]

# finally remove one tester row that seems not to correspond to a real org on Neptune,
# for reasons that I can't comprehend (and don't care about because it's just one row of the data)
missing_org_rows <- nrow(filter(d_nt, organization_id %in% "Organization_z2siN3K8"))
if(missing_org_rows > 1){
    stop("A custom decision was made to filter out rows corresponding to " %+%
             "Organization_z2siN3K8 in the survey data, because this org " %+%
             "could not be found in the Neptune database, and because it was only " %+%
             "represented by a single row in the survey data. This error indicates that " %+%
             "the second assumption has been broken. " %+% missing_org_rows %+%
             " rows now correspond to the org. If this number is still low, then you may be fine. " %+%
             "But if it's greater than, say, 20, then you should probably investigate what's going " %+%
             "on with this org because it might be a real org and not a tester.")
}

d_nt <- d_nt[!d_nt$organization_id %in% "Organization_z2siN3K8", ]


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

## compute scales & change scores

d_scales <- sc.append_scales(d_dh, all_items, add_z_score = TRUE)
d_scales$gms_post_neg <- rowMeans(d_scales[c("gms_post_3", "gms_post_4")])

d_scales$gms_diff <- d_scales$gms_post - d_scales$gms_pre
d_scales$gms_diff_neg <- d_scales$gms_post_neg - d_scales$gms_pre

# create fixed, mixed, and growth variables
d_scales$gms_cat_pre <- NA
d_scales$gms_cat_pre[d_scales$gms_pre <= 2] <- "fixed"
d_scales$gms_cat_pre[d_scales$gms_pre <= 4 & !d_scales$gms_cat_pre %in% "fixed"] <- "mixed"
d_scales$gms_cat_pre[d_scales$gms_pre > 4] <- "growth"
d_scales$gms_growth_pre <- d_scales$gms_cat_pre == "growth"

d_scales$gms_cat_post <- NA
d_scales$gms_cat_post[d_scales$gms_post <= 2] <- "fixed"
d_scales$gms_cat_post[d_scales$gms_post <= 4 & !d_scales$gms_cat_post %in% "fixed"] <- "mixed"
d_scales$gms_cat_post[d_scales$gms_post > 4] <- "growth"
d_scales$gms_growth_post <- d_scales$gms_cat_post == "growth"

d_scales$gms_cat_post_neg <- NA
d_scales$gms_cat_post_neg[d_scales$gms_post_neg <= 2] <- "fixed"
d_scales$gms_cat_post_neg[d_scales$gms_post_neg <= 4 & !d_scales$gms_cat_post_neg %in% "fixed"] <- "mixed"
d_scales$gms_cat_post_neg[d_scales$gms_post_neg > 4] <- "growth"
d_scales$gms_growth_post_neg <- d_scales$gms_cat_post_neg == "growth"

d_scales$gms_pre_growth <- ifelse(d_scales$gms_cat_pre %in% "growth", 1, 0)
d_scales$gms_post_growth <- ifelse(d_scales$gms_cat_post %in% "growth", 1, 0)
d_scales$gms_post_neg_growth <- ifelse(d_scales$gms_cat_post_neg %in% "growth", 1, 0)

d_scales$gms_pre_mixed <- ifelse(d_scales$gms_cat_pre %in% "mixed", 1, 0)
d_scales$gms_post_mixed <- ifelse(d_scales$gms_cat_post %in% "mixed", 1, 0)
d_scales$gms_post_neg_mixed <- ifelse(d_scales$gms_cat_post_neg %in% "mixed", 1, 0)

d_scales$gms_pre_fixed <- ifelse(d_scales$gms_cat_pre %in% "fixed", 1, 0)
d_scales$gms_post_fixed <- ifelse(d_scales$gms_cat_post %in% "fixed", 1, 0)
d_scales$gms_post_neg_fixed <- ifelse(d_scales$gms_cat_post_neg %in% "fixed", 1, 0)


### merge in project-cohort info from Triton. Note that we will begin by
### merging the survey table to the Qualtrics table on survey_id. Per the documentation
### here (https://docs.google.com/presentation/d/1tEZ_mlI11xyo8g9Kr8l7u8uXyZfJqB_mmnWdW52eJv8/edit#slide=id.g11ba45caf1_0_0),
### the survey field is the most precise way to match survey records with other info in the database.
### Note that using another variable, like organization_id, will get you in trouble because orgs
### can have multiple program_ids. Starting by merging in the survey table will never lead you astray.

sids_rn <- sids %>% select(uid, project_cohort_id, program_label) %>%
    rename(survey_id = uid)
sids_rc <- sids_rn %>% filter(program_label %in% "cg17")

# do a left join to keep rows from data, but drop rows from pc_rc (probably schools that didn't "matriculate")
d_merged <- merge(d_scales, sids_rc, by = "survey_id", all.x = TRUE, all.y = FALSE)
if(!nrow(d_merged) == nrow(d_scales)){
    stop("merging project-cohort info resulted in rows being added or dropped from data.")
}
if(any(util.is_blank(d_merged$project_cohort_id))){
    missing_orgs <- d_merged$organization_id[util.is_blank(d_merged$project_cohort_id)]
    stop("The following organization_ids could not be matched to project_cohort_ids: " %+%
             paste0(missing_orgs, collapse = ", "))
}


# define final wide dataset, data_w
data_wide <- d_merged

########################################################################
################## Create melted dataset for pre/post analysis #########

demog_vars <- c("gender_simplified", "race_simplified")
id_vars <- c("participant_id", "organization_name", "organization_id", "organization_name_wrapped", "project_cohort_id")
pre_shift_vars <- c("gms_pre_growth")
post_shift_vars <- c("gms_post_neg_growth")
data_melted <- melt(
    data_wide[c(id_vars, demog_vars, pre_shift_vars, post_shift_vars)],
    id.vars = c(id_vars, demog_vars)
)
data_melted$pre_post <- ifelse(grepl("_pre", data_melted$variable), "pre", "post")
data_melted$pre_post_label <- factor(
    data_melted$pre_post,
    levels = c("pre", "post"),
    labels = c("Before Program", "After Program")
)

#####################################################################
###### Write unit-level data to crypt for further analysis. #########

CRYPT <- gsub("CG17_OFFICIAL_1_" %+% READ_DATE %+% ".csv", "", s_raw_paths$complete)

if(dir.exists(CRYPT)){
write.csv(data_wide, CRYPT %+% "CG_data_wide_" %+% WRITE_DATE %+% ".csv")
    write.csv(data_melted, CRYPT %+% "CG_data_long_" %+% WRITE_DATE %+% ".csv")
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

report_sites <- unique(data_wide[c("organization_name", "organization_id", "organization_name_wrapped", "project_cohort_id")])

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






if(SINGLE_RUN){
    #report_sites <- report_sites[1, ]
    report_sites <- report_sites[report_sites$project_cohort_id %in% #"ProjectCohort_hXWtHbFn",]
                                     c("ProjectCohort_vEgCCbhH", "ProjectCohort_MZyraFxf", "ProjectCohort_MZyraFxf", "ProjectCohort_2quLJTKv", "ProjectCohort_oWQou8Jv", "ProjectCohort_CU6Txq4G",
                                      "ProjectCohort_9YxfNnTY", "ProjectCohort_WFYC60GC", "ProjectCohort_hXWtHbFn", "Organization_fNKJduKn"),]
}


all_lcs  <- all_items$learning_condition %>% unique
all_lcs <- all_lcs[!util.is_blank(all_lcs)]

# undescribed_lcs <- all_lcs[!all_lcs %in% names(lc_desc)]
# if(length(undescribed_lcs) > 0){
#     all_lcs <- setdiff(all_lcs, undescribed_lcs)
#     util.warn("The following learning conditions do not match learning conditions in " %+%
#                   "driver_desc (an object sourced in from " %+%
#                   "engagement_helpers.R): " %+%
#                   paste0(undescribed_lcs, collapse = ", ") %+%
#                   ". These learning conditions will NOT appear in the reports, " %+%
#                   "unless you either (1) add a description to lc_desc, " %+%
#                   "or (2) change the driver label in the items .csv " %+%
#                   "to match one of the existing learning conditions in lc_desc.")
# }

count_log <- data.frame(
    project_cohort_id = unique(data_wide$project_cohort_id),
    nrows_agm = NA,
    nbars_bel = NA,
    missing_subset_features_bel = NA,
    EVAL_DISAG = NA,
    n_completers = NA,
    n_starters = NA,
    n_completers_WA = NA,
    n_completers_BLO = NA,
    n_completers_male = NA,
    n_completers_female = NA,
    agm_n = NA,
    agm_n_WA = NA,
    agm_n_BLO = NA,
    agm_n_male = NA,
    agm_n_female = NA,
    quotes_displayed = NA,
    max_ycoord = NA,
    number_of_quotes = NA,
    simplified_quotes_length = NA,
    likely_repeat_quotes = NA,
    quotes = NA
)
count_log <- merge(
    count_log,
    unique(data_wide[c("project_cohort_id", "organization_id", "organization_name")]),
    by = "project_cohort_id",
    all = TRUE
)

#stop("here")

# filter out colleges with 0 new participants
fresh_data_organization_id <- summary_tbl$organization_id[!is.na(summary_tbl$after_PREVIOUS_REPORT_DATE)]
report_sites <- report_sites[report_sites$organization_id %in% fresh_data_organization_id,]



# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}

for(i in 1:nrow(report_sites)){

    org_name <- report_sites[i, "organization_name"] %>% as.character()
    org_id <- report_sites[i, "organization_id"] %>% as.character()
    project_cohort_id <- report_sites[i, "project_cohort_id"] %>% as.character()
    org_name_wrapped <- report_sites[i, "organization_name_wrapped"] %>% as.character()

    rmd_path <- RMD_BASE_PATH %+% "CG17_report.Rmd"
    report_name <- "Final Report " %+% project_cohort_id %+% ".html" %>% gsub(" ", "-", .)

    if (ANONYMOUS) {
      org_name <- ANONYMOUS_NAME
      org_name_wrapped <- wrap_text(org_name, 15)
      report_name <- "Final-Report-" %+% project_cohort_id %+% "_anon" %+% ".html" %>% gsub(" ", "-", .)

    }

    report_path   <- RMD_BASE_PATH %+% "reports/" %+% report_name

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

# add participation dates data to the log file
count_log <- merge(count_log,
                   summary_tbl,
                   by = "organization_id",
                   all.x = T,
                   all.y = F)



#check if expexted files are present
count_log$file_saved <- NA
for(project_cohort_id in report_sites$project_cohort_id) {
  report_name <- "Final Report " %+% project_cohort_id %+% ".html" %>% gsub(" ", "-", .)
  report_path   <- RMD_BASE_PATH %+% "reports/" %+% report_name
  count_log$file_saved[count_log$project_cohort_id == project_cohort_id] <- file.exists(report_path)
}

CRYPT <- gsub("CG17_OFFICIAL_1_2018-04-10.csv", "", s_raw_paths$complete)
write.csv(count_log, CRYPT %+% "count_log_" %+% WRITE_DATE %+%".csv", row.names = FALSE)

