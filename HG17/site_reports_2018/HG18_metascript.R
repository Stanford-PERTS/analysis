# NOTE for Spring 2020 — there is some unresolved stuff going on around lines 700-750.
# I got some model errors with glmer and wrote some very hasty code to substitute lmer
# for those models. This should be fixed in a leisurely fashion for the next report rendering.

# additionally, note that the crypt you need to run this script is called

SINGLE_RUN <- FALSE
SAMPLE_RUN <- FALSE
PDF <- TRUE
PRODUCTION_QUALITY <- TRUE
ANONYMOUS <- FALSE # creates anonymized reports only
ANONYMOUS_NAME <- "Name Redacted"
# set to FALSE in debugging mode if you don't want to wait 30 minutes to regenerate all the statistical models
COMPUTE_MODELS <- FALSE
MODELS_PARENT_DIR <- "/Volumes/NO NAME 1/" # this is where to save/read the model object for debugging.

library(rmarkdown)
library(tidyr)
library(purrr)
library(broom)
library(lubridate)
library(dplyr)
library(stringr)
library(reshape2)
library(lme4)



# source gymnast functions
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"
source(paste0(github_base_path,"R/util_legacy.R"))
source(paste0(github_base_path,"R/util_qualtrics_cleaning.R"))
source(paste0(github_base_path,"R/util_graphing.R"))
source(paste0(github_base_path,"R/util_scale_computation.R"))


REPO_PARENT <- "~/Sites/"
RMD_BASE_PATH <- REPO_PARENT %+% "analysis/HG17/site_reports_2018/"
REPORT_BASE_PATH <- RMD_BASE_PATH %+% "reports"
DROPBOX_DIR <- "~/Dropbox (PERTS)/"
MIN_CELL <- 5
DRIVER_GRAPH_COLORS <- c("#155994","#95D4F6")
OUTCOME_COLORS <- c("#155994", "#3faeeb")
# what proportion of s1 survey-takers are required to have completed s2
# in order to report growth mindset effects?
S2_PROP_THRESHOLD <- 0
# how many TOTAL students are required to have completed s2
# in order to report growth mindset effects?
S2_N_THRESHOLD <- 20
# how many total students are required to have completed s1 in order to present the LCs?
S1_N_THRESHOLD <- MIN_CELL * 4
# probability threshold (what p-value do you want to use as a cutoff for "it worked!" language?)
PROBABILITY_THRESHOLD <- .05

COHORT_LABEL <- "2019-2020"
COHORT_LABEL_PLATFORM <- "2019"
LONG_COHORT_LABEL <- "Spring 2020"
DATA_PULL_DATE_READABLE <- "May 5th, 2020"
DATA_PULL_DATE <- "2020-05-05"

PREVIOUS_REPORT_DATE <-  "2019-12-01"

source(REPO_PARENT %+% "/analysis/HG17/site_reports_2018/HG18_helpers.R")
source(REPO_PARENT %+% "/analysis/common/render_jurassic_pdf.R")
source(REPO_PARENT %+% "/analysis/HG17/site_reports_2018/HG_models.R")

# set stringsAsFactors to FALSE, and add any other global R settings
options(stringsAsFactors = FALSE)

#################################################################################
############## Create some plotting parameters ################################
plot_title_size <- 12
panel_margin <- .2


##############################################################################
##############################################################################
############################## Load Data #####################################

# The only data for HG17 are the s1 and s2 surveys. There are two "weird"
# things to note about the survey data:

# 1. The two fms questions were saved as embedded data fields to
# send the data back to Neptune. Therefore, there are TWO columns
# labeled fms_1 and fms_2 in the exported data file. The first is
# the embedded column and the second is the question itself. It doesn't
# matter which one you use, since they have the same data, just delete
# one of them.

# 2. The "race" questions allowed multiple checkbox responses, which means
# they get automatically dummy-coded in the exported file. So, that's just
# a bit of a non-standard column cleaning procedure.

s1_raw <- read_all_qualtrics(
  "HG19_Session_1_OFFICIAL_" %+% DATA_PULL_DATE %+% ".csv",
  "HG19_Session_1_OFFICIAL-Responses in Progress_" %+% DATA_PULL_DATE %+% ".csv"
)

s2_raw <- read_all_qualtrics(
  "HG19_Session_2_OFFICIAL_" %+% DATA_PULL_DATE %+% ".csv",
  "HG19_Session_2_OFFICIAL-Responses in Progress_" %+% DATA_PULL_DATE %+% ".csv"
)


############################################################################
############## Neptune data ###############################################
############################################################################
# The following tables were created with Chris's Neptune R script. The version
# I'm using is based on analysis/Neptune/neptune_data_pull_demo.R
# Run that script just as far as defining the object `tables`. (Note you will need to mount
# perts_crypt in order to run neptune_data_pull_demo.R). Then save the following elements
# of `tables`: Organization, ProjectCohort, Survey in the HG crypt as "Organization_DATE.csv" etc.
# Note that in the survey_ids and completed_report_task_ids fields in the ProjectCohort table in `tables`
# as returned by neptune_data_pull_demo is a list type, which means I left it out of the saved file (because
# R won't write a nested df to csv.), along with a few other columns for good measure (survey_params_json
# and data_export_survey_json).

neptune_table_paths <- util.find_crypt_paths(
  list(
    org = "organization_" %+% DATA_PULL_DATE %+% ".csv",
    pc = "project_cohort_"  %+% DATA_PULL_DATE %+% ".csv",
    s = "survey_"  %+% DATA_PULL_DATE %+% ".csv",
    pd = "pd_" %+% DATA_PULL_DATE %+% ".csv"
  )
)

neptune_tables <- util.read_csv_files(neptune_table_paths)

# get a list of the unique survey_id values (these are school-session combos)
survey_ids_qualtrics <- c(s1_raw$survey_id, s2_raw$survey_id) %>% unique
# and filter out the fakes
survey_ids_qualtrics <- survey_ids_qualtrics[!grepl("fake", survey_ids_qualtrics)]

# now merge and filter the Neptune data:
p_raw <- neptune_tables$s %>%
  merge(
    .,
    neptune_tables$org,
    by.x = "organization_id",
    by.y = "uid",
    suffixes = c("_survey", "_org"),
    all.x = TRUE,
    all.y = FALSE
  ) %>%
  rename(survey_id = uid) %>%
  merge(
    .,
    neptune_tables$pc,
    by.x = "project_cohort_id",
    by.y = "uid",
    all.x = TRUE,
    all.y = FALSE,
    suffixes = c("_so", "_pc")
  ) %>%
  dplyr::filter(survey_id %in% survey_ids_qualtrics) %>%
  # remove a few of the suffixes too because the column names are used later without suffixes
  # (e.g., in creating report_sites)
  rename(
    cohort_label = cohort_label_pc,
    program_label = program_label_pc,
    organization_name = name,
    organization_id = organization_id_so
  )

# expected row check:
if(nrow(p_raw) != length(survey_ids_qualtrics)){
  util.warn("The number of unique rows in the merged Neptune data.frame does not match the number of
       unique survey_id values pulled from the Qualtrics surveys. These should match, otherise
       it means there are entire surveys (i.e., school-session combos) that are not represented
       in either the Neptune or else the Qualtrics data. This needs to be investigated before proceeding.")
  # to troubleshoot, see how many rows/survey_ids are affected
  extra_survey_ids <- survey_ids_qualtrics[!survey_ids_qualtrics %in% p_raw$survey_id]
  s1_raw %>%
    dplyr::filter(survey_id %in% extra_survey_ids) %>%
    nrow()
  # in this case, it looks like just seven. I think I'm going to proceed and
}

# how many rows are affected?
extra_survey_ids <- survey_ids_qualtrics[!survey_ids_qualtrics %in% p_raw$survey_id]

# it looks like it's an EXTRA survey id in the Qualtrics but not the platform data
pids_extra_survey_ids <- s1_raw %>%
  dplyr::filter(survey_id %in% extra_survey_ids) %>%
  pull(participant_id)

# and it's not in the survey table at all, or the pd table. The participants aren't either.
neptune_tables$s[neptune_tables$s$uid %in% extra_survey_ids, ]
neptune_tables$pd[neptune_tables$pd$survey_id %in% extra_survey_ids, ]
neptune_tables$pd[neptune_tables$pd$participant_id %in% pids_extra_survey_ids, ]

# Ok I'm not going to worry about this. It's just seven rows.

# check for entire duplicated survey_id values in the merged platform data
if ((duplicated(p_raw$survey_id) %>% sum) > 0) {
  stop ("The merge of data from Neptune resulted in duplicated survey_ids!")
}

# race key
race_key <- util.find_crypt_paths(list(race_key = "HG17_race_key.csv")) %>%
  unlist() %>%
  read.csv()

scale_descriptions <- util.find_crypt_paths(list(sd = "HG17_scale_descriptions.csv")) %>%
  unlist() %>%
  read.csv()

#### Read in the items .csv doc
# because non-ascii characters must be removed, we will also have to re-code variables back to logicals.
to_logical <- function(x){
  if(all(x %in% c("TRUE", "FALSE", NA))){
    x <- as.logical(x)
  }
  return(x)
}

items_path <- util.find_crypt_paths(list(items = "HG17_items_manually_checked.csv"))
all_items <- read.csv(items_path$items) %>%
  util.to_ascii() %>%
  util.apply_columns(., to_logical)

# strip the pdd tags from the question text
all_items$question_text <- gsub("__pdd__[0-9a-zA-Z_-]*__pdd__", "", all_items$question_text)
all_items$question_text <- gsub('Survey Section Go Back ', "", all_items$question_text)

# read in driver descriptions
driver_desc_path <- util.find_crypt_paths(list(driver_desc = "HG17_driver_descriptions.csv"))
driver_desc <- read.csv(driver_desc_path$driver_desc)

# dynamic text
dynamic_text_path <- util.find_crypt_paths(list(dt = "HG17_dynamic_text.csv"))
dt <- read.csv(dynamic_text_path$dt)

# change ideas
ci_path <- util.find_crypt_paths(list(ci = "change_ideas.R"))
source(ci_path$ci)

##############################################################################
##############################################################################
##### Deprecated: de-identify ################################################

# deidentification is deprecated so simply re-name objects. Note that
# it's deprecated because (a) we've moved to a procedure of anonymizing
# rather than de-identifying data; and (b) these are school-facing reports,
# not research reports. We are therefore allowed to create summaries
# based on raw data (note that no raw data will appear in the reports)
s1_di <- s1_raw
s2_di <- s2_raw
p_di <- p_raw



###############################################################
############ Clean columns ####################################

# remove the embedded data versions of the fms variable.
# (There are two versions of this variable in the dataset because the
# survey was programmed to send growth mindset information back to the
# Neptune platform as an embedded data field. This means that Qualtrics stores
# the values for the question twice: once as a survey data field, and once as
# an embedded data field. You can confirm that the values are identical by
# doing table(s1_di$fms_1.1, s1_di$fms_1.3)).
# It doesn't matter which one so just remove the first, which is named fms_1.1.
s1_rn <- s1_di %>%
  select(-fms_1.1, -fms_2.1) %>%
  rename(fms_1 = fms_1.2, fms_2 = fms_2.2)
s2_rn <- s2_di

#### Standardize column values
#
# * any variable recoding (ideally using a key file)
# * includes grade recoding with `glk` if applicable

# recode the race values:
race_cols <- c(
  "race_1",
  "race_2",
  "race_3",
  "race_4",
  "race_5",
  "race_6",
  "race_7",
  "race_8"
)


# in hg18 there is difference in race column names. This was not changed in hg19
orig_race_cols_hg18 <- paste0("Q5.3_", 1:8)

# first, replace numeric column suffixes with readable machine label suffixes
if(!any(names(s1_rn) %in% orig_race_cols_hg18)){
  stop(
    "The operation to rename race columns below must be deprecated, because " %+%
    "no columns match the orig_race_cols_hg18 object. Consider deleting this step " %+%
    "so as not to inadvertently mess something up!"
  )
}
new_race_cols <- util.recode(
  names(s1_rn)[names(s1_rn) %in% orig_race_cols_hg18],
  orig_race_cols_hg18,
  paste0("race_", race_key$machine_label)
)

# check if the order of categories is correct (look in the docx printout from Qualtrics)

names(s1_rn)[names(s1_rn) %in% orig_race_cols_hg18] <- new_race_cols


# next, create a categorical column.
# whenever somebody specified JUST ONE primary race, use that.
# otherwise code "other/mixed"

# only compute race categorical if hasn't been computed yet
if(is.null(s1_rn$race_cat)){
  s1_rn$race_cat <- NA
  s1_rn$num_races_listed <- apply(
    s1_rn[c(new_race_cols)],
    1,
    function(x) sum(!util.is_blank(x))
  )

  # this is looking up the column name corresponding to the single non-NA value
  s1_rn$race_cat[s1_rn$num_races_listed == 1] <- s1_rn[s1_rn$num_races_listed == 1, new_race_cols] %>%
    apply(., 1, function(x) return(gsub("race_", "", new_race_cols[which(!util.is_blank(x))])))

  # if they listed more than one primary race, they are "Other"
  s1_rn$race_cat[s1_rn$num_races_listed > 1] <- "Other"

  # recode into advantaged race
  s1_rn$race_advantaged <- util.recode(s1_rn$race_cat, race_key$machine_label, race_key$advantaged_race)
}

# finally, recode all other categorical vars
categoricals <- all_items$var_name[all_items$categorical]
categorical_vals <- all_items$recoded_values[all_items$categorical]
for(i in 1:length(categoricals)){
  column <- categoricals[i] %>% gsub("__s[1-2]{1}", "", .)
  originals <- sort(unique(s1_rn[,column]))
  new_values <- str_split(categorical_vals[i], "; ")[[1]]
  s1_rn[[column %+% "_cat"]] <- util.recode(s1_rn[[column]], originals, new_values)
}

#####################################################################
############ Clean rows #############################################

# drop rows with no date information
s1_rn <- s1_rn %>%
  dplyr::filter(!util.is_blank(StartDate), !util.is_blank(EndDate))
s2_rn <- s2_rn %>%
  dplyr::filter(!util.is_blank(StartDate), !util.is_blank(EndDate))

#### Missing participant_ids indicate test data and should be removed,
#### as should non-blank values in the "testing" column

s1_rc <- s1_rn %>%
  mutate(
    remove_blank_ID = util.is_blank(participant_id)
  ) %>%
  mark_qualtrics_dups(id_vars = "participant_id")

s2_rc <- s2_rn %>%
  mutate(
    remove_blank_ID = util.is_blank(participant_id)
  ) %>%
  mark_qualtrics_dups(id_vars = "participant_id")

s1_rcf <- s1_rc %>%
  dplyr::filter(!remove_blank_ID, !remove_too_quick, !remove_duplicated)

s2_rcf <- s2_rc %>%
  dplyr::filter(!remove_blank_ID, !remove_too_quick, !remove_duplicated)

# ### Conduct checks on the pre-processed data
#
# #### Unique indexes
# Make sure your indexes are unique (could use [check_index_is_unique](https://gist.github.com/sarahgripshover/3192e2ce1128bb5e79fc3edc2471d9eb)).

if(!check_index_is_unique(s1_rcf, "participant_id")){
  util.warn("Duplicate participants found in survey 1 data after duplicate handling. Investigate")
}

if(!check_index_is_unique(s2_rcf, "participant_id")){
  util.warn("Duplicate participants found in survey 2 data after duplicate handling. Investigate")
}

################ Clean platform data #####################################
# remove unnecessary records (orphaned old cohorts, non-matchable survey_id)
p_filtered <- p_raw %>%
  dplyr::filter(cohort_label == "2019") %>%
  arrange(organization_id)

if(nrow(p_raw) != nrow(p_filtered)){
  filtered_survey_ids <- p_raw$survey_id[!p_raw$survey_id %in% p_filtered$survey_id]
  filtered_students_s1 <- s1_raw %>% filter(survey_id %in% filtered_survey_ids) %>% nrow
  filtered_students_s2 <- s2_raw %>% filter(survey_id %in% filtered_survey_ids) %>% nrow
  util.warn(
    "data found from other cohorts besides the current one in the filtered Neptune data. This" %+%
      "could indicate the presence of old participation codes. Might be a good idea to check and see " %+%
      "if there are a lot of them and if there are, see if you can fix it. In the meantime, we're going " %+%
      "to filter those data out. There were " %+% length(filtered_survey_ids) %+% " such survey_id values " %+%
      "corresponding to " %+% sum(filtered_students_s1, filtered_students_s2) %+% " students."
  )
}

if (SAMPLE_RUN) {p_filtered <- p_filtered[1:10,]} # take only x rows

# check that all org_ids match across sources

if(!all(p_raw$organization_id_pc == p_raw$organization_id_so)){
  stop("some organization_id values from the project_cohort table do not match those found in the
       merged survey/organization table. It is expected that these should match perfectly because
       surveys are nested within project-cohorts which are nested within organizations. So both the
       survey table and the project-cohort table should inherit the same organization_id value from
       the organization table. Please resolve this before rendering the reports, as it reveals a
       problem with assumptions made about the Neptune data schemas.")
}

# check how many rows per organization
nrow_check <- p_raw %>%
  group_by(project_cohort_id) %>%
  summarise(count_ = n())

if(any(nrow_check$count_ > 2)){
  stop("some project-cohort values in the Neptune data correspond to more than two unique survey_id
            values in the Qualtrics data. This is unexpected and challenges assumptions about the Neptune
            data schema. Please investigate further before proceeding.")
}

# max is 2, so no need for corrections. Since there are no schools participating
# both with old and new codes, you can also remove all schools which are not
# registered for the 2018 program


###### Merge survey data with a left join ##############################

s1_rcf_p <- merge(s1_rcf, p_filtered, by = "survey_id", all.x = TRUE, all.y = FALSE)
s2_rcf_p <- merge(s2_rcf, p_filtered, by = "survey_id", all.x = TRUE, all.y = FALSE)



# check the merges:

# first, check row counts
if(!nrow(s1_rcf_p) == nrow(s1_rcf)){
  stop("Merging platform with survey 1 data resulted in rows being added or dropped.")
}

if(!nrow(s2_rcf_p) == nrow(s2_rcf)){
  stop("Merging platform with survey 2 data resulted in rows being added or dropped.")
}

if(any(c(util.is_blank(s1_rcf_p$project_cohort_id), util.is_blank(s2_rcf_p$project_cohort_id)))){
  util.warn(
    "At least one blank project_cohort_id was found in the merged s1 or s2 " %+%
    "survey/platform data. This check was written in case a bad merge could " %+%
    "cause a lot of data to be dropped due to not finding the right project_cohort_id. " %+%
    "If it's just a handful of rows, then it's probably nothing to worry about. " %+%
    "But if it's more than five or so, then the analyst should investigate " %+%
    "which project_cohort_ids are missing and why before running the rest of the " %+%
    "script. It could be something dumb, like somebody from the previous cohort " %+%
    "accidentally completing the current survey. But you shouldn't render the " %+%
    "reports without understanding what's causing the blank project_cohort_ids. "
  )
}

s1_rcf_p %>%
  dplyr::filter(util.is_blank(project_cohort_id)) %>%
  nrow() # just one row

s2_rcf_p %>%
  dplyr::filter(util.is_blank(project_cohort_id)) %>%
  nrow() # also one row

# next, check that platform data was included successfully

id_vars_wide <- c("participant_id", "organization_name", "organization_id",
                  "program_label", "cohort_label", "project_cohort_id", "expected_participants")

s1_rcf_p_suffixes <- setNames(s1_rcf_p, paste0(names(s1_rcf_p), "__s1"))
s2_rcf_p_suffixes <- setNames(s2_rcf_p, paste0(names(s2_rcf_p), "__s2"))

# remove suffixes from merging variables
names(s2_rcf_p_suffixes)[names(s2_rcf_p_suffixes) %in% paste0(id_vars_wide, "__s2")] <- gsub(
  "__s2",
  "",
  names(s2_rcf_p_suffixes)[names(s2_rcf_p_suffixes) %in% paste0(id_vars_wide, "__s2")]
)
names(s1_rcf_p_suffixes)[names(s1_rcf_p_suffixes) %in% paste0(id_vars_wide, "__s1")] <- gsub(
  "__s1",
  "",
  names(s1_rcf_p_suffixes)[names(s1_rcf_p_suffixes) %in% paste0(id_vars_wide, "__s1")]
)

# This merge is supposed to create a wide dataset. It works as long as none of the id_vars_wide have a time component
# (i.e., survey_id can't be one of the id_vars_wide)
d <- merge(
  s1_rcf_p_suffixes,
  s2_rcf_p_suffixes,
  by = id_vars_wide,
  all = TRUE
)

# dplyr::filter out test data
d_tf <- dplyr::filter(
  d,
  !organization_name %in% c("TestCrackers GMAT and GRE Prep"),
  util.is_blank(testing__s1),
  util.is_blank(testing__s2)
)

# Check the merge
blank_project_cohorts <- d_tf$participant_id[util.is_blank(d_tf$project_cohort_id)]
blank_org_names <- d_tf$participant_id[util.is_blank(d_tf$name)]
if(length(blank_project_cohorts) > 0){
  util.warn("Some participants could not be matched to project cohorts (n = " %+%
              length(blank_project_cohorts) %+% "). These will be removed from the data.")
}
if(length(blank_org_names) > 0){
  util.warn("Some participants could not be matched to organization names (n = " %+%
              length(blank_project_cohorts) %+% "). These will be removed from the data.")
}

# check that the expected project-cohorts are still present
missing_pcids <- p_filtered$project_cohort_id[!p_filtered$project_cohort_id %in% d_tf$project_cohort_id]
if(length(missing_pcids) > 0){
  util.warn(
    length(missing_pcids) %+% " expected project_cohort_ids are missing from the " %+%
    "merged platform/survey data with testers removed (d_tf). The overlap " %+%
    "should be very good. A few missing project_cohort ids could occur " %+%
    "if all the data from particular project-cohorts happened to be test data. " %+%
    "But if there are a lot of missing project-cohort ids, then this could mean a bad " %+%
    "merge or test data filtered out inappropriately."
  )
}

# check out the filtered project-cohorts
p_filtered %>%
  dplyr::filter(project_cohort_id %in% missing_pcids)
# one was not even approved, and the other

# check that d is wide by participant_id
n_dup_participant_rows <- d_tf %>% dplyr::filter(util.duplicated_all(participant_id)) %>% nrow
if(n_dup_participant_rows > 0){
  stop("Merging platform and survey data resulted in participant ids being duplicated. (" %+%
         n_dup_participant_rows %+% " rows total.) These will be dropped from the data.")
}

# filter any participants with missing org name or pc id values, or duplicated participants

d_f <- dplyr::filter(
  d_tf,
  !util.is_blank(project_cohort_id),
  !util.is_blank(organization_name),
  !util.duplicated_all(participant_id)
)

util.warn(
  as.character(nrow(d_tf) - nrow(d_f)) %+% " of " %+% nrow(d_tf) %+% " rows " %+%
            "rows filtered for blank project_cohort, organization_name, and " %+%
            "duplicated participant_id values. If this sounds high, investigate further."
  )

#####################################################################
###### Identify present metrics, subset_groups, and outcomes ########mes
all_metrics <- all_items$var_name[all_items$is_metric]
present_metrics <- all_metrics[all_metrics %in% names(d_f)]

all_subset_groups <- all_items$var_name[all_items$is_subset]
present_subset_groups <- all_subset_groups[all_subset_groups %in% names(d_f)]

all_outcomes <- all_items$var_name[all_items$is_outcome]
present_outcomes <- all_outcomes[all_outcomes %in% names(d_f)]

all_outcome_pre_measures <- gsub("__s2", "__s1", present_outcomes)
present_outcome_pre_measures <- all_outcome_pre_measures[all_outcome_pre_measures %in% names(d_f)]

all_panel_groups <- all_items$var_name[all_items$is_panel_var]
present_panel_groups <- all_panel_groups[all_panel_groups %in% names(d_f)]

present_drivers <- all_items$driver[all_items$var_name %in% present_metrics] %>%
  unique
present_drivers <- present_drivers[!util.is_blank(present_drivers)]

q_group_info <- all_items[all_items$driver %in% present_drivers, c("q_group", "driver")] %>%
  unique

# make sure all q_groups have drivers that match driver_desc. If not, throw a warning.
if(!all(unique(q_group_info$driver) %in% driver_desc$driver)){
  q_groups_without_driver <- q_group_info$q_group[
    !q_group_info$driver %in% driver_desc$driver
    ]
  non_matching_drivers <- q_group_info$driver[!q_group_info$driver %in% driver_desc$driver] %>%
    unique
  warning("The following question groups have drivers that are not matched" %+%
            "in driver descriptions" %+%
            paste0(q_groups_without_driver, collapse = ", ") %+%
            ". (The non-matching drivers are: " %+%
            paste0(non_matching_drivers, collapse = ", "))
}


### Create wrapped question text
all_items$question_text_wrapped <- sapply(all_items$question_text, function(x) wrap_text(x))

#####################################################################
###### Do some analysis on the full dataset #########################

###### Analyze overall main effect of treatment
d_f$fms__s1 <- rowMeans(d_f[present_outcome_pre_measures])
d_f$fms__s2 <- rowMeans(d_f[present_outcomes])

d_f$gms__s1_bin <- ifelse(d_f$fms__s1 < 2, 1, 0)
d_f$gms__s2_bin <- ifelse(d_f$fms__s2 < 2, 1, 0)

d_f$gms_bin_change <- d_f$gms__s1_bin

program_gms_s1_all <- mean(d_f$gms__s1_bin, na.rm = T)
program_gms_s2_all <- mean(d_f$gms__s2_bin, na.rm = T)
program_gms_s1_both <- mean(d_f$gms__s1_bin[!is.na(d_f$gms__s1_bin) & !is.na(d_f$gms__s2_bin)])
program_gms_s2_both <- mean(d_f$gms__s2_bin[!is.na(d_f$gms__s1_bin) & !is.na(d_f$gms__s2_bin)])
program_n_s1 <- nrow(d_f[!is.na(d_f$gms__s1_bin), ])
program_n_s2 <- nrow(d_f[!is.na(d_f$gms__s2_bin), ])
program_n_both <- nrow(d_f[!is.na(d_f$gms__s1_bin) & !is.na(d_f$gms__s2_bin), ])
program_n_either <- nrow(d_f)
program_n_either_rounded <- 3000
program_n_orgs <- length(unique(d_f$organization_id))
program_n_orgs_rounded <- 30
program_pct_pt_increase <- (round(program_gms_s2_both, 2) - round(program_gms_s1_both, 2))*100

int_analysis_id_vars <- c("participant_id", "organization_id", "organization_name", "project_cohort_id")

############################ Data exclusion note ###################################
## d_m is not merely a melted data object. It is an object that filters out students
## from organizations with fewer than 2 sessions completed. Note that this exclusion
## criterion is a bit different from the conditional logic in the report script that
## determines whether individual results will be displayed: that rule requires that
## no fewer than 20 participants must appear in the data. This rule filters on the n_sessions
## variable, which actually come to think of it I don't know how that is computed, so maybe
## it is the same — I just don't know.

d_m <- d_f[c(int_analysis_id_vars, "gms__s1_bin", "gms__s2_bin")] %>%
  melt(., id.vars = int_analysis_id_vars) %>%
  separate(col = variable, into = c("metric", "session"), sep = "__") %>%
  mutate(
    session = gsub("_bin", "", session),
    session_label = ifelse(session %in% "s1", "Before Program", "After Program"),
    session_label = factor(session_label, c("Before Program", "After Program"))
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  # also filter out any orgs that have just 1 session. Wait...why?
  group_by_(.dots = setdiff(int_analysis_id_vars, "participant_id")) %>%
  mutate(n_sessions = length(unique(session))) %>%
  dplyr::filter(n_sessions > 1) %>%
  ungroup %>% as.data.frame

program_model <- glmer(value ~ session + (1 | participant_id), data = d_m, family = "binomial")
program_model_summary <- summary(program_model)
program_pval <- program_model_summary$coefficients[[2, "Pr(>|z|)"]]

###### Graph overall program impact
program_p_pi <- ggplot(d_m, aes(session_label, value, fill = session_label)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_y_continuous(labels = percent) +
  ylab("% Who Held a Growth Mindset") +
  xlab("") +
  scale_fill_manual(values = OUTCOME_COLORS, name = "") +
  ug.ht +
  ggtitle("% of Students Who Held a Growth Mindset\nBefore and After the Program") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = plot_title_size)
  )

############################################################################
################### Create stats within orgs ###############################
############################################################################

# Note — this should have been updated to create stats within project-cohorts.
# If I have time I'll do it now, otherwise the new research associate will have to do
# it when they start.

# these are the n's according to Neptune. They will be used in the reports'
# participation table
expected_ns_each <- neptune_tables$pd %>%
  dplyr::filter(cohort_label %in% COHORT_LABEL_PLATFORM) %>%
  rename(progress = value) %>%
  # get each participant's highest level of completion
  group_by(project_cohort_id, survey_id, survey_ordinal) %>%
  summarise(
    total_n = n(),
    n_100 = length(participant_id[progress %in% "100"]),
    n_66 = length(participant_id[progress %in% "66"]),
    n_33 = length(participant_id[progress %in% "33"]),
    n_1 = length(participant_id[progress %in% "1"])
  )

# also get the platform's record of who completed BOTH sessions
expected_ns_both <- neptune_tables$pd %>%
  group_by(participant_id, project_cohort_id) %>%
  summarise(
    n_sessions_atall = n_distinct(survey_ordinal),
    n_sessions_100 = sum(value %in% 100),
    n_sessions_atleast_66 = sum(value >= 66),
    n_sessions_atleast_33 = sum(value >= 33),
    n_sessions_atleast_1 = sum(value >= 1)
  ) %>%
  group_by(project_cohort_id) %>%
  summarise(
    n_both_100 = sum(n_sessions_100 == 2),
    n_both_atleast_66 = sum(n_sessions_atleast_66 == 2),
    n_both_atleast_33 = sum(n_sessions_atleast_33 == 2),
    n_both_atleast_1 = sum(n_sessions_atleast_1 == 2)
  )

expected_ns_each_casted <- expected_ns_each %>%
  ungroup() %>%
  select(project_cohort_id, survey_ordinal, n_100) %>%
  dcast(project_cohort_id ~ survey_ordinal, value.var = "n_100") %>%
  setNames(c("project_cohort_id", "n_100_1", "n_100_2")) %>%
  mutate(
    n_100_1 = ifelse(is.na(n_100_1), 0, n_100_1),
    n_100_2 = ifelse(is.na(n_100_2), 0, n_100_2)
  )

if(any(
  !p_raw$project_cohort_id[p_raw$cohort_label %in% COHORT_LABEL_PLATFORM] %in%
  expected_ns_each_casted$project_cohort_id
)){
  util.warn(
    "Some project_cohort_ids from the current cohort in p_raw do not appear in expected_ns_casted. " %+%
    "This could be possible if some project cohorts had absolutely no activity, " %+%
    "but make sure it's expected before proceeding."
  )
}

numbers_summary <- d_f %>%
  mutate(
    gms_bin_change = gms__s2_bin - gms__s1_bin,
    both_sessions_gms = !is.na(gms__s1_bin) & !is.na(gms__s2_bin)
  ) %>%
  group_by(project_cohort_id, organization_name) %>%
  summarise(
    n_s1_qualtrics = sum(!is.na(ResponseID__s1)),
    n_s2_qualtrics = sum(!is.na(ResponseID__s2)),
    n_s1_started = sum(!is.na(ResponseID__s1)),
    n_both_qualtrics = sum(!is.na(ResponseID__s1) & !is.na(ResponseID__s2)),
    n_either = sum(!is.na(gms__s1_bin) | !is.na(gms__s2_bin)),
    prop_s1_gms_both = mean(gms__s1_bin[both_sessions_gms]),
    prop_s2_gms_both = mean(gms__s2_bin[both_sessions_gms]),
    prop_s1_gms_all = mean(gms__s1_bin, na.rm = T),
    prop_s2_gms_all = mean(gms__s2_bin, na.rm = T),
    gms_bin_change = prop_s2_gms_both - prop_s1_gms_both,
    s2_prop_s1 = n_both_qualtrics/n_s1_qualtrics,
    hide_results_s2_n = n_both_qualtrics < S2_N_THRESHOLD,
    hide_results_s2_prop = s2_prop_s1 < S2_PROP_THRESHOLD,
    hide_LCs = n_s1_qualtrics < S1_N_THRESHOLD
  ) %>%
  as.data.frame() %>%
  left_join(
    .,
    expected_ns_each_casted,
    by = "project_cohort_id"
  ) %>%
  left_join(
    .,
    expected_ns_both,
    by = "project_cohort_id"
  )

# check the logic of the participation counts

# it should never be possible to have an n_both_100 value that's greater than
# either of the two n_100_1 and n_100_2 values

if(!all(numbers_summary$n_100_1 >= numbers_summary$n_both_100 &
        numbers_summary$n_100_2 >= numbers_summary$n_100_2)){
  stop("n_both_100 is sometimes smaller than n_100_1 or n_100_2.
       The set of students who completed both session should always
       be less than or equal to the set that completed either separately.")
}


# create a long data.frame for nesting
pcs_to_model <- numbers_summary$project_cohort_id[numbers_summary$n_both_qualtrics >= S2_N_THRESHOLD]

# long_d_m creates a full, unique data.frame for every org and then is going to
# nest them so that the org x not-org interaction can be tested for each org
long_d_m <- as.data.frame(matrix(ncol = ncol(d_m), nrow = 0)) %>%
  setNames(names(d_m))
for(pc in pcs_to_model){
  org_name <- p_filtered$organization_name[p_filtered$project_cohort_id %in% pc] %>% unique
  if(length(org_name) > 1) stop("too many orgs")
  pc_df <- d_m %>%
    mutate(
      pc_being_tested = pc,
      org_name_being_tested = org_name,
      is_pc = ifelse(project_cohort_id %in% pc, 1, 0)
    )
  long_d_m <- rbind(long_d_m, pc_df)
}

# now, nest by org_being_tested
nested_df <- long_d_m %>%
  group_by(pc_being_tested) %>%
  nest

if(COMPUTE_MODELS){
  # Warning! These models take FOREVER to compute.
  unnested_df <- run_hg_models(nested_df)
  saveRDS(unnested_df, MODELS_PARENT_DIR %+% "all_models_" %+% DATA_PULL_DATE %+% ".Rds")
} else{
  unnested_df <- readRDS(MODELS_PARENT_DIR %+% "all_models_" %+% DATA_PULL_DATE %+% ".Rds")
}

unnested_df %>%
  dplyr::filter(is.na(p.value))

deviant_summary <- unnested_df[c("pc_being_tested", "sig_interaction", "worse_effect")] %>%
  unique %>%
  rename(project_cohort_id = pc_being_tested) %>%
  mutate(is_neg_deviant = sig_interaction & worse_effect)

neg_deviant_pc_ids <- deviant_summary$project_cohort_id[deviant_summary$is_neg_deviant]
unnested_df[unnested_df$pc_being_tested %in% neg_deviant_pc_ids,]

stats_summary <- merge(
  numbers_summary,
  deviant_summary,
  by = "project_cohort_id",
  all.x = TRUE,
  all.y = FALSE
)

##### Merge stats info with report_sites

report_sites <- unique(
  d_f[c(
    "organization_name",
    "organization_id",
    "program_label",
    "cohort_label",
    "project_cohort_id",
    "expected_participants"
  )]
)

report_sites_summary <- merge(
  report_sites,
  select(stats_summary, -organization_name),
  by = "project_cohort_id",
  all = TRUE
)


if(!nrow(report_sites) == nrow(report_sites_summary)){
  stop("report_sites_summary has a different number of rows than report_sites")
}


# check participation dates for different schools
d_f$StartDate_dt <- as.Date(d_f$StartDate__s1)
d_f$StartDate_year <- year(d_f$StartDate_dt)
d_f$StartDate_week <- sprintf( "%02d", month(d_f$StartDate_dt))
d_f$year_month <- paste0(d_f$StartDate_year, "_", d_f$StartDate_week)
d_f$after_PREVIOUS_REPORT_DATE <- d_f$StartDate_dt > as.Date(PREVIOUS_REPORT_DATE)


# why would there be blank start dates? Is that a problem?

# SG has no idea what this stuff means or what it's for
summary_tbl <- d_f %>%
  group_by(project_cohort_id, after_PREVIOUS_REPORT_DATE) %>%
  summarise(count = n()) %>%
  dcast(., project_cohort_id ~ after_PREVIOUS_REPORT_DATE)
try({summary_tbl <- summary_tbl %>% rename(after_PREVIOUS_REPORT_DATE = 'TRUE')}, silent = TRUE)
try({summary_tbl <- summary_tbl %>% rename(before_PREVIOUS_REPORT_DATE = 'FALSE')}, silent = TRUE)
summary_tbl$after_PREVIOUS_REPORT_DATE %>% table(., exclude = NULL)
#summary_tbl$before_PREVIOUS_REPORT_DATE %>% table(., exclude = NULL)

# filter out schools with 0 new participants
fresh_data_pc_id <- summary_tbl$project_cohort_id[
  !is.na(summary_tbl$after_PREVIOUS_REPORT_DATE)
]
report_sites_summary <- report_sites_summary[
  report_sites_summary$project_cohort_id %in% fresh_data_pc_id,
]


###############################################################
###### Check data integrity before reports are rendered #######
###############################################################

# make sure all the schools with zero participation in session 2 have zero participants in the Qualtrics data
no_s2_participation_pcs <- numbers_summary$project_cohort_id[
  numbers_summary$n_both == 0
]
# these will be a mix of s1 and s2 survey ids but that's ok
no_s2_participation_survey_ids_from_Neptune <- neptune_tables$s$uid[
  neptune_tables$s$project_cohort_id %in% no_s2_participation_pcs
]

s2_raw[s2_raw$survey_id %in% no_s2_participation_survey_ids_from_Neptune, ] %>%
  nrow() # this value should be zero. It's 1...what does that mean?
  # I suppose that means someone with a survey_id that shouldn't have participation
  # somehow made it onto the platform anyways...doesn't seem like a big deal though


#####################################################################
###### Loop through sites and render reports ########################
#####################################################################

if( ANONYMOUS) {

  tryCatch({
    file_path <- util.find_crypt_paths(list(anon_file = "anonymous_sites.csv"))

    anon_df <- read.csv(file_path$anon_file)
  }, error = function(e){
    stop("cannot read/find " %+% file_path$anon_file)
  })

  # Check if any of the requested sites is missing
  if (any(!anon_df$site %in%  report_sites_summary$project_cohort_id)) {
    sites <- anon_df$site[!anon_df$site %in%  report_sites_summary$project_cohort_id]
    (" The following sites set for anonymization were not found in the data: " %+%
        paste0(sites, collapse = ", ")
    ) %>% stop()
  }

  # leave only the sites to be anonymized
  report_sites_summary <- report_sites_summary[report_sites_summary$project_cohort_id %in% anon_df$site,]
  report_sites_summary$project_cohort_id_anon  <- report_sites_summary$project_cohort_id %>%
    paste0(., "_anon")
  print(names(report_sites_summary))

}


if(SINGLE_RUN){
  old_report_sites_summary <- report_sites_summary
  report_sites_summary <- report_sites_summary[1, ]
} else if(SAMPLE_RUN){
  # these are three reports each with different display logic
  report_sites_summary <- report_sites_summary[1:3, ]
}


# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}

i <- which(report_sites_summary$project_cohort_id %in% "ProjectCohort_0Cvgi9sd")
for(i in 1:nrow(report_sites_summary)){

  organization_name <- report_sites_summary[i, "organization_name"]
  organization_id <- report_sites_summary[i, "organization_id"]
  program_label <- report_sites_summary[i, "program_label"]
  cohort_label <- report_sites_summary[i, "cohort_label"]
  project_cohort_id <- report_sites_summary[i, "project_cohort_id"]
  report_name <- "Final-Report-" %+% project_cohort_id %+% ".html" %>% gsub(" ", "-", .)
  rmd_path <- RMD_BASE_PATH %+% "HG18_report.Rmd"

  if (ANONYMOUS) {
    organization_name <- ANONYMOUS_NAME
    report_name <- "Final-Report-" %+% project_cohort_id %+% "_anon" %+% ".html" %>% gsub(" ", "-", .)

  }

  report_path   <- RMD_BASE_PATH %+% "reports/" %+% report_name

  paste0("Running ",report_name ) %>%
    message


  possible_error <-
    tryCatch(
      render(rmd_path,
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
    render_jurassic_pdf(report_path, production = PRODUCTION_QUALITY)
  }
}
# turn off logging
closeAllConnections()

# Set back to regular interactive mode settings
interactive <- base::interactive

#####################################################################
##### Final checks ##################################################

# visually inspect
write.csv(report_sites_summary, "~/Downloads/report_sites_summary.csv", row.names = FALSE)

##### Check that n's correspond between agm and the full dataset
if(all(report_sites_summary$n_s1_started >= as.numeric(report_sites_summary$agm_n), na.rm = T)){
  util.passed(
    "Nice! All n values in the report_sites_summary are greater " %+%
    "than or equal to those found in agm_n.")
} else{
  stop(
    "Oops! Some n values in report_sites_summary are less than those in agm_n." %+%
    "this shouldn't be the case and needs to be investigated.")
}

if(all(report_sites_summary$n_100_1 == report_sites_summary$ptable_n_s1 &
       report_sites_summary$n_100_2 == report_sites_summary$ptable_n_s2)){
  util.passed(
    "Nice! N values in the n_s1_started field correspond to those in the ptable_n_s1" %+%
      " field. This is good because it means the participation summary n matches " %+%
      " the dashboard"
  )
} else{
  stop(
    "Oops! N values in the n_s1_started field don't correspond to those in the ptable_n_s1" %+%
      " field. These should be consistent. Check it out before uploading reports"
  )
}

# are there any inconsistencies in the Qualtrics data that would lead to
# surprising display results (people not getting a model when they should)?

inconsistent_numbers_model <- report_sites_summary %>%
  dplyr::filter(
    n_both_qualtrics < n_both_100,
    ((n_both_qualtrics > S2_N_THRESHOLD & !n_both_100 > S2_N_THRESHOLD) |
    (n_both_qualtrics < S2_N_THRESHOLD & !n_both_100 < S2_N_THRESHOLD))
  ) %>%
  select(project_cohort_id, n_both_qualtrics, n_both_100) %>%
  pull(project_cohort_id)

inconsistent_numbers_graphs <- report_sites_summary %>%
  dplyr::filter(
    n_s1_qualtrics < n_100_1,
    ((n_s1_qualtrics > S1_N_THRESHOLD & !n_100_1 > S1_N_THRESHOLD) |
       (n_s1_qualtrics < S1_N_THRESHOLD & !n_100_1 < S1_N_THRESHOLD))
  ) %>%
  select(project_cohort_id, n_s1_qualtrics, n_100_1) %>%
  pull(project_cohort_id)

if(length(inconsistent_numbers_model) > 0 | length(inconsistent_numbers_graphs)){
  stop(
    "Discrepancies between qualtrics and platform resulted in inconsistencies " %+%
    "in display logic for models and/or graphs for project-cohorts " %+%
    paste0(
      unique(c(inconsistent_numbers_model, inconsistent_numbers_graphs)),
      collapse = ", "
    )
  )
}

# make sure every project_cohort_id got a rendered report
report_files <- dir(RMD_BASE_PATH %+% "reports/")

html_report_files <- report_files[grepl("\\.html", report_files)]
html_report_ids <- gsub("\\.html", "", html_report_files) %>%
  gsub("Final-Report-", "", .)

pdf_report_files <- report_files[grepl("\\.pdf", report_files)]
pdf_report_ids <- gsub("\\.pdf", "", pdf_report_files) %>%
  gsub("Final-Report-", "", .)


################################################################
##### Check expected participation against the Neptune table ###

report_sites_ns <- report_sites_summary %>%
  select(project_cohort_id, n_s1_qualtrics, n_s1_started, n_s2) %>%
  melt(id.vars = "project_cohort_id", stringsAsFactors = FALSE) %>%
  mutate(
    survey_ordinal = ifelse(grepl("s1", variable), 1, ifelse(grepl("s2", variable), 2, NA)),
    variable = gsub("_s1|_s2", "", variable)
  ) %>%
  dplyr::filter(!is.na(survey_ordinal)) %>%
  dcast(project_cohort_id + survey_ordinal ~ variable)

participation_neptune_check <- left_join(
  report_sites_ns,
  expected_ns,
  by = c("project_cohort_id", "survey_ordinal")
) %>%
  util.as_numeric_if_number() %>%
  rowwise() %>%
  mutate(
    expected_n_min = sum(n_100, n_66),
    expected_n_max = sum(n_100, n_66, n_33, n_1)
  ) %>%
  ungroup() %>%
  mutate(
    difference_with_min = expected_n_min - n,
    difference_with_max = expected_n_max - n
  )

# get the really bad ones
participation_neptune_check %>%
  dplyr::filter(
    !is.na(survey_id),
    abs(difference_with_min) > 10 & abs(difference_with_max) > 10
  ) %>%
  arrange(abs(difference_with_max))

d_f %>%
  group_by(project_cohort_id) %>%
  summarise(
    s1_n_survey_ids = n_distinct(survey_id__s1),
    s2_n_survey_ids = n_distinct(survey_id__s2)
  ) %>%
  dplyr::filter(s1_n_survey_ids > 1 | s2_n_survey_ids > 1)

hist(participation_neptune_check$difference_with_max)
# so the majority of deviations are negative, and nearly all of the really
# big "outlier" type deviations are negative, which should mean more participants
# in the Qualtrics data than on the platform

numbers_summary2 %>% dplyr::filter(project_cohort_id %in% "ProjectCohort_0Cvgi9sd") # 644

# yep, looks right. How many rows appeared in the survey data for this pc?
s1_rcf_p_suffixes %>%
  dplyr::filter(survey_id__s1 %in% "Survey_gGqIfFoO") %>%
  nrow() # 568

d_f %>%
  dplyr::filter(survey_id__s2 %in% "Survey_xymVESTo") %>%
  nrow() # 377

s2_raw %>%
  dplyr::filter(
    survey_id %in% "Survey_xymVESTo",
    !is.na(fms_1),
    !is.na(fms_2)
  ) %>%
  nrow() #

# ok so that's right. What about the pd table?
neptune_tables$pd %>%
  dplyr::filter(survey_id %in% "Survey_TP8oTpsm") %>%
  select(participant_id) %>%
  unique() %>%
  nrow() #452 unique participants on the pd table


##########################################################
# now check the cases where the survey has more participants than the platform
# ProjectCohort_0Cvgi9sd Survey_WxHKppY4 (session ordinal 2)

# the platform dashboard currently says sum(7, 33, 18, 420) == 478 students participated with progress >= 1

s2_raw %>%
  dplyr::filter(survey_id %in% "Survey_WxHKppY4") %>%
  nrow() # 372



############ A more general pattern ##########################
# now look at how many "extra" rows (total) are in the pd table
missing_from_survey_s1 <- neptune_tables$pd %>%
  dplyr::filter(
    !participant_id %in% s1_raw$participant_id,
    cohort_label %in% "2019",
    survey_ordinal %in% 1,
    !value %in% 1,
    survey_id %in% s1_raw$survey_id
  )
nrow(missing_from_survey_s1) #1017...that's so weird. How??


#project_cohort_ids_all <- unique(d_f$project_cohort_id)
# we are not creating repors for all project_cohorts, but only for those who have new
# participants. That's why the check for missing files will use different expectation list
project_cohort_ids_all <- unique(report_sites_summary$project_cohort_id)

ids_missing_html <- project_cohort_ids_all[!project_cohort_ids_all %in% html_report_ids]
ids_missing_pdf <- project_cohort_ids_all[!project_cohort_ids_all %in% pdf_report_ids]

if(length(ids_missing_html) > 0){
  message("The following project cohorts did NOT get html reports: " %+%
            paste0(ids_missing_html, collapse = ", "))
} else{
  message("Html reports were successfully rendered for all project_cohort_id values!")
}

if(PDF){
  if(length(ids_missing_pdf) > 0){
    util.warn("The following project cohorts did NOT get pdf reports: " %+%
                paste0(ids_missing_html, collapse = ", "))
  } else{
    message("PDF reports were successfully rendered for all project_cohort_id values!")
  }
}

