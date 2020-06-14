
SINGLE_RUN <- FALSE
SAMPLE_RUN <- FALSE
PDF <- TRUE
PRODUCTION_QUALITY <- FALSE
ANONYMOUS <- FALSE # creates anonymized reports only
ANONYMOUS_NAME <- "Name Redacted"

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

REPO_PARENT <- "~/Sites/"
RMD_BASE_PATH <- REPO_PARENT %+% "analysis/HG17/site_reports/"
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

CURRENT_COHORT <- "2018_spring"

PREVIOUS_REPORT_DATE <-  "2017-12-01"

source(REPO_PARENT %+% "/analysis/HG17/site_reports/HG17_helpers.R")
source(REPO_PARENT %+% "/analysis/common/render_jurassic_pdf.R")
library(rmarkdown)
library(tidyr)
library(purrr)
library(broom)
library(lubridate)

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
    "HG17_Session_1_OFFICIAL_2018-04-30.csv",
    "HG17_Session_1_OFFICIAL-Responses in Progress_2018-04-30.csv"
)

s2_raw <- read_all_qualtrics(
    "HG17_Session_2_OFFICIAL_2018-04-30.csv",
    "HG17_Session_2_OFFICIAL-Responses in Progress_2018-04-30.csv"
)



# Participant/Org/Cohort table:
# Note that this table was made by Sarah Gripshover on Nov 28, 2017.
# It is needed because cohort/org information was not passed to the survey.
# SG has recommended that future surveys store cohort/org info from the platform
# so that this step is not necessary.

# this is a table from bigquery with platform info made using the query
# SELECT o.name as organization_name,
# o.uid as organization_id,
# p.uid as project_cohort_id,
# p.program_label as program_label,
# s.uid as survey_id,
# s.cohort_label as cohort_label
# FROM [neptuneplatform:auto_backup_2017_11_28.Organization] o
# JOIN [neptuneplatform:auto_backup_2017_11_28.Survey] s
# ON s.organization_id = o.uid
# JOIN [neptuneplatform:auto_backup_2017_11_28.ProjectCohort] p
# ON p.organization_id = o.uid
# WHERE s.uid IN (
#     unique values of the survey_id field for sessions 1 and 2
# )
# update from Rumen: now we run this query in sql_connect_HG.R,
# except the finarl filtering part, which we run here


p_raw <- util.find_crypt_paths(list(p = "query.csv")) %>%
    unlist %>%
    read.csv
# filter out neptune records from other surveys
current_survey_ids <- unique(c(s1_raw$survey_id, s2_raw$survey_id))
p_raw <- p_raw[p_raw$survey_id %in% current_survey_ids,]

# remove testing units from p_raw
p_raw <- p_raw[!p_raw$organization_name %in% "Arnrow Test U",]
if ((duplicated(p_raw) %>% sum) > 0) {
  stop ("The merge of data from Neptune resulted in duplicated survey_ids!")
}


# source the salt
util.find_crypt_paths(list(salt = "HG17_salt.R")) %>%
    unlist %>%
    source

# race key
race_key <- util.find_crypt_paths(list(race_key = "HG17_race_key.csv")) %>%
    unlist %>%
    read.csv

scale_descriptions <- util.find_crypt_paths(list(sd = "HG17_scale_descriptions.csv")) %>%
    unlist %>% read.csv

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
##### De-identify ############################################################

s1_di <- s1_raw
s2_di <- s2_raw
p_di <- p_raw

# s1_di$participant_hid <- util.hash_vector(s1_di$participant_id, salt = salt)
# s1_di <- s1_di[!names(s1_di) %in% "participant_hid"]
# s2_di$participant_hid <- util.hash_vector(s2_di$participant_id, salt = salt)
# s2_di <- s2_di[!names(s2_di) %in% "participant_hid"]
# p_di$participant_hid <- util.hash_vector(p_raw$p_uid, salt = salt)
# p_di <- p_di[!names(p_di) %in% "participant_hid"]



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
s1_rn <- s1_di %>% select(-fms_1.1, -fms_2.1) %>% rename(fms_1 = fms_1.2, fms_2 = fms_2.2)
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

# the or_race_cols have x in them for some reason, so add that
or_race_cols <- paste0("or_", race_cols) %>%
    gsub("_([0-9]+)", "_x\\1", .)

# first, replace numeric column suffixes with readable machine label suffixes
new_race_cols <- util.recode(
    names(s1_rn)[names(s1_rn) %in% race_cols],
    race_cols,
    paste0("race_", race_key$machine_label)
)
names(s1_rn)[names(s1_rn) %in% race_cols] <- new_race_cols

new_or_race_cols <- util.recode(
    names(s1_rn)[names(s1_rn) %in% or_race_cols],
    or_race_cols,
    paste0("or_race_", race_key$machine_label)
)
names(s1_rn)[names(s1_rn) %in% or_race_cols] <- new_or_race_cols
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

    # for people with > one race listed, see if they listed a single primary race
    s1_rn$num_primary_races_listed <- apply(s1_rn[new_or_race_cols], 1, function(x) sum(!util.is_blank(x)))

    s1_rn$race_cat[s1_rn$num_primary_races_listed == 1] <- s1_rn[s1_rn$num_primary_races_listed == 1, new_or_race_cols] %>%
        apply(., 1, function(x) return(gsub("or_race_", "", new_or_race_cols[which(!util.is_blank(x))])))

    # if they listed more than one primary race, they are "Other"
    s1_rn$race_cat[s1_rn$num_primary_races_listed > 1] <- "Other"

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

#### Missing participant_ids indicate test data and should be removed
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
    filter(!remove_blank_ID, !remove_too_quick, !remove_duplicated)

s2_rcf <- s2_rc %>%
    filter(!remove_blank_ID, !remove_too_quick, !remove_duplicated)

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




###### Merge survey data with a full join ##############################

s1_rcf_p <- merge(s1_rcf, p_raw, by = "survey_id", all.x = TRUE, all.y = FALSE)
s2_rcf_p <- merge(s2_rcf, p_raw, by = "survey_id", all.x = TRUE, all.y = FALSE)

# check the merges:

# first, check row counts
if(!nrow(s1_rcf_p) == nrow(s1_rcf)){
    stop("Merging platform with survey 1 data resulted in rows being added or dropped.")
}

if(!nrow(s2_rcf_p) == nrow(s2_rcf)){
    stop("Merging platform with survey 2 data resulted in rows being added or dropped.")
}

id_vars <- c("participant_id", setdiff(names(p_raw), "survey_id"))

s1_rcf_p_suffixes <- setNames(s1_rcf_p, paste0(names(s1_rcf_p), "__s1"))
s2_rcf_p_suffixes <- setNames(s2_rcf_p, paste0(names(s2_rcf_p), "__s2"))

# remove suffixes from merging variables
names(s2_rcf_p_suffixes)[names(s2_rcf_p_suffixes) %in% paste0(id_vars, "__s2")] <- gsub(
    "__s2",
    "",
    names(s2_rcf_p_suffixes)[names(s2_rcf_p_suffixes) %in% paste0(id_vars, "__s2")]
)
names(s1_rcf_p_suffixes)[names(s1_rcf_p_suffixes) %in% paste0(id_vars, "__s1")] <- gsub(
    "__s1",
    "",
    names(s1_rcf_p_suffixes)[names(s1_rcf_p_suffixes) %in% paste0(id_vars, "__s1")]
)

d <- merge(
    s1_rcf_p_suffixes,
    s2_rcf_p_suffixes,
    by = id_vars,
    all = TRUE
)


# filter out test data
d_tf <- filter(
    d,
    !organization_name %in% c("TestCrackers GMAT and GRE Prep")
)

# Check the merge
blank_project_cohorts <- d_tf$participant_id[util.is_blank(d_tf$project_cohort_id)]
blank_org_names <- d_tf$participant_id[util.is_blank(d_tf$organization_name)]
if(length(blank_project_cohorts) > 0){
    util.warn("Some participants could not be matched to project cohorts (n = " %+%
                  length(blank_project_cohorts) %+% "). These will be removed from the data.")
}
if(length(blank_org_names) > 0){
    util.warn("Some participants could not be matched to organization names (n = " %+%
                  length(blank_project_cohorts) %+% "). These will be removed from the data.")
}

n_dup_participant_rows <- d_tf %>% filter(util.duplicated_all(participant_id)) %>% nrow
if(n_dup_participant_rows > 0){
    stop("Merging platform and survey data resulted in participant ids being duplicated. (" %+%
             n_dup_participant_rows %+% " rows total.) These will be dropped from the data.")
}

# filter any participants with missing org name or pc id values, or duplicated participants
d_f <- filter(
    d_tf,
    !util.is_blank(project_cohort_id),
    !util.is_blank(organization_name),
    !util.duplicated_all(participant_id)
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
program_n_either_rounded <- 5000
program_n_orgs <- length(unique(d_f$organization_id))
program_n_orgs_rounded <- 40
program_pct_pt_increase <- (round(program_gms_s2_both, 2) - round(program_gms_s1_both, 2))*100

int_analysis_id_vars <- c("participant_id", "organization_id", "organization_name")
d_m <- d_f[c(int_analysis_id_vars, "gms__s1_bin", "gms__s2_bin")] %>%
    melt(., id.vars = int_analysis_id_vars) %>%
    separate(col = variable, into = c("metric", "session"), sep = "__") %>%
    mutate(
        session = gsub("_bin", "", session),
        session_label = ifelse(session %in% "s1", "Before Program", "After Program"),
        session_label = factor(session_label, c("Before Program", "After Program"))
    ) %>%
    filter(!is.na(value)) %>%
    # also filter out any orgs that have just 1 session
    group_by_(.dots = setdiff(int_analysis_id_vars, "participant_id")) %>%
    mutate(n_sessions = length(unique(session))) %>%
    filter(n_sessions > 1) %>%
    ungroup %>% as.data.frame

program_model <- glmer(value ~ session + (1 | organization_id) + (1 | participant_id), data = d_m, family = "binomial")
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


# create a long data.frame for nesting
# (we want one data.frame per org in which we can test the session x is_org interaction)
all_orgs <- unique(d_m$organization_id)
long_d_m <- as.data.frame(matrix(ncol = ncol(d_m), nrow = 0)) %>%
    setNames(names(d_m))
for(org in all_orgs){
    org_name <- p_raw$organization_name[p_raw$organization_id %in% org] %>% unique
    if(length(org_name) > 1) stop("too many orgs")
    org_df <- d_m %>%
        mutate(
            org_being_tested = org,
            org_name_being_tested = org_name,
            is_org = ifelse(organization_id %in% org, 1, 0)
        )
    long_d_m <- rbind(long_d_m, org_df)
}

# Number of rows should equal nrow(d_m) * the number of orgs
nrow(long_d_m) == nrow(d_m) * length(all_orgs)

# now, nest by org_being_tested
nested_df <- long_d_m %>%
    group_by(org_being_tested, org_name_being_tested) %>%
    nest

# run the models
glm_custom <- function(df){
    possible_error <- tryCatch(
        glmer(value ~ session * is_org + (1 | participant_id) + (1 | organization_id),
          data = df, family = "binomial"),
        error = function(e){
            paste0("Error in model: ", e) %>%
                message
        }
    )
}

model_df <- nested_df %>%
    mutate(
        model = map(data, glm_custom)
    )

tidy_df <- model_df %>%
    mutate(
        tidy = map(model, tidy)#,
#         n_s1 = unlist(map(data, function(df) nrow(df[df$session %in% "s1" & df$is_org %in% 1, ]))),
#         n_s2 = unlist(map(data, function(df) nrow(df[df$session %in% "s2" & df$is_org %in% 1, ]))),
#         prop_gms_s1 = unlist(map(data, function(df) mean(df$value[df$session %in% "s1" & df$is_org %in% 1]))),
#         prop_gms_s2 = unlist(map(data, function(df) mean(df$value[df$session %in% "s2" & df$is_org %in% 1])))
    )

SIG_THRESHOLD <- .05/length(all_orgs)
unnested_df <- tidy_df %>%
    select(org_being_tested, org_name_being_tested, tidy
           #, n_s1, n_s2, prop_gms_s1, prop_gms_s2
           ) %>%
    unnest %>%
    filter(
        group %in% "fixed",
        !term %in% "(Intercept)"
    ) %>%
    group_by(org_being_tested) %>%
    mutate(
        sig_interaction = any(term %in% "sessions2:is_org" & p.value < SIG_THRESHOLD),
        worse_effect = any(term %in% "sessions2:is_org" & estimate < 0)
    )

unnested_df %>% filter(sig_interaction & worse_effect)
unnested_df %>% filter(sig_interaction & !worse_effect)

deviant_summary <- unnested_df[c("org_being_tested", "sig_interaction", "worse_effect")] %>%
    unique %>%
    rename(organization_id = org_being_tested) %>%
    mutate(is_neg_deviant = sig_interaction & worse_effect)

numbers_summary <- d_f %>%
    mutate(
        gms_bin_change = gms__s2_bin - gms__s1_bin,
        both_sessions = !is.na(gms__s1_bin) & !is.na(gms__s2_bin)
    ) %>%
    group_by(organization_id, organization_name) %>%
    summarise(
        n_s1 = sum(!is.na(gms__s1_bin)),
        n_s2 = sum(!is.na(gms__s2_bin)),
        n_s1_started = sum(!is.na(ResponseID__s1)),
        n_both = sum(both_sessions),
        n_either = sum(!is.na(gms__s1_bin) | !is.na(gms__s2_bin)),
        prop_s1_gms_both = mean(gms__s1_bin[both_sessions]),
        prop_s2_gms_both = mean(gms__s2_bin[both_sessions]),
        prop_s1_gms_all = mean(gms__s1_bin, na.rm = T),
        prop_s2_gms_all = mean(gms__s2_bin, na.rm = T),
        gms_bin_change = prop_s2_gms_both - prop_s1_gms_both,
        s2_prop_s1 = n_both/n_s1,
        hide_results_s2_n = n_both < S2_N_THRESHOLD,
        hide_results_s2_prop = s2_prop_s1 < S2_PROP_THRESHOLD,
        hide_LCs = n_s1 < S1_N_THRESHOLD
    ) %>%
    as.data.frame

stats_summary <- merge(numbers_summary, deviant_summary, by = "organization_id", all.x = TRUE, all.y = FALSE)

##### Merge stats info with report_sites

report_sites <- unique(
    d_f[c("organization_name", "organization_id", "program_label", "cohort_label", "project_cohort_id", "expected_participants")]
)

report_sites_summary <- merge(
    report_sites,
    select(stats_summary, -organization_name),
    by = "organization_id",
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
#table(d_f$organization_name, d_f$year_month, exclude = NULL)
table(d_f$organization_name, d_f$after_PREVIOUS_REPORT_DATE, exclude = NULL)

summary_tbl <- d_f %>% group_by(project_cohort_id, after_PREVIOUS_REPORT_DATE) %>%
  summarise(count = n()) %>%
  dcast(., project_cohort_id ~ after_PREVIOUS_REPORT_DATE) %>%
  rename(after_PREVIOUS_REPORT_DATE = 'TRUE',
         before_PREVIOUS_REPORT_DATE = 'FALSE')
summary_tbl$after_PREVIOUS_REPORT_DATE %>% table(., exclude = NULL)
summary_tbl$before_PREVIOUS_REPORT_DATE %>% table(., exclude = NULL)

# filter out colleges with 0 new participants
fresh_data_organization_id <- summary_tbl$project_cohort_id[!is.na(summary_tbl$after_PREVIOUS_REPORT_DATE)]
report_sites_summary <- report_sites_summary[report_sites_summary$project_cohort_id %in% fresh_data_organization_id,]


#####################################################################
###### Loop through sites and render reports ########################

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

}


if(SINGLE_RUN){
    report_sites_summary <- report_sites_summary[1, ]
    report_sites_summary <- report_sites_summary[report_sites_summary$project_cohort_id %in% c("ProjectCohort_5u5digc7"),]
} else if(SAMPLE_RUN){
    # these are three reports each with different display logic
    report_sites_summary <- report_sites_summary[report_sites_summary$project_cohort_id %in% c("ProjectCohort_5u5digc7", "ProjectCohort_kp9IjEnt", "ProjectCohort_oznbPPPE"), ]
}

# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}

for(i in 1:nrow(report_sites_summary)){

    organization_name <- report_sites_summary[i, "organization_name"]
    organization_id <- report_sites_summary[i, "organization_id"]
    program_label <- report_sites_summary[i, "program_label"]
    cohort_label <- report_sites_summary[i, "cohort_label"]
    project_cohort_id <- report_sites_summary[i, "project_cohort_id"]
    report_name <- "Final-Report-" %+% project_cohort_id %+% ".html" %>% gsub(" ", "-", .)
    rmd_path <- RMD_BASE_PATH %+% "HG17_report.Rmd"

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
all(report_sites_summary$n_s1_started >= as.numeric(report_sites_summary$agm_n), na.rm = T) # should be TRUE

##### Check that n's correspond between participation summary and the full dataset
all(report_sites_summary$n_s1_started == report_sites_summary$ptable_n_s1) # should be TRUE

##### Visually for weird combinations of missing bars
report_sites_summary %>% filter(!util.is_blank(missing_subset_features_bel))

##### Check that all orgs missing all session 2 data are truly missing it
no_s2_pc_ids <- report_sites_summary$project_cohort_id[report_sites_summary$n_s2 == 0]

# all survey_ids corresponding to such pc_ids should appear in the s1 data but not the s2 data
no_s2_survey_ids <- p_raw[p_raw$project_cohort_id %in% no_s2_pc_ids, "survey_id"]
all(no_s2_survey_ids %in% s1_raw$survey_id) # should be TRUE
all(!no_s2_survey_ids %in% s1_raw$survey_id) # should be FALSE


# make sure every project_cohort_id got a rendered report
report_files <- dir(RMD_BASE_PATH %+% "reports/")

html_report_files <- report_files[grepl("\\.html", report_files)]
html_report_ids <- gsub("\\.html", "", html_report_files) %>%
    gsub("Final-Report-", "", .)

pdf_report_files <- report_files[grepl("\\.pdf", report_files)]
pdf_report_ids <- gsub("\\.pdf", "", pdf_report_files) %>%
    gsub("Final-Report-", "", .)

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

