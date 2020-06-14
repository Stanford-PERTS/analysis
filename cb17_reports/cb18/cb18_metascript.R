

SINGLE_RUN <- FALSE
PDF <- FALSE
PRODUCTION_QUALITY <- FALSE

ANONYMOUS <- FALSE # creates anonymized reports only
ANONYMOUS_NAME <- "Name Redacted"

# source gymnast functions
tryCatch({
  source("~/Sites/gymnast/R/util.R", chdir = TRUE)
  gymnast_install()
  source("~/Sites/gymnast/R/util_qualtrics_cleaning.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_graphing.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_scale_computation.R", chdir = TRUE)
}, error = function(e){
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R")
  gymnast_install()
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_qualtrics_cleaning.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_graphing.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/scale_computation.R")
})

REPO_PARENT <- "~/Sites/"
RMD_BASE_PATH <- REPO_PARENT %+% "analysis/cb17_reports/"
REPORT_BASE_PATH <- RMD_BASE_PATH %+% "reports"
DROPBOX_DIR <- "~/Dropbox (PERTS)/"
MIN_CELL <- 5
DRIVER_GRAPH_COLORS <- c("#155994","#95D4F6")
OUTCOME_COLORS <- c("#155994", "#3faeeb")
# probability threshold (what p-value do you want to use as a cutoff for "it worked!" language?)
PROBABILITY_THRESHOLD <- .05


powered_by_logo_path <- "https://www.perts.net/static/images/ctc_logos/powered_by_logo.png"
perts_logo_path <- "https://www.perts.net/static/images/ctc_logos/perts_logo.png"
ctc_small_logo_path <- "https://www.perts.net/static/images/ctc_logos/CTC_logo_small.png"
ctc_perts_logo_path <- "https://www.perts.net/static/images/ctc_perts_logo_small.png"
ctc_big_logo_path <- "https://www.perts.net/static/images/ctc_logos/CTC_logo_big.png"


source(REPO_PARENT %+% "analysis/cb17_reports/cb17_helpers.R")
source(REPO_PARENT %+% "analysis/common/render_jurassic_pdf.R")
library(rmarkdown)
library(tidyr)
library(tools)
library(jsonlite)


# set stringsAsFactors to FALSE, and add any other global R settings
options(stringsAsFactors = FALSE)
options(scipen=999) #avoids scientific notations
###########################



# duplicated names t1__dv__major
# t1__demog__race

# Load Data

# I cannot read the complted responses file since it has duplicated names
# I will have to manually adjust column names

#complete_file_name <- "CB17_Session_1_OFFICIAL_Oct_19_2017.csv"
#partial_file_name <- "CB17_Session_1_OFFICIAL-Responses in Progress_Oct_19_2017.csv"
complete_file_name <- "CB18_Session_1_OFFICIAL_Oct_25_2018.csv"
partial_file_name <- "CB18_Session_1_OFFICIAL-Responses in Progress_Oct_25_2018.csv"


complete_path <- util.find_crypt_paths(list(f1 = complete_file_name)) %>%  unlist
partial_path <- util.find_crypt_paths(list(f1 = partial_file_name)) %>%  unlist

compl_raw <- read.csv(complete_path)
partial_raw <- read.csv(partial_path)
partial_raw$LastActivity <- NULL # this is an extra column



# there are duplicated pdd tags. Add the question number to the tags, so they
# will become unique.
compl_raw <- rename_pdds(compl_raw, "pdd__t1__demog__race__pdd" )
compl_raw <- rename_pdds(compl_raw, "pdd__t1__dv__major__pdd" )
compl_raw <- rename_pdds(compl_raw, "pdd__t1__dv__fall2__belong__pdd" )

#


#qc.clean_qualtrics(complete_path)
compl_qc <- compl_raw %>% qc.clean_qualtrics()

# compared to complete responses. Investigate when you have time.
partial_qc <- partial_raw %>% qc.clean_qualtrics()

colnames(partial_qc) <- colnames(compl_qc)
partial_qc$partial <- TRUE
compl_qc$partial <- FALSE
s1_raw <- rbind(compl_qc,partial_qc)

x <- s1_raw %>% select(school_name, survey_id, school_name_full) %>% unique
duplicated(x$school_name) %>% table
duplicated(x$school_name_full) %>% table
x$dupl <- duplicated(x$school_name_full)
#View(x)

# source the salt
util.find_crypt_paths(list(salt = "CB17_salt.R")) %>%
  unlist %>%
  source


# neptune table
# neptune_df <- util.find_crypt_paths(list(p = "results-20171027-113558.csv")) %>%
neptune_df <- util.find_crypt_paths(list(p = "neptune_df.csv")) %>%
  unlist %>%
  read.csv

neptune_df$school_name_nept <- neptune_df$survey_params_json %>% read_school_name_json
neptune_df <- neptune_df[, c("school_name_nept", "uid", "expected_participants", "organization_id", "survey_id", "cohort_label")] %>%
  rename(.,cohort_name = uid,
         expected_n = expected_participants)
neptune_df <- neptune_df[!is.na(neptune_df$school_name_nept),]
#neptune_df$school_name_nept %>% duplicated %>% sum
#neptune_df$cohort_name %>% duplicated %>% sum

# race key
race_key <- util.find_crypt_paths(list(race_key = "CB17_race_key.csv")) %>%
  unlist %>%
  read.csv

scale_descriptions <- util.find_crypt_paths(list(sd = "CB17_scale_descriptions.csv")) %>%
  unlist %>% read.csv

#### Read in the items.csv doc
items_path <- util.find_crypt_paths(list(items = "CB17_items.csv"))
items <- read.csv(items_path$items)
#### Read expected sample size by school

# remove white spaces
items <- items %>% apply(., 2, function(x) util.trim(x)) %>% as.data.frame()
items <- items %>% apply(., 2, function(x) util.strip_non_ascii(x)) %>% as.data.frame()

# dynamic text
#dynamic_text_path <- util.find_crypt_paths(list(dt = "CB17_dynamic_text.csv"))
text_entries_path <- RMD_BASE_PATH %+% "text_entries.R"
source(text_entries_path)


##############################################################################
########################BUG FIX######################################################
# Fix duplicated organizations
### Additional repair: Some colleges are registered for 2018, but they have participated
# with their 2017 code entirely. The previous repair fixed the so-called mixed group, which
# contained both 2017 and 2018 participation codes. Now we will need to repair the pure 2017 group,
# and replace its study_id and cohort_name with the 2018 versions

# check if there are unmatched survey_ids
#s1_merged$survey_id[!s1_merged$survey_id %in% neptune_df$survey_id]

s1_merged <- s1_raw %>%
  merge(.,
        neptune_df %>% dplyr::select(organization_id, survey_id, cohort_name, cohort_label),
        by = "survey_id",
        all.x = TRUE,
        all.y = FALSE)




# check which schools have multiple survey_ids
buff_df <- s1_merged %>% group_by(organization_id) %>%
  summarise(cohort_code = paste0(unique(cohort_code), collapse = ","),
            survey_id = paste0(unique(survey_id), collapse = ","),
            cohort_label = paste0(unique(cohort_label), collapse = ","))
#View(buff_df)
rm(buff_df)

# create recode table
buff_df <- neptune_df %>%
  group_by(organization_id, cohort_label) %>%
  summarise(survey_id = first(survey_id)) %>%
  dcast(organization_id ~ cohort_label) %>%
  rename(survey_id_2018 = "2018", survey_id_2017 = "2017_fall")

recode_df <- neptune_df %>%
  group_by(organization_id, cohort_label) %>%
  summarise(cohort_name = first(cohort_name)) %>%
  dcast(organization_id ~ cohort_label) %>%
  rename(cohort_name_2018 = "2018", cohort_name_2017 = "2017_fall") %>%
  merge(buff_df, by = "organization_id", all = TRUE)

recode_df$keep <-  recode_df[,c("survey_id_2018", "survey_id_2017")] %>% apply(.,1 , function (x) all(!is.na(x)))

recode_df <- recode_df %>% dplyr::filter(keep)

#recode_df[recode_df$survey_id_2017 %in% s1_merged$survey_id, ] %>% View

s1_merged$survey_id_raw <- s1_merged$survey_id
s1_merged$cohort_name_raw <- s1_merged$cohort_name


s1_merged$survey_id <- s1_merged$survey_id %>%
  util.recode(recode_df$survey_id_2017, recode_df$survey_id_2018)

s1_merged$cohort_name <- s1_merged$cohort_name %>%
  util.recode(recode_df$cohort_name_2017, recode_df$cohort_name_2018)

# check if the replacement resulted in any peculiarities
buff_df <- s1_merged %>% group_by(survey_id) %>%
  summarise(survey_id_raw = paste0(unique(survey_id_raw), collapse = ", "),
            cohort_label = paste0(unique(cohort_label), collapse = ", "),
            school_name_full = first(school_name_full)) %>%
  mutate(same_survey_id = survey_id == survey_id_raw)

# there are 3 colleges which participated with both 2017 and 2018 codes, and one college which was
# registered for 2018, but participated only with 2017 codes. There is also one which participated only with 2017 codes, and
# was not registered for 2018
#View(buff_df)


# remove survey_ids which after the recoding still do not have 2018 version

select_vect <- (s1_merged$survey_id %>% unique) %in% (neptune_df %>% dplyr::filter(cohort_label == "2018") %>% select(survey_id) %>% unlist)
(s1_merged$survey_id %>% unique)[!select_vect]
# two survey_ids will be excluded, one was never registered, the other is n of 1, probably testing
survey_ids_2018 <- neptune_df %>% dplyr::filter(cohort_label == "2018") %>% select(survey_id) %>% unlist %>% unname
cohort_names_2018 <- neptune_df %>% dplyr::filter(cohort_label == "2018") %>% select(cohort_name) %>% unlist %>% unname

dim(s1_merged)
s1_merged <- s1_merged %>% dplyr::filter(
  (survey_id %in% survey_ids_2018)
)
dim(s1_merged) # 25 records are gone

##################### END REPAIR THE DUPLICATED ORGANIZATIONS ISSUE



##### De-identify ############################################################

s1_di <- s1_merged

# @todo: deidentifiy later

###############################################################
############ Clean columns ####################################

#### Standardize column names
s1_rn <- s1_di

# From Alice: Q102 should be t1__dv__fall2__belong, while Q103 should be t1__dv__fall3__at_home.
#s1_rn <- s1_rn %>% rename(t1__dv__fall2__belong = t1__dv__fall2__belong_Q102)
#s1_rn <- s1_rn %>% rename(t1__dv__fall3__at_home = t1__dv__fall2__belong_Q103)
# These do not work anymore, here are the correct ones
s1_rn <- s1_rn %>% rename(t1__dv__fall2__belong = t1__dv__fall2__belong_Q8.3)
s1_rn <- s1_rn %>% rename(t1__dv__fall3__at_home = t1__dv__fall2__belong_Q8.4)

#### Replace non-ascii and typos in school names
(s1_rn$school_name) %>% unique %>% sort
(s1_rn$school_name_full) %>% unique %>% sort
(neptune_df$school_name_nept) %>% unique %>% sort

old_vals <- c(
  "St. Mary&#39;s University",
  "University of Richmond-&quot;Belonging in Transition&quot;",
  "North Carolina Agricultural &amp; Technical State University",
  "IUPUI"
)

new_vals <- c(
  "St. Mary's University",
  'University of Richmond',
  "North Carolina Agricultural & Technical State University",
  "Indiana University-Purdue University Indianapolis"
)
s1_rn$school_name_full <- s1_rn$school_name_full %>% util.recode(old_vals, new_vals)

old_vals <- c(
  "CIS",
  "Minnesota State Mankato",
  "NCA&amp;T"
)

new_vals <- c(
  "University of Pennsylvania",
  "Minnesota State, Mankato",
  "NCA&T"
)
s1_rn$school_name <- s1_rn$school_name %>% util.recode(old_vals, new_vals)




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
  "race_8",
  "race_9",
  "race_10",
  "race_11",
  "race_12",
  "race_13",
  "race_14",
  "race_15",
  "race_16",
  "race_17",
  "race_18"
)


names(s1_rn)[grepl("t1__demog__race", names(s1_rn))]
# the or_race_cols have x in them for some reason, so add that
or_race_cols <- paste0("t1__demog__", race_cols) %>%
  gsub("_([0-9]+)", "_Q19.1_\\1", .) # in 2017 it used to be Q62

for (col in or_race_cols) {
  suffix <-  gsub("t1__demog__race_Q19.1_", "", col) # in 2017 it used to be Q62
  s1_rn[,col][!is.na(s1_rn[,col])] <- suffix
}


sep=","
s1_rn$t1__demog__race_1_to_18 <- apply( s1_rn[,or_race_cols], 1 , paste0 , collapse = sep)
#remove NA if present
s1_rn$t1__demog__race_1_to_18 <- gsub(",NA", "",s1_rn$t1__demog__race_1_to_18)
s1_rn$t1__demog__race_1_to_18 <- gsub("NA,", "",s1_rn$t1__demog__race_1_to_18)

# compute how many race columns are non-NAs
s1_rn$t1__demog__race_count <- apply( s1_rn[,or_race_cols], 1 , function(x) sum(is.na(x)))
s1_rn$t1__demog__race_count <- 18 - s1_rn$t1__demog__race_count


# the logic for creting new vars from the disadvantaged-type is available here:
# https://docs.google.com/document/d/1XI06XX4Mf72Uu1GgxxxkKruxAeYnI_Bwa8ogw-Cg0Hs/edit

s1_rn$cb17_race <- NA

cond_vect <- s1_rn$t1__demog__race_count == 1
s1_rn$cb17_race[cond_vect] <- s1_rn$t1__demog__race_1_to_18[cond_vect] %>%
  util.recode(
    race_key[race_key$race_var == "multiple","level"],
    race_key[race_key$race_var == "multiple","race"]
  )

# Students will also have a t1__demog__identifying_race value if they selected
# more than one response for t1__demog__race. If t1__demog__identifying_race
# != NA, we override cb17_race with values
cond_vect <- s1_rn$t1__demog__race_count > 1 & !is.na(s1_rn$t1__demog__identifying_race)
s1_rn$cb17_race[cond_vect] <- s1_rn$t1__demog__identifying_race[cond_vect] %>%
  util.recode(
    race_key[race_key$race_var == "single","level"],
    race_key[race_key$race_var == "single","race"]
  )
# If students selected multiple values for t1__demog__race but
# t1__demog__identifying_race == NA, cb17_race <- Multiracial
# overriding prior values
cond_vect <- s1_rn$t1__demog__race_count > 1 & is.na(s1_rn$t1__demog__identifying_race)
s1_rn$cb17_race[cond_vect] <- "Multiracial"
s1_rn$report_race <- s1_rn$cb17_race

s1_rn$report_race <- factor(s1_rn$report_race,
    levels = c(
                "Asian",
                "Black",
                "Latino",
                "Native",
                "Multiracial",
                "Other",
                "White",
                NA)
)



# compute gender for the appendix table
s1_rn$report_gender <- s1_rn$t1__demog__gender

# recode gender values (there are differences from cb17)
#old_vals <- c(1,2,3,4,5,6,7,8,9)
#new_vals <- c("Male","Female", "Other","Other","Other","Other","Other","Other","Other")
old_vals <- c(1,5,6)
new_vals <- c("Male","Female", "Other")
s1_rn$report_gender <- s1_rn$report_gender %>% util.recode(., old_vals, new_vals)

s1_rn$report_gender <- factor(s1_rn$report_gender,
                            levels = c(
                              "Male",
                              "Female",
                              "Other",
                              NA)
)


###############################################################################
######### create disadvantaged columns
###############################################################################

# an important point is that those without sufficient information for race and gender
# are not coded as advantaged or disadvantaged (I will use NA to code them)

# compute disadvantaged status based on the race of the participant
criteria_vect <- c("Black", "Latino", "Native")
s1_rn$disadv_race.1 <- s1_rn$cb17_race %in%  criteria_vect
s1_rn$disadv_race.1[is.na(s1_rn$cb17_race)] <- NA

criteria_vect <- c(1, 6, 7, 8, 9, 10, 11, 12, 13, 17)
race_hits <- compute_multiple_races(s1_rn$t1__demog__race_1_to_18, criteria_vect)
s1_rn$disadv_race.2 <- race_hits &
  (s1_rn$cb17_race %in%  c("Multiracial"))

s1_rn$disadv_race <- s1_rn$disadv_race.1
s1_rn$disadv_race[!s1_rn$disadv_race.1 & s1_rn$disadv_race.2 ] <- TRUE

### compute disadvantaged white based on parent's education
s1_rn$disadv_white <- NA

s1_rn$max_par_educ <- s1_rn[,c("t1__demog__ed_level_p1", "t1__demog__ed_level_p2")] %>%
  apply(., 1, function(x) max(x, na.rm = TRUE))

s1_rn$max_par_educ[!is.finite(s1_rn$max_par_educ)] <- NA

s1_rn$disadv_educ <- s1_rn$max_par_educ < 5

s1_rn$report_par_ed <- NA
s1_rn$report_par_ed[s1_rn$disadv_educ] <- "First-Generation\n (Neither parent has a\n four-year college degree)"
s1_rn$report_par_ed[!s1_rn$disadv_educ] <- "Continuing-Generation \n (At least one parent has a four-year\n college degree)"

s1_rn$report_par_ed <- factor(s1_rn$report_par_ed,
                              levels = c(
                                "First-Generation\n (Neither parent has a\n four-year college degree)",
                                "Continuing-Generation \n (At least one parent has a four-year\n college degree)",
                                NA)
)



# compute white status
criteria_vect <- c("White")
s1_rn$white.1 <- s1_rn$cb17_race %in%  criteria_vect
s1_rn$white.1[is.na(s1_rn$cb17_race)] <- NA

criteria_vect <- c(14,15,16)
race_hits <- compute_multiple_races(s1_rn$t1__demog__race_1_to_18, criteria_vect)
s1_rn$white.2 <-  race_hits & (s1_rn$cb17_race %in%  c("Multiracial"))

s1_rn$white <- s1_rn$white.1
s1_rn$white[!s1_rn$white.1 & s1_rn$white.2 ] <- TRUE


s1_rn$white <- alternative_or(s1_rn$white.1, s1_rn$white.2)

s1_rn$disadv_white_educ <- s1_rn$disadv_educ & s1_rn$white

# compute disadvant status
s1_rn$disadv_status <- s1_rn$disadv_white_educ | s1_rn$disadv_race
s1_rn$disadv_status_log <- s1_rn$disadv_status
s1_rn$disadv_status <- NA
s1_rn$disadv_status[s1_rn$disadv_status_log] <- "Disadvantaged"
s1_rn$disadv_status[!s1_rn$disadv_status_log] <- "Advantaged"
#table(s1_rn$disadv_status, s1_rn$disadv_status_log, exclude = NULL)


# check
buff_df <- s1_rn[,c(
  "cb17_race",
  "t1__demog__race_1_to_18",
  "t1__demog__identifying_race",
  "t1__demog__ed_level_p1",
  "t1__demog__ed_level_p2",
  "disadv_educ",
  "disadv_race",
  "disadv_white",
  "disadv_white_educ",
  "disadv_status",
  "white"
)]
#buff_df %>% View
###############################################################################
#### Compute composites #######


var_list <- c(
  "t1__dv__fall1__fit_in",
  "t1__dv__fall2__belong",
  "t1__dv__fall3__at_home"
)

s1_rn$y1_belonging <- rowMeans(s1_rn[ ,var_list], na.rm = TRUE)

var_list <-
  c("t1__dv__soph1__fit_in",
     "t1__dv__soph2__belong",
     "t1__dv__soph3__at_home")

s1_rn$y2_belonging <- rowMeans(s1_rn[ ,var_list], na.rm = TRUE)
s1_rn$y2_y1_belonging_diff <- s1_rn$y2_belonging - s1_rn$y1_belonging

#remove NaN from the vector
s1_rn$y2_y1_belonging_diff[is.na(s1_rn$y2_y1_belonging_diff)] <-NA

# change the dv variables to float
# otherwise the pct_good is not computed correctly
dv_vars <- items$variables
s1_rn[,dv_vars] <- s1_rn[,dv_vars]  %>% lapply(.,as.numeric)
items[,c("min_good", "max_good")] <- items[,c("min_good", "max_good")] %>%
  lapply(.,as.numeric)

#####################################################################
############ Clean rows #############################################


#### Missing participant_ids indicate test data and should be removed
s1_rc <- s1_rn %>%
  mutate(remove_blank_ID = util.is_blank(participant_id)) %>%
  filter(!util.is_blank(participant_id)) %>%
  mark_qualtrics_dups(id_vars = "participant_id")
s1_rc$remove_duplicated %>% table(., exclude = NULL)

s1_rcf <- s1_rc %>%
  filter( !remove_too_quick, !remove_duplicated)

# removing testing
s1_rcf$testing %>% table(exclude = NULL)
s1_rcf <- s1_rcf %>% dplyr::filter(testing != "true")

# ### Conduct checks on the pre-processed data
#
# #### Unique indexes
# Make sure your indexes are unique (could use [check_index_is_unique](https://gist.github.com/sarahgripshover/3192e2ce1128bb5e79fc3edc2471d9eb)).

if(!check_index_is_unique(s1_rcf, "participant_id")){
  util.warn("Duplicate participants found in survey 1 data after duplicate handling. Investigate")
}

########################################################################
###### Merge survey data with a full join ##############################

# @todo platform data

d <- s1_rcf
d_f <- filter(d, !school_name %in% c(NA))

#####################################################################
###### Identify present metrics, subset_groups, and outcomes ########mes


all_metrics <- items$variables
present_metrics <- all_metrics[all_metrics %in% names(d_f)]

all_subset_groups <- c("disadv_status")
present_subset_groups <- all_subset_groups[all_subset_groups %in% names(d_f)]

present_drivers <- items$driver %>% unique
present_drivers <- present_drivers[!util.is_blank(present_drivers)]

#progress_vars <- c("teasertxt","txtDVintro", "t1__demog__tech_difficulty") # 2017
progress_vars <- c("Q1.2","Q5.1", "t1__demog__tech_difficulty") # 2018

report_vars <- c("report_par_ed", "report_race", "report_gender", "y2_belonging", "y1_belonging")
#####################################################################
###### Loop through sites and render reports ########################

#report_sites <- unique(d_f[c("org_name", "org_uid", "pc_program_label", "pc_cohort_label", "pc_uid")])

report_sites_df <- d_f[c("survey_id", "school_name", "school_name_full")] %>%
  unique %>%
  rename(school_name_qlt = school_name,
         school_name_full_qlt = school_name_full) %>%
  mutate( dupl_survey_id = duplicated(survey_id),
          dupl_school_name_qlt = duplicated(school_name_qlt),
          dupl_school_name_full_qlt = duplicated(school_name_full_qlt)) %>%
  # remove duplicated survey_ids
  filter(!school_name_full_qlt %in% c("IUPUI")) %>%
  filter(!school_name_qlt %in% c("ROCO", "CIS")) %>%
  dplyr::select(-dupl_survey_id)

# check remaining duplicates
duplicated(report_sites_df$survey_id) %>% sum
duplicated(report_sites_df$school_name_qlt) %>% sum
duplicated(report_sites_df$school_name_full_qlt) %>% sum

report_sites_df <- merge(
  report_sites_df,
  neptune_df,
  by = "survey_id",
  all.x = TRUE,
  all.y = FALSE
)

#orphaned_surveys <- report_sites_df$survey_id[util.is_blank(report_sites_df$cohort_name)]
#x <- d_f[d_f$survey_id %in% orphaned_surveys,]
#View(x)

# add sample size
report_sites_df <- d_f %>%
  group_by(survey_id) %>%
  summarise(sample_size = n()) %>%
  merge(
    .,
    report_sites_df,
    by = "survey_id",
    all.x = FALSE,
    all.y = TRUE
  )

# rename survey ids: # Oregon State University, Wayne State University,
drop_survey_ids <- c("Survey_7m8yizCU", "Survey_Xq9mlSc0", "Survey_DXlcVA5g")
keep_survey_ids <- c("Survey_f2SfMwKL", "Survey_FbKlsTS8", "Survey_RAdEI0CA")

# survey_ids from small subsemples will be merged to the main sample for that school
d_f$survey_id <- d_f$survey_id %>% util.recode(drop_survey_ids, keep_survey_ids)

# remove those records from the reports_sites_df
report_sites_df <- report_sites_df %>% filter(!survey_id %in% drop_survey_ids)

(unique(d_f$survey_id) %in% report_sites_df$survey_id) %>% table
# drop two schools for which we have no neptune records
report_sites_df <- report_sites_df %>% filter(!util.is_blank(cohort_name))

# check if we ended up with duplicated participants
d_f$participant_id %>% duplicated %>% table
# probably that is imposible by design (the same participant is never part
# of the same survey)

# change the school_name for one of the schools "Survey_XRbcRd4F"
d_f$school_name[d_f$survey_id == "Survey_XRbcRd4F"] %>% unique

d_f$school_name <- d_f$school_name %>% util.recode("ROCO", "Rosemont College")



# remove blank or test schools
d_f <- d_f[!util.is_blank(d_f$school_name),]
#d_f <- d_f[!(d_f$school_name %in% c("arnrow u", "vgsdvs")),]
d_f <- d_f[!(d_f$school_name %in% c("vgsdvs")),]
#

#report_sites <- unique(d_f[,c("school_name")])
#report_sites_full_name <- unique(d_f[,c("school_name_full")])
#report_sites_df <- unique(d_f[c("school_name_full", "school_name_full")])


# create a log data frame

log_df <- report_sites_df
log_df$total_n <-NA
log_df$disadv_n <- NA
log_df$adv_n <- NA
log_df$report_created <- NA
log_df$error_msg <- NA

#keep track of appendix tables
appendix_tbl_full <- data.frame()




# save the current_state of the data as a separate file, so it can be used by
# k_anonymization script
data_out_path <- complete_path %>% gsub(complete_file_name, "input_data_k_anonym.csv",.)
write.csv(d_f, data_out_path, row.names = F)


if( ANONYMOUS) {

  tryCatch({
    file_path <- util.find_crypt_paths(list(anon_file = "anonymous_sites.csv"))

    anon_df <- read.csv(file_path$anon_file)
  }, error = function(e){
    stop("cannot read/find " %+% file_path$anon_file)
  })

  # Check if any of the requested sites is missing
  if (any(!anon_df$site %in%  neptune_df$cohort_name)) {
    sites <- anon_df$site[!anon_df$site %in%  report_sites_summary$cohort_name]
    (" The following sites set for anonymization were not found in the data: " %+%
        paste0(sites, collapse = ", ")
    ) %>% stop()
  }
}


if(SINGLE_RUN){
  report_sites_df <- report_sites_df[1,]
}

# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}
report_sites_df_orig <- report_sites_df
error_cohorts <- c("ProjectCohort_IK4orZS7", "ProjectCohort_RtGHnOxS", "ProjectCohort_6P3iPRYj")
error_cohorts <-  c("ProjectCohort_6P3iPRYj",
                    "ProjectCohort_dlKbQ8SB",
                    "ProjectCohort_IK4orZS7",
                    "ProjectCohort_bZ2xOaiH")
#report_sites_df <- report_sites_df %>% dplyr::filter(cohort_name %in% error_cohorts)


#for(i in sample(1:nrow(report_sites_df),5)){
for(i in 1:nrow(report_sites_df)){

  org_name <- report_sites_df[i,"school_name_qlt"]
  org_name_full <- report_sites_df[i,"school_name_full_qlt"]
  org_cohort_name <- report_sites_df[i,"cohort_name"] %>% unname
  survey_id <- report_sites_df[i,"survey_id"]


  if (ANONYMOUS) {
    if (!org_cohort_name %in% anon_df$site){
      next
    } else {
      org_cohort_name <- org_cohort_name %+% "_anon"
    }
  }
  rmd_path <- RMD_BASE_PATH %+% "cb18/cb18_report.Rmd"
  report_name <- org_cohort_name %+% ".html" %>% gsub(" ", "_", .)
  report_path   <- RMD_BASE_PATH %+% "cb18/reports/" %+% report_name





  paste0("Running ",report_name ) %>%
    message

  possible_error_msg <- NA
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
        possible_error_msg <<- e

      }
    )
  log_df$error_msg[log_df$cohort_name == org_cohort_name] <- possible_error_msg %>% unlist %>% unname %>% paste0(.,collapse = "; ")

  if(PDF){
    render_jurassic_pdf(report_path, production = PRODUCTION_QUALITY)
  }
  log_df$report_created[log_df$survey_id == survey_id] <- 1
  appendix_tbl_full <- bind_rows(appendix_tbl_full, appendix_tbl)
}
# turn off logging
closeAllConnections()

# Set back to regular interactive mode settings
interactive <- base::interactive

# add neptune codes to log_df
#log_df <- merge(log_df, neptune_df, by.x = "survey_id", by.y = "survey_id", all = T)
# keep track if a file was created
log_df$html_saved <- NA
log_df$pdf_saved <- NA
for (i in 1:nrow(log_df)){
  f_name =  RMD_BASE_PATH %+% 'cb18/reports/'  %+% log_df$cohort_name[i]  %+%  '.html'
  if(file.exists(f_name)) {
    log_df$html_saved[i] <- TRUE
  }
  f_name =  RMD_BASE_PATH %+% 'cb18/reports/'  %+% log_df$cohort_name[i]  %+%  '.pdf'
  if(file.exists(f_name)) {
    log_df$pdf_saved[i] <- TRUE
  }
}
# add gender and race decomposition
log_df <- d_f %>%
  group_by(report_race, survey_id) %>%
  summarise(freq = n()) %>%
  dcast(survey_id ~ report_race) %>%
  merge(.,log_df, by = "survey_id", all.x = F, all.y = T)


log_df <- d_f %>%
  group_by(report_gender, survey_id) %>%
  summarise(freq = n()) %>%
  dcast(survey_id ~ report_gender) %>%
  merge(.,log_df, by = "survey_id", all.x = F, all.y = T)

# from the appendix table, compute which categories are already present
log_df <- appendix_tbl_full[appendix_tbl_full$group %>% grepl("BOLD",.),] %>%
  select(group, survey_id) %>%
  unique %>%
  group_by(survey_id) %>%
  summarise(categories = paste0(group, collapse =", ")) %>%
  merge(.,log_df, by = "survey_id", all.x = F, all.y = T)



#check which report is saved as an html
write.csv(log_df, RMD_BASE_PATH %+% 'cb18/log_df.csv', row.names = F)
write.csv(appendix_tbl_full, RMD_BASE_PATH %+% 'cb18/appendix_tbl.csv', row.names = F)


