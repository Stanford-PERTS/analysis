
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
RMD_BASE_PATH <- REPO_PARENT %+% "analysis/cb17_reports/cb18/"
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


source(RMD_BASE_PATH %+% "cb18_helpers.R")
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
complete_file_name <- "CB18_Session_1_OFFICIAL_Oct_3_2018.csv"
partial_file_name <- "CB18_Session_1_OFFICIAL-Responses in Progress_Oct_3_2018.csv"


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

# source the salt
util.find_crypt_paths(list(salt = "CB17_salt.R")) %>%
  unlist %>%
  source

# Table from Dan F.
p_raw <- util.find_crypt_paths(list(p = "CB17_participant-org-cohort-table.csv")) %>%
  unlist %>%
  read.csv

# neptune table
neptune_df <- util.find_crypt_paths(list(p = "results-20171027-113558.csv")) %>%
  unlist %>%
  read.csv

neptune_df$school_name_nept <- neptune_df$survey_params_json %>% read_school_name_json
neptune_df <- neptune_df[, c("school_name_nept", "X__key___name", "expected_participants")] %>%
  rename(.,cohort_name = X__key___name,
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
##############################################################################
##### De-identify ############################################################

s1_di <- s1_raw
p_di <- p_raw
# @todo: deidentifiy later

###############################################################
############ Clean columns ####################################

#### Standardize column names
s1_rn <- s1_di

#From Alice: Q102 should be t1__dv__fall2__belong, while Q103 should be t1__dv__fall3__at_home.
s1_rn <- s1_rn %>% rename(t1__dv__fall2__belong = t1__dv__fall2__belong_Q102)
s1_rn <- s1_rn %>% rename(t1__dv__fall3__at_home = t1__dv__fall2__belong_Q103)

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
  gsub("_([0-9]+)", "_Q62_\\1", .)

for (col in or_race_cols) {
  suffix <-  gsub("t1__demog__race_Q62_", "", col)
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
old_vals <- c(1,2,3,4,5,6,7,8,9)
new_vals <- c("Male","Female", "Other","Other","Other","Other","Other","Other","Other")
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
  mark_qualtrics_dups(id_vars = "participant_id")
s1_rc$remove_duplicated %>% table(., exclude = NULL)

s1_rcf <- s1_rc %>%
  filter(!remove_blank_ID, !remove_too_quick, !remove_duplicated)


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

progress_vars <- c("teasertxt","txtDVintro", "t1__demog__tech_difficulty")

report_vars <- c("report_par_ed", "report_race", "report_gender", "y2_belonging", "y1_belonging")
#####################################################################
###### Loop through sites and render reports ########################

#report_sites <- unique(d_f[c("org_name", "org_uid", "pc_program_label", "pc_cohort_label", "pc_uid")])


# remove blank or test schools
d_f <- d_f[!util.is_blank(d_f$school_name),]
#d_f <- d_f[!(d_f$school_name %in% c("arnrow u", "vgsdvs")),]
d_f <- d_f[!(d_f$school_name %in% c("vgsdvs")),]
#

report_sites <- unique(d_f[,c("school_name")])
report_sites_full_name <- unique(d_f[,c("school_name_full")])

# create a log data frame
log_df <- data.frame(
  school_name = (report_sites %>% unlist %>% unname),
  school_name_full = (report_sites_full_name %>% unlist %>% unname)
)
log_df$total_n <-NA
log_df$disadv_n <- NA
log_df$adv_n <- NA
log_df$report_created <- NA
log_df$error_msg <- NA

#keep track of appendix tables
appendix_tbl_full <- data.frame()

# keep a track of rendering errors
errors_ <- c()


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
    sites <- anon_df$site[!anon_df$site %in%  report_sites_summary$project_cohort_id]
    (" The following sites set for anonymization were not found in the data: " %+%
        paste0(sites, collapse = ", ")
    ) %>% stop()
  }
}


if(SINGLE_RUN){
  report_sites <- report_sites[1,]
  report_sites_full_name <- report_sites_full_name[1,]
}

# Temporarily set to non-interactive mode (for html printing)
old_interactive <- interactive
interactive <- function() {return (FALSE)}




for(i in 1:nrow(report_sites)){

  org_name <- report_sites[i,]
  org_name_full <- report_sites_full_name[i,]
  org_cohort_name <- neptune_df$cohort_name[neptune_df$school_name == unlist(org_name)] %>%
    unlist %>%
    unname


  if (ANONYMOUS) {
    if (!org_cohort_name %in% anon_df$site){
      next
    } else {
      org_cohort_name <- org_cohort_name %+% "_anon"
    }
  }
  rmd_path <- RMD_BASE_PATH %+% "cb17_report.Rmd"
  report_name <- org_cohort_name %+% ".html" %>% gsub(" ", "_", .)
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
        errors_ <- c(errors_, e)
        log_df$error_msg[log_df$school_name == unname(unlist(org_name))] <- e
      }
    )

  if(PDF){
    render_jurassic_pdf(report_path, production = PRODUCTION_QUALITY)
  }
  log_df$report_created[log_df$school_name == unname(unlist(org_name))] <- 1
  appendix_tbl_full <- bind_rows(appendix_tbl_full, appendix_tbl)
}
# turn off logging
closeAllConnections()

# Set back to regular interactive mode settings
interactive <- base::interactive

# add neptune codes to log_df
log_df <- merge(log_df, neptune_df, by.x = "school_name", by.y = "school_name_nept", all = T)
# keep track if a file was created
log_df$html_saved <- NA
log_df$pdf_saved <- NA
for (i in 1:nrow(log_df)){
  f_name =  RMD_BASE_PATH %+% 'reports/'  %+% log_df$cohort_name.y[i]  %+%  '.html'
  if(file.exists(f_name)) {
    log_df$html_saved[i] <- TRUE
  }
  f_name =  RMD_BASE_PATH %+% 'reports/'  %+% log_df$cohort_name.y[i]  %+%  '.pdf'
  if(file.exists(f_name)) {
    log_df$pdf_saved[i] <- TRUE
  }
}

#check which report is saved as an html



write.csv(log_df, RMD_BASE_PATH %+% '/log_df.csv', row.names = F)
write.csv(appendix_tbl_full, RMD_BASE_PATH %+% '/appendix_tbl.csv', row.names = F)


######
# @todo:
# title containing - do not get a pdf file, only html. Investigate.
