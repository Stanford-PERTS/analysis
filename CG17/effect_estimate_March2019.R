
##############################################################################
################# some inputs to the parameters in the impact formula:########
##############################################################################

# crypt is PERTS encrypted data/CG17/Neptune participation March 2019 analyses.vc
# plus the data store if you're reconstructing that.
# But I saved a merged dataset called merged_participation in the crypt too.

source("~/Sites/gymnast/R/util.R", chdir = TRUE)
source("~/Sites/gymnast/R/util_qualtrics_cleaning.R", chdir = TRUE)
setwd("~/Sites/analysis") # for unknown reasons, big_pipe seems to need a relative path to source properly???
source("~/Sites/analysis/common/big_pipe.R")
source("~/Sites/analysis/common/sql.R")

crypt_paths <- util.find_crypt_paths(list(creds = 'neptune_credentials.R'))
if(length(crypt_paths) == 0) stop("your Neptune credintials crypt is not loaded.")
source(crypt_paths$creds)

# These files must also be in a mounted crypt, required for sql.connect.
ssl_file_names <- list(
    key = "neptune_sql_production-01-analysis-replica.key",
    cert = "neptune_sql_production-01-analysis-replica.cert",
    ca = "neptune_sql_production-01-analysis-replica.ca"
)


# Datastore
tables <- big_pipe('neptuneplatform', project_credentials, download = TRUE)

# Nash's tables
nash_table_files <- util.find_crypt_paths(list(cb = "CB17 Tracking - Overall.csv",
                                               cg = "CG17 Tracking - Overall.csv"))

nash_tables <- util.read_csv_files(nash_table_files)

# Qualtrics
files <- util.find_crypt_paths(list(CG17 = "CG17_OFFICIAL_1.csv",
                                    CG18 = "CG18_OFFICIAL_1.csv",
                                    CB17 = "CB17_Session_1_OFFICIAL.csv",
                                    CB18 = "CB18_Session_1_OFFICIAL.csv"))
qualtrics <- util.read_csv_files(files) %>%
    lapply(., qc.clean_qualtrics)
qualtrics$CG17$code <- "cg17"
qualtrics$CG18$code <- "cg18"
qualtrics$CB17$code <- "cb17"
qualtrics$CB18$code <- "cb18"

qualtrics_df <- util.rbind_union(qualtrics)
ncol(qualtrics_df) # a ton of cols: 992
qnames <- names(qualtrics_df)
q_race_cols <- qnames[grep("race", qnames)]
q_gender_cols <- qnames[grep("gender", qnames)]
q_ed_level_cols <- qnames[grep("ed_level|edlevel", qnames)]

qualtrics_df_restricted <- qualtrics_df[
    c(q_race_cols, q_gender_cols, q_ed_level_cols,
      "survey_id", "ResponseID", "participant_id", "code")
]

table(duplicated(qualtrics_df_restricted[c("survey_id", "participant_id")]))
# a couple of dups. Don't worry about them.
# FALSE   TRUE 
# 146982    824 

qualtrics_df_filtered <- qualtrics_df_restricted[!duplicated(qualtrics_df_restricted[c("survey_id", "participant_id")]), ]
table(duplicated(qualtrics_df_filtered[c("survey_id", "participant_id")]))

organization <- tables$Organization
project_cohort <- tables$ProjectCohort

survey <- tables$Survey %>%
    filter(program_label %in% c("cb17", "cg17"))

# merge Nash tables with organization table from Neptune.
# we will want to keep orgs with no matches, but 
nash_tables_df <- util.rbind_intersection(nash_tables)
nash_tables_df$OPE.ID <- util.trim(nash_tables_df$OPE.ID)
nash_tables_df$College <- util.strip_special_characters(nash_tables_df$College)
nash_tables_df$College <- util.trim(nash_tables_df$College)

nash_tables_limited <- unique(nash_tables_df[c("OPE.ID", "X2.or.4.year")]) %>%
    filter(!util.is_blank(OPE.ID),
           !OPE.ID %in% "N/A")

# now need to link the orgs to the survey data...how do you do this again?

# start by merging surveys to organizations

s_org <- merge(
    survey,
    organization,
    by.x = "organization_id",
    by.y = "uid"
)

nrow(s_org) # 904, 2 rows dropped rows from survey which had 906.
table(duplicated(s_org$uid)) # FALSE. This is the value that will link to Qtrics

## Then merge in the Nash values
s_org_nash <- merge(
    s_org,
    nash_tables_limited,
    by.x = "ope_id",
    by.y = "OPE.ID",
    all.x = TRUE,
    all.y = FALSE
)

nrow(s_org_nash) == nrow(s_org) # TRUE. 904 rows.

# now link to survey data

s_org_nash_restricted <- s_org_nash[c("uid", "X2.or.4.year", "ope_id", "project_id", "cohort_label")]
any(duplicated(s_org_nash_restricted)) # FALSE

# uid is the survey id. This is the value that should link to Qualtrics surveys.
d <- merge(
    s_org_nash_restricted,
    qualtrics_df_filtered,
    by.x = "uid",
    by.y = "survey_id"
)

nrow(d) # 146975
nrow(s_org_nash_restricted) # 904
nrow(qualtrics_df_filtered) # 146979
nrow(d) - nrow(qualtrics_df_filtered) # four rows dropped. meh.

names(d)
table(duplicated(d$participant_id))
# FALSE   TRUE 
# 143253   3722 
# Probably because students could complete multiple programs. We want unique participants though.

# just filter it arbitrarily for now. We aren't really worried about program anyway.
d_filtered <- d[!duplicated(d$participant_id), ]

# create a combined race variable
race_values_revised <- c("AA", "Asian", "Lat", "ME", "Nat", "PI", "Eu", "Oth")
race_values_not_revised <- c("AA", "Asian", "Nat", "ME", "PI", "Eu", "Oth")

old_race_cols_revised <- names(d_filtered)[grep("race_revised\\.[0-9]+$", names(d_filtered))]
new_race_cols_revised <- gsub("\\.[0-9]+", "", old_race_cols_revised) %+% "_" %+% race_values_revised

old_race_cols_not_revised <- names(d_filtered)[grep("^race\\.[0-9]+$", names(d_filtered))]
new_race_cols_not_revised <- gsub("\\.[0-9]+", "", old_race_cols_not_revised) %+% "_" %+% race_values_not_revised

d_rn <- d_filtered
names(d_rn) <- util.recode(names(d_rn), old_race_cols_revised, new_race_cols_revised)
names(d_rn) <- util.recode(names(d_rn), old_race_cols_not_revised, new_race_cols_not_revised)

d_rc <- d_rn


# In this section, we will also determine for each participant
# which race question they answered, and keep their response to that question
# in order to combine both versions into a single variable about race.
d_rc <- d_rn
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
# Now fix the CTC race cols

# these are ordered based on the survey
ctc_race_cats <- c("Nat", 
                   "Asian", "Asian", "Asian", "Asian",
                   "Lat", "Lat", "Lat", "Lat", 
                   "AA", "AA", "AA", "AA",
                   "Eu", "ME", "Eu",
                   "PI", "Oth")

all(is.na(d_rc$race_simplified[d_rc$code %in% c("cb17", "cb18")])) # TRUE. We're going to fill these in.

AA_indexes <- which(ctc_race_cats == "AA")
d_rc$race_AA[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", AA_indexes, sep = ".")
    ] %>% 
    apply(., 1, function(x) sum(x, na.rm = T))

Asian_indexes <- which(ctc_race_cats == "Asian")
d_rc$race_Asian[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", Asian_indexes, sep = ".")
    ] %>% 
    apply(., 1, function(x) sum(x, na.rm = T))

Lat_indexes <- which(ctc_race_cats == "Lat")
d_rc$race_Lat[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", Lat_indexes, sep = ".")
    ] %>% 
    apply(., 1, function(x) sum(x, na.rm = T))

Eu_indexes <- which(ctc_race_cats == "Eu")
d_rc$race_Eu[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", Eu_indexes, sep = ".")
    ] %>% 
    apply(., 1, function(x) sum(x, na.rm = T))

Nat_indexes <- which(ctc_race_cats == "Nat")
d_rc$race_Nat[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", Nat_indexes, sep = ".")
    ] 

ME_indexes <- which(ctc_race_cats == "ME")
d_rc$race_ME[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", ME_indexes, sep = ".")
    ] 

PI_indexes <- which(ctc_race_cats == "PI")
d_rc$race_PI[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", PI_indexes, sep = ".")
    ]

Oth_indexes <- which(ctc_race_cats == "Oth")
d_rc$race_Oth[d_rc$code %in% c("cb17", "cb18")] <- d_rc[
    d_rc$code %in% c("cb17", "cb18"),
    paste("t1__demog__race", Oth_indexes, sep = ".")
    ]

# remove the NAs
race_cols_all <- paste("race", race_values_revised, sep = "_")
for(col in race_cols_all){
    d_rc[[col]] <- ifelse(is.na(d_rc[[col]]), 0, 1)
}

# compute the simplified categories
d_rc$race_simplified <- NA
d_rc$race_simplified[d_rc$race_Asian | d_rc$race_Eu] <- "White/Asian"
d_rc$race_simplified[d_rc$race_AA | d_rc$race_Lat | d_rc$race_Nat | d_rc$race_Oth | d_rc$race_PI | d_rc$race_ME] <- "Blk/Lat/Oth"
d_rc$race_disadv <- d_rc$race_simplified %in% "Blk/Nat/Lat/Oth"

write.csv(d_rc, "/Volumes/untitled/merged_participation.csv", row.names = FALSE)

############################################
### Descriptives ###
############################################

demog_summary_college <- d_rc %>%
    group_by(X2.or.4.year) %>%
    summarise(
        N = n(),
        proportion = round(N/nrow(d_rc), 2)
    )

demog_summary_race <- d_rc %>%
    group_by(race_simplified) %>%
    summarise(
        N = n(),
        proportion = round(N/nrow(d_rc), 2)
    )

############################################
### Basic formula ###
############################################
# Results from RCTs typically show effects on persistence of 3-5% 
# points on persistence outcomes, and those effects tend to be limited 
# to students from underserved groups. In cases where the sample consists
# of community college students enrolled in developmental or skills courses
# (and thus is nearly entirely composed of under-served students), main 
# effects have been observed for all participating students 
# (Paunesku et al., 2017). In samples from four-year colleges, 
# effects tend to be restricted to underserved sub-groups of students, 
# such as first-generation students and students from historically 
# underrepresented minority backgrounds (Yeager et al., 2016; 
# Broda et al., 2018; Walton & Cohen, 2011). We therefore do not 
# apply the 3-5% point improvement rate to the entire sample of 
# students who participated in the GMCS or SBCS programs. Rather, 
# we estimate conservatively that 50% of students who participated 
# through our 2-year college partners, and 30% of students who 
# participated through our 4-year college partners, would fall into 
# the underserved student groups that are most likely to be affected 
# by these programs.

.45 * nrow(d_rc) * .03
.45 * nrow(d_rc) * .05
 