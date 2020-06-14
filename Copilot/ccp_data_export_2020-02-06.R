# CCP data export script by CAM.
#
# 1. Mount perts_crypt.vc
# 2. Set working directory to local analysis repo.
# 3. Run this script.
#
# You should now have two dataframes: `ccp_response_wide` and
# `ccp_qualtrics`


################################################
###
###   Set working directory
###
################################################

wd <- getwd()
if(grepl("paunesku",wd)){
  setwd("~/Documents/GitHub/analysis/")
}

# Ensure correct working directory.
if (!grepl('/analysis/?$', getwd())) {
  stop("Your working directory should be the root of the analysis repo.")
}


################################################
###
###   Load libraries and functions
###
################################################


# Load modules.
gymnast_url <- "https://raw.githubusercontent.com/PERTS/gymnast/master/R/"
gymnast <- new.env()
source(paste0(gymnast_url, "util.R"), local = gymnast)
# Gymnast still doesn't have proper dplyr:: prefixing, grumble grumble.
modules::import('dplyr', `%>%`, 'group_by', 'mutate')
modules::import('stringr', 'str_extract')
source(paste0(gymnast_url, "util_qualtrics_cleaning.R"), local = gymnast)
source(paste0(gymnast_url, "util_graphing.R"), local = gymnast)
json_utils <- modules::use('common/json_utils.R')
qualtrics <- modules::use('common/qualtrics.R')
sql <- modules::use('common/sql.R')


################################################
###
###   Download data
###
################################################


# Contants.
TRITON_ANALYSIS_REPLICA_IP <- ''  # production-01-analysis-replica
QUALTRICS_SURVEY_ID <- 'SV_7VhdOnQLdNXw81D'  # 2019-20
CCP_PROGRAM_ID <- 'Program_RSGXIjImewxZIsiN'

# Get data from Triton/Copilot.
conn <- sql$connect(
  TRITON_ANALYSIS_REPLICA_IP,
  dbname = 'triton',
  ssl_file_names = list(
    key = 'triton_sql_production-01-analysis-replica.key',
    cert = 'triton_sql_production-01-analysis-replica.cert',
    ca = 'triton_sql_production-01-analysis-replica.ca'
  ),
  mysql_user = 'readonly'
)
# Get all CCP responses (e.g. )
ccp_response <- sql$query(
  conn,
  paste0("
    SELECT r.* FROM `response` r
    JOIN `team` t on r.`team_id` = t.`uid`
    WHERE t.`program_id` = '", CCP_PROGRAM_ID, "'
  ")
)
# Get all the classroom codes in CCP so we can filter Q data later.
ccp_codes <- sql$query(
  conn,
  paste0("
    SELECT c.`code` FROM `team` t
    JOIN `classroom` c on t.`uid` = c.`team_id`
    WHERE t.`program_id` = '", CCP_PROGRAM_ID, "'
  ")
) %>% unlist()


# classrooms
ccp_classrooms <- sql$query(
  conn,
  paste0("
    SELECT c.* FROM `team` t
    JOIN `classroom` c on t.`uid` = c.`team_id`
    WHERE t.`program_id` = '", CCP_PROGRAM_ID, "'
  ")
)

cycles_table <- sql$query(
  conn,
  paste0("
    SELECT * FROM `cycle` c
  ")
)


ccp_team_table <- sql$query(
  conn,
  paste0("
    SELECT * FROM `team` t
    WHERE t.`program_id` = '", CCP_PROGRAM_ID, "'
  ")
)


# org table needed for org names
# merge to uncompressed ccp_team_table ( ccp_team_association )
ccp_org_table <- sql$query(
  conn,
  paste0("
    SELECT * FROM `organization` o
    WHERE o.`program_id` = '", CCP_PROGRAM_ID, "'
  ")
)

sql$disconnect_all()


# Get data from Qualtrics.
creds_path <- gymnast$util.find_crypt_paths(list(
  creds = 'rserve_credential_body.json'
))$creds
q_api_key <- jsonlite::fromJSON(creds_path)$qualtrics_credentials$api_key
qualtrics_service <- qualtrics$create_service(q_api_key)
raw_qualtrics <- qualtrics_service$get_responses(QUALTRICS_SURVEY_ID)


################################################
###
###   Uncompress and widen data
###
################################################


# Uncompress/widen JSON-serialized response data
ccp_response_wide <- ccp_response %>%
  json_utils$widen_object_column(body)


ccp_team_associations <- ccp_team_table %>%
  json_utils$expand_string_array_column(organization_ids) %>%
  dplyr::rename(organization_id = organization_ids,
                team_id = uid)

# Filter qualtrics data down to just CCP.
ccp_qualtrics <- raw_qualtrics %>%
  gymnast$qc.clean_qualtrics() %>%
  dplyr::filter(code %in% ccp_codes)

# org table is ccp_org_table


# FYI interesting values of ccp_response_wide$module_label are:
#
# * CCPCoAnalysis
# * CCPCollaborativeAssessment
# * CCPTeacherIntroduction
# * CCPTeacherModule1
# * CCPTeacherModule2
# * CCPTeacherModule3

####


################################################
###
###   Map of org-team-class
###
################################################


orgs <- ccp_org_table %>%
  dplyr::rename(org_id = uid, org_name=name) %>%
  dplyr::select(org_id, org_name)

org_to_team <- ccp_team_associations %>%
  dplyr::rename(org_id = organization_id, team_name=name) %>%
  dplyr::select(org_id, team_id, team_name) %>%
  dplyr::inner_join(orgs, by="org_id")

classrooms <- ccp_classrooms %>%
  dplyr::select(code, team_id)

full_map <- org_to_team %>%
  dplyr::inner_join(classrooms, by="team_id")


################################################
###
###   Filter to desired qualtrics data.
###   Then merge in organization.
###
################################################

qualtrics_ids  <- c("code","StartDate","survey_id","participant")
qualtrics_vars <- c("tc1_2","tc2_2","tc4_2",
                    "mw1_2","mw2_2","mw3_2",
                    "fg1_2","fg2_2","fg3_2")

# make a scrambled participant id
# it will change across runs
salt <- paste0(runif(10) , collapse="")
ccp_qualtrics <- ccp_qualtrics %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    participant = digest::digest(paste0(participant_id,salt), algo="sha256") )


qual <- ccp_qualtrics[,c(qualtrics_ids, qualtrics_vars)] %>%
  dplyr::rename(survey_date = StartDate)

# convert each LC into a scale binary
qual$tc <- rowMeans(qual[,c("tc1_2","tc2_2","tc4_2")],na.rm=TRUE)
qual$fg <- rowMeans(qual[,c("fg1_2","fg2_2","fg3_2")],na.rm=TRUE)
qual$mw <- rowMeans(qual[,c("mw1_2","mw2_2","mw3_2")],na.rm=TRUE)

qual$tc_good <- ifelse(qual$tc >= 6,1,0)
qual$fg_good <- ifelse(qual$fg >= 6,1,0)
qual$mw_good <- ifelse(qual$mw >= 6,1,0)

# drop raw values - retain only the LC means
qual <- qual[, ! names(qual) %in% qualtrics_vars ]

# map dates to cycles
qual_with_teams  <- dplyr::inner_join(classrooms, qual, by="code")
qual_all_cycles <- dplyr::inner_join(qual_with_teams, cycles_table, by="team_id")
qual_matched_cycles <- qual_all_cycles %>%
  dplyr::filter( start_date <= survey_date,
                 end_date >= survey_date ) %>%
  dplyr::select(code, tc, participant, fg, mw, tc_good, fg_good,
                mw_good, ordinal, start_date, end_date, survey_date)

# remove duplicate entries per student within cycle
qual_matched_cycles <- qual_matched_cycles %>%
  dplyr::group_by(participant, ordinal) %>%
  dplyr::mutate( max_survey_date = max(survey_date) ,
                 is_last_survey = (max_survey_date == survey_date) ) %>%
  dplyr::filter( is_last_survey )

# for privacy
# filter to observations with k>4 per cycle per class
# and remove class (code) level
qual_f <- qual_matched_cycles %>%
  dplyr::group_by(code, ordinal) %>%
  dplyr::mutate( n = dplyr::n() ) %>%
  dplyr::arrange(code, ordinal) %>%
  dplyr::filter( n > 4 ) %>%
  dplyr::ungroup() %>%
  dplyr::select( code, ordinal, participant,
                 fg, fg_good, mw, mw_good, tc, tc_good )


# checking that participants are unique within cycle
qual_f %>%
  dplyr::group_by(participant, ordinal) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::arrange( -n )

# melt down the variables to cast by cycle
melt_ids <- c("participant","ordinal","code")
qual_wide <- qual_f %>%
  reshape2::melt(id.vars=melt_ids) %>%
  reshape2::dcast(participant + code ~ variable + ordinal, value="value" )

# append site level data
site_codes <- full_map %>%
  dplyr::filter( grepl("Site:", org_name) ) %>%
  dplyr::select(org_name, code )

# confirm no code is associated with > 1 site
site_codes %>%
  dplyr::group_by(code) %>%
  dplyr::summarise(n=dplyr::n()) %>%
  dplyr::filter(n > 1)

# map the site codes to the qualtrics data
qual_mapped <- dplyr::inner_join(site_codes, qual_wide, by="code") %>%
  dplyr::select(-code)

# write.csv(qual_mapped, "~/Desktop/ccp_student_surveys.csv", row.names = FALSE)




# ggplot2::ggplot(qual_f[grepl("Site:",qual_f$org_name),],
#                 aes(ordinal, fg_good, color=org_name)) +
#   geom_path(stat="summary", fun.y="mean") +
#   ylab("feedback for growth") +
#   xlab("Cycle") +
#   scale_y_continuous(labels=percent) +
#   gymnast$ug.ht

# ggplot2::ggplot(qual_f[grepl("Site:",qual_f$org_name),],
#                 aes(ordinal, tc_good, color=org_name)) +
#   geom_path(stat="summary", fun.y="mean") +
#   ylab("teacher caring") +
#   xlab("Cycle") +
#   scale_y_continuous(labels=percent) +
#   gymnast$ug.ht
#
# ggplot2::ggplot(qual_f[grepl("Site:",qual_f$org_name),],
#                 aes(ordinal, mw_good, color=org_name)) +
#   geom_path(stat="summary", fun.y="mean") +
#   ylab("meaningful work") +
#   xlab("Cycle") +
#   scale_y_continuous(labels=percent) +
#   gymnast$ug.ht






################################################
###
###   Filter to desired observation data.
###   Then merge in organization.
###
################################################

ccp_response_wide$module_label

# * CCPCoAnalysis
# * CCPCollaborativeAssessment
# * CCPTeacherIntroduction
# * CCPTeacherModule1
# * CCPTeacherModule2
# * CCPTeacherModule3

classroom_interactions$value









