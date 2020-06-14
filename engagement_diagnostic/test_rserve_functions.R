setwd("~/Sites/analysis/rserve")

# this script will test the RServe EP functions using local data
survey_file_names <- list(
  "engagement" = "Engagement_Project_OFFICIAL_1.csv" # if you change the name, chnage it in also qualtrics_api.R
)
items_file_name <- list(
    i = "items_new_data_v6.csv"
)



############### LOAD LIBRARIES AND PATHS #####################################

options(stringsAsFactors = FALSE)
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"

local_boolean = TRUE
source(paste0(github_base_path,"R/util.R"), local = local_boolean)
gymnast_install() # this shouls be added separately since now we try not to run
# anything inside the util functions (requirement for Rserve proper functioning)
source(paste0(github_base_path,"R/util_data_summaries.R"), local = local_boolean)
source(paste0(github_base_path,"R/util_qualtrics_cleaning.R"), local = local_boolean)
source(paste0(github_base_path,"R/util_graphing.R"), local = local_boolean)
source(paste0(github_base_path,"R/util_scale_computation.R"), local = local_boolean)
source(paste0(github_base_path,"R/util_dfc.R"), local = local_boolean)


packages <- c('modules','lubridate', 'markdown', 'rmarkdown',
              'purrr', 'tidyr','jsonlite', 'readr')
ensure_packages(packages)



modules::import('lubridate')
modules::import('tidyr')
modules::import('jsonlite')
modules::import('readr')






#modules::use('scripts/engagement_helpers.R', attach = TRUE)
rserve_path <- '~/Sites/analysis/rserve/scripts/'
source(paste0(rserve_path,'engagement_helpers.R'), local = TRUE)
source(paste0(rserve_path,'function_that_used_to_be_the_metascript.R'), local = TRUE)




`%>%` <- dplyr::`%>%`
RMD_BASE_PATH       <- "~/Sites/analysis/engagement_diagnostic/"
SQL_CONNECT_FILE <- paste0(RMD_BASE_PATH, "sql_connect.R")

#find crypt and repository folders
home_dirs <- util.list_dirs('~')
REPOSITORY_FOLDER <- home_dirs[grep("engagement_diagnostic$", home_dirs)]
image_files <- list(teacher_caring = "teacher_caring.png",
                    feedback_for_growth = "feedback_for_growth.png",
                    meaningful_work = "meaningful_work.png")





source(SQL_CONNECT_FILE) # this runs the script for downloading information from Triton






############### LOAD DATA #####################################################


###############################################################################
########## DATA INPUTS ######################
item_path <- RMD_BASE_PATH %+% items_file_name
google_keys_data_input <- read.csv(item_path, stringsAsFactors = FALSE) # disentange data inputs

survey_paths <- util.find_crypt_paths(survey_file_names)
qualtrics_data_input <- survey_paths %>%
  util.read_csv_files()

triton_data_input <- triton_data_input

###############################################################################

REPORT_DATE <- "2018-06-11" # must be Monday
TEAMS_LIST_DEBUG =   c("Team_UyjewaYFQwSdvkle")
ANONYMOUS = TRUE
TEAM_ONLY = FALSE

# create outputs
team_class_output_list <- function_that_used_to_be_the_metascript(
                              triton_tbl,
                              user_tbl,
                              class_tbl,
                              team_tbl,
                              qualtrics_data_input,
                              google_keys_data_input,
                              REPORT_DATE = "2018-06-11",
                              ANONYMOUS = FALSE,
                              TEAM_ONLY = FALSE,
                              TEAMS_LIST_DEBUG =   c("Team_UyjewaYFQwSdvkle"),
                              requested_rus = c("Classroom_Nfmbfaay2GfuCCI7", "Team_UyjewaYFQwSdvkle", "a")
                          )




