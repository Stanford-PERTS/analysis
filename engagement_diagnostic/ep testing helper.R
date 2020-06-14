# EP testing helper
# Set up for Dan's directory path, but will probably work for you? :-)

# Run all this before the inside of the metascript

setwd("~/Sites/analysis/rserve")
modules::import('tidyr')
modules::import('jsonlite')
modules::import('readr')
modules::import('stats')
modules::import('dplyr')
modules::import('stringr')
modules::import('reshape2')
modules::import('ggplot2')

github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"
source(paste0(github_base_path,"R/util.R"), local = TRUE)
source(paste0(github_base_path,"R/util_qualtrics_cleaning.R"), local = TRUE)
source(paste0(github_base_path,"R/util_graphing.R"), local = TRUE)

modules::use('scripts/copilot/engagement_helpers.R', attach = TRUE)

logging <- modules::use('common/logging.R')

# Load data
data_paths <- util.find_crypt_paths(list(input_data_path =
                                           "EP dashboard data 2018-2019/rserve_data/rds"))
metascript_args <- readRDS(data_paths$input_data_path %+% "/metascript_args.rds")
list2env(metascript_args, globalenv())

create_report <- modules::use('scripts/copilot/create_report.R')$create_report
create_active_rus_summary <- modules::use('scripts/copilot/public_summaries.R')$create_active_rus_summary
create_req_deliv_summary <- modules::use('scripts/copilot/public_summaries.R')$create_req_deliv_summary
create_msg <- modules::use('scripts/copilot/public_summaries.R')$create_msg


# If needed: import 2018-2019 EP triton tables
# triton_table_path <- util.find_crypt_paths(list(triton_table_path =
#                                            "EP dashboard data 2018-2019/2018-2019 Triton tables"))$triton_table_path
# class_tbl <- read.csv(triton_table_path %+% "/classroom_2018_2019.csv") %>%
#   dplyr::rename(class_name = name,
#                 class_id = uid)
# class_tbl$class_name <- util.trim(class_tbl$class_name)
# cycle_tbl <- read.csv(triton_table_path %+% "/cycle_2018_2019.csv")
# team_tbl <- read.csv(triton_table_path %+% "/team_2018_2019.csv") %>%
#   dplyr::rename(team_name = name,
#                 team_id = uid)
# team_tbl$team_name <- util.trim(team_tbl$team_name)
# triton_participant_tbl <- read.csv(triton_table_path %+% "/participant_2018_2019.csv")
# user_tbl <- read.csv(triton_table_path %+% "/user_2018_2019.csv") %>%
#   dplyr::rename(user_id = uid)
# triton_tbl <- merge(
#   class_tbl,
#   team_tbl,
#   by = "team_id",
#   all.x = TRUE,
#   all.y = FALSE
# )
# triton_tbl <- triton_tbl[, c("team_name", "class_name", "code", "num_students", "team_id", "class_id")]
# triton_tbl <- dplyr::rename(triton_tbl, expected_n = num_students)


# Optional: only request reports for a specific team
# TEAMS_LIST <- c("Team_eMJCXlafjT8romUS")
TEAMS_LIST <- NULL
requested_rus <- NULL
run_program <- "ep19"


# REPORT_DATE <- "2019-04-01"
REPORT_DATE <- lubridate::ymd(Sys.Date())

