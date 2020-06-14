# check if reports are uploaded online
# the beginning is identical with the metascript





##############################################################################
## Load Libraries and Paths

options(stringsAsFactors = FALSE)
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"

library(lubridate)
library(knitr)
library(markdown)
library(rmarkdown)
library(purrr)
library(tidyr)

tryCatch({
  source("~/Sites/gymnast/R/util.R")
  source("~/Sites/gymnast/R/util_data_summaries.R")
  source("~/Sites/gymnast/R/util_qualtrics_cleaning.R")
  source("~/Sites/gymnast/R/util_graphing.R")
  source("~/Sites/gymnast/R/util_scale_computation.R")
}, error = function(e){
  source(github_base_path %+% "R/util.R")
  source(github_base_path %+% "R/util_data_summaries.R")
  source(github_base_path %+% "R/util_qualtrics_cleaning.R")
  source(github_base_path %+% "R/util_graphing.R")
  source(github_base_path %+% "R/util_scale_computation.R")
})
RMD_BASE_PATH       <- "~/Sites/perts_analyses/engagement_diagnostic/"
REPORT_BASE_PATH    <- paste0(RMD_BASE_PATH, "reports/")
PDF_GENERATOR_PATH  <- "~/Sites/pdf-generator/"
SQL_CONNECT_FILE <- paste0(RMD_BASE_PATH, "sql_connect.R")
QLTR_API_FILE <- paste0(RMD_BASE_PATH, "qualtrics_api.R")

#find crypt and repository folders
home_dirs <- util.list_dirs('~')
REPOSITORY_FOLDER <- home_dirs[grep("engagement_diagnostic$", home_dirs)]


# set working directory to repo
setwd(RMD_BASE_PATH)

# load helper functions and text
helper_path         <- RMD_BASE_PATH %+% "engagement_helpers.R"

source(helper_path)

source(SQL_CONNECT_FILE) # this runs the script for downlowading information from

write.csv(reports_df, RMD_BASE_PATH %+% "reports_online.csv", row.names = FALSE)
