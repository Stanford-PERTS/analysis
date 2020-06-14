# DEBUGGING ONLY
# this file is for testing purposes only. It loads objects saved from ep.R
# examines them, and saves them as json objects, if desired
# In emergency, it can be used to create html version of the reports.

REPORT_DATE <- "2019-04-08"
# REPORT_DATE <- lubridate::ymd(Sys.Date())


library(tidyr)
library(jsonlite)
github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"
source(paste0(github_base_path,"R/util.R"), local = TRUE)
source(paste0(github_base_path,"R/util_qualtrics_cleaning.R"), local = TRUE)
source(paste0(github_base_path,"R/util_graphing.R"), local = TRUE)

setwd("~/Sites/analysis/rserve")

data_paths <- util.find_crypt_paths(list(input_data_path =
                                           "rserve_data/rds",
                                    html_data_path = "rserve_data/html",
                                    json_data_path = "rserve_data/json"))


metascript_output <- readRDS(data_paths$input_data_path %+% "/metascript_output_workspace.rds")

report_data_list <- metascript_output$metascript_output_list$report_data_list


#
# # save all ru objects
setwd(data_paths$json_data_path)
for (ru_id in names(report_data_list) ){
 jsn_obj <- jsonlite::toJSON(report_data_list[[ru_id]], pretty=TRUE, auto_unbox = TRUE)
 writeLines(
   jsn_obj,
   paste0(ru_id, ".json")
 )
}

#write.csv(agg_metrics_small, "agg_metrics_small.csv", row.names = FALSE)
#agg_metrics_small <- read.csv("agg_metrics_small_try.csv", stringsAsFactors = FALSE)

# render the html version of the reports.



for (ru_id in names(report_data_list) ){
  command_str <- paste0(
    "~/Sites/analysis/engagement_diagnostic/jinja_template_import.py --data ",
    shQuote(
      paste0(
        data_paths$json_data_path,
        "/",
        ru_id,
        ".json"
      )
    ),
    # " --template ~/Sites/analysis/engagement_diag nostic/ep_report_NEW.html --out ",
    " --template ~/Sites/neptune/templates/ep_report.html --out ",
    shQuote(
      paste0(
        data_paths$html_data_path,
        "/",
        ru_id,
        ".",
        REPORT_DATE,
        ".html")
      )

  )
  print(command_str)
  command_str %>% system()
}

