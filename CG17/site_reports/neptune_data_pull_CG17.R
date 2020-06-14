# downloads platform data for the CG17 reports

setwd('~/Sites/analysis')
source("~/Sites/gymnast/R/util.R", chdir = TRUE)
source('common/big_pipe.R')
source('common/sql_connect.R')

# Sourcing this file will define the variable `project_credentials`, required
# for BigPipe. The file must be in a mounted crypt.
crypt_paths <- util.find_crypt_paths(list(creds = 'neptune_credentials.R'))
source(crypt_paths$creds)
CRYPT_FOLDER <- crypt_paths %>% gsub("neptune_credentials/neptune_credentials.R", "",.)
#READ_DATE <- Sys.Date() # as.Date("2018-03-19")
READ_DATE <- as.Date("2018-04-10")

# These files must also be in a mounted crypt, required for sql.connect.
ssl_file_names <- list(
  key = "neptune_sql_production-01-analysis-replica.key",
  cert = "neptune_sql_production-01-analysis-replica.cert",
  ca = "neptune_sql_production-01-analysis-replica.ca"
)

# Datastore
tables <- big_pipe('neptuneplatform', project_credentials, download = TRUE)

tables_ls <- list("ProjectCohort" = "project_cohort_",
                  "Survey" = "survey_")
for (table_name in names(tables_ls)) {
  df <- tables[[table_name]] %>% flatten(., recursive = TRUE)
  names(df) <- names(df) %>% gsub("key__\\.","key__",.) %>% gsub("key__", "key___",.)
  #for (x in names(df)){
  #  y <- df[,x]
  #  #print(x)
  #  print(typeof(y))
  #}
  # one of the variables is not flattened yet, I wil change it manually
  df$'__error__' <- df$'__error__' %>% as.character()
  file_path <- CRYPT_FOLDER %+% tables_ls[[table_name]] %+% READ_DATE %+% ".csv"
  write.csv(df, file_path, row.names = F)
}


# compare old and new project_cohort data
#pc_new <- read.csv(CRYPT_FOLDER %+% "project_cohort_2018-04-10.csv", stringsAsFactors = F)
#pc_old <- read.csv(CRYPT_FOLDER %+% "project_cohort_2018-03-19.csv", stringsAsFactors = F)

# compare new and old files
#dim(pc_new)
#dim(pc_old)
#names(pc_old)[!names(pc_old) %in% names(pc_new)]
# the only problem is with X__key___id, which has NAs only, so it seems not important
#(pc_old$uid %in% pc_new$uid) %>% table
#pc_old[!(pc_old$uid %in% pc_new$uid),] # there are 4 records missing in the new table
# two of them are from cg17, from single organization Organization_DGKDuM6n
#tables$Organization[tables$Organization$uid == "Organization_DGKDuM6n",]
# the status is rejected, so I assume that this organization was removed

