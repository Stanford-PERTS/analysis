# downloads platform data for the HG17 reports

setwd('~/Sites/analysis')
source("~/Sites/gymnast/R/util.R", chdir = TRUE)
source('common/big_pipe.R')
source('common/sql.R')

# Sourcing this file will define the variable `project_credentials`, required
# for BigPipe. The file must be in a mounted crypt.
crypt_paths <- util.find_crypt_paths(list(creds = 'neptune_credentials.R',
                                          data_crypt = "HG17_salt.R"))
source(crypt_paths$creds)
CRYPT_FOLDER <- crypt_paths$data_crypt %>% gsub("HG17_salt.R", "",.)
#READ_DATE <- Sys.Date() # as.Date("2018-03-19")
READ_DATE <- as.Date("2019-04-29")

# These files must also be in a mounted crypt, required for sql.connect.
ssl_file_names <- list(
  key = "neptune_sql_production-01-analysis-replica.key",
  cert = "neptune_sql_production-01-analysis-replica.cert",
  ca = "neptune_sql_production-01-analysis-replica.ca"
)

# Datastore
tables <- big_pipe('neptuneplatform', project_credentials, download = TRUE)

tables_ls <- list("ProjectCohort" = "project_cohort_",
                  "Survey" = "survey_",
                  "User" = "user_",
                  "Task" = "task_",
                  "Organization" = "organization_",
                  "Project" = "project_")
#for (table_name in names(tables_ls)) {
#  df <- tables[[table_name]] %>% flatten(., recursive = TRUE)
#  names(df) <- names(df) %>% gsub("key__\\.","key__",.) %>% gsub("key__", "key___",.)
#  for (x in names(df)){
#    y <- df[,x]
#    #print(x)
#    print(typeof(y))
#    if(typeof(y) == "list") {
#      df[,x] <- y %>% as.character()
#    }
#  }
#  # one of the variables is not flattened yet, I wil change it manually
#  #df$'__error__' <- df$'__error__' %>% as.character()
#  file_path <- CRYPT_FOLDER %+% tables_ls[[table_name]] %+% READ_DATE %+% ".csv"
#  write.csv(df, file_path, row.names = F)
#}



# this is the query which Sarah run, and I will replcate in R
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

rename_with_prefix <- function(in_df, new_name) {
  # renames data frame and adds prefix to each column
  names(in_df) <- names(in_df) %>% paste0(new_name, ".", .)
  return(in_df)
}

o <- tables$Organization %>% rename_with_prefix(.,"o")
s <- tables$Survey %>% rename_with_prefix(.,"s")
p <- tables$ProjectCohort %>% rename_with_prefix(.,"p")
# the survey_ids in the p table consists of two survey_ids, save them as separate
# columns
p$survey_id_1 <- NA
p$survey_id_2 <- NA

for (i in 1:nrow(p)) {
  current_element <- p$p.survey_ids[[i]]
  if (length(current_element) > 0) {
    p$survey_id_1[i] <- current_element[1]
    if (length(current_element) > 1) {
      p$survey_id_2[i] <- current_element[2]
      if (length(current_element) > 3) {
        stop("More than 2 elements in p$p.survey_ids")
      }
    }
  }
}




# create merged query
q <- merge(s,
           p,
           by.x = "s.project_cohort_id",
           by.y = "p.uid") %>%
  merge (.,
         o,
         by.x = "p.organization_id",
         by.y = "o.uid") %>%
  dplyr::rename(organization_name = o.name,
         organization_id = p.organization_id,
         project_cohort_id = s.project_cohort_id,
         program_label = p.program_label,
         survey_id = s.uid,
         cohort_label = s.cohort_label,
         expected_participants = p.expected_participants) %>%
  dplyr::select(organization_name,
         organization_id,
         project_cohort_id,
         program_label,
         survey_id,
         cohort_label,
         expected_participants,
         survey_id_1,
         survey_id_2) %>%
  dplyr::filter(program_label == "hg17")

write.csv(q, CRYPT_FOLDER %+% "query_new.csv", row.names = F)
# the last condtion (WHERE s.uid IN ...) will be run in the metascript
