# first use analysis/Neptune/neptune_data_pull_demo.R to create platform tables
# once the tables are in the environment, temporarily save them in the Desktop
# saving the tables is for debugging, it could be skipped. If saved, make sure
# they are deleted at the end

# saveRDS(tables, "~/Desktop/tables.rds")
tables <- readRDS("~/Desktop/tables.rds")
tables <- sql$prefix_tables(tables)
# make sure you delete the file after reading it
# file.remove("~/Desktop/tables.rds")

SAVE_DATE <- "2020-04-15"

neptune_ProjectCohort_df <- tables$ProjectCohort %>%
  dplyr::select(-c(projectcohort.completed_report_task_ids, projectcohort.survey_ids)) %>% # these are lists, remove them
  as.data.frame()
  #%>%
  #dplyr::filter(projectcohort.program_label %in% "cg17") %>%
  #dplyr::filter(projectcohort.cohort_label %in% 2018)


dim(neptune_ProjectCohort_df)


names(neptune_ProjectCohort_df) <- names(neptune_ProjectCohort_df) %>% gsub("projectcohort.", "",.)

# there is some column that is type "list"
lapply(neptune_ProjectCohort_df, function(x) typeof(x)) %>% unlist %>% table
#names(lapply(neptune_ProjectCohort_df, function(x) typeof(x)))[lapply(neptune_ProjectCohort_df, function(x) typeof(x)) == "list"]
#completed_report_task_ids , survey_ids

# it seems we need to add survey_id
# notes from Chris:
#qualtrics_response.survey_id => survey.uid;
#survey.project_cohort_id => project_cohort.uid.
#Then use project_cohort.expected_n.


neptune_merged_df <- merge(
  neptune_ProjectCohort_df,
  tables$Survey %>% dplyr::select(survey.project_cohort_id, survey.uid, survey.cohort_label),
  by.x = "uid",
  by.y = "survey.project_cohort_id",
  all.x = T,
  all.y = T
)

# the old are just a subset of the new ones, I will replace with new
neptune_merged_df$survey_id <-  neptune_merged_df$survey.uid
neptune_merged_df <- neptune_merged_df %>%
  dplyr::select(-c(survey.uid)) %>%
  dplyr::rename(project_cohort_id = uid)# %>%
  #select(-cohort_label) %>%
  #rename(cohort_label = survey.cohort_label)


# remove columns which are lists (we are not really using them)
tables$ProjectCohort$projectcohort.completed_report_task_ids <- NULL
tables$ProjectCohort$projectcohort.survey_ids <- NULL

write.csv(tables$ProjectCohort, paste0("/Volumes/NO NAME 1/CG19_Spring/project_cohort_", SAVE_DATE,".csv"))
write.csv(neptune_merged_df, paste0("/Volumes/NO NAME 1/CG19_Spring/survey_", SAVE_DATE,".csv"))
