# first use analysis/Neptune/neptune_data_pull_demo.R to save tables
# saveRDS(tables, "~/Desktop/tables.rds")
tables <- readRDS("~/Desktop/tables.rds")
# make sure you delete the file after reading it
#file.remove("~/Desktop/tables.rds")

# figure out what table we need, based on the old csv
all_names <- lapply(tables, function(x) names(x)) %>% unlist %>% unname %>% c()
all_names %>% grep("status", x = ., value = TRUE)
all_names %>% grep("expected", x = ., value = TRUE) # "projectcohort.expected_participants"
all_names %>% grep("portal_type", x = ., value = TRUE) # "projectcohort.expected_participants"
# clearly we need ProjectCohort



neptune_ProjectCohort_df <- tables$ProjectCohort %>% as.data.frame() %>%
  dplyr::filter(projectcohort.program_label %in% "cb17") #%>%
  #filter(projectcohort.cohort_label %in% 2018)





names(neptune_ProjectCohort_df) <- names(neptune_ProjectCohort_df) %>% gsub("projectcohort.", "",.)

# there is some column that is type "list"
lapply(neptune_ProjectCohort_df, function(x) typeof(x)) %>% unlist %>% table
#completed_report_task_ids , survey_ids
list_to_vec <- function(in_list) {
  # some cols in the data frame are actually lists
  # I cannot unlist them directly, because it skips the empty elements, resulting in
  # different lengths
  out_vec <- rep(NA, length(in_list))
  condition_vec <- lapply(in_list,function(x) length(x) > 0) %>% unlist
  out_vec[condition_vec] <- in_list[condition_vec] %>% unlist
  return(out_vec)
}
neptune_ProjectCohort_df$completed_report_task_ids <- neptune_ProjectCohort_df$completed_report_task_ids %>% list_to_vec
neptune_ProjectCohort_df$survey_ids <- neptune_ProjectCohort_df$survey_ids %>% list_to_vec


# it seems we need to add survey_id
# notes from Chris:
#qualtrics_response.survey_id => survey.uid;
#survey.project_cohort_id => project_cohort.uid.
#Then use project_cohort.expected_n.

neptune_merged_df <- merge(
  neptune_ProjectCohort_df,
  tables$Survey %>% dplyr::select(survey.project_cohort_id, survey.uid),
  by.x = "uid",
  by.y = "survey.project_cohort_id",
  all.x = T,
  all.y = F
)

# check what is happening with old and new survey ids
neptune_merged_df %>% dplyr::select(survey.uid, survey_ids)

# the old are just a subset of the new ones, I replace with new
neptune_merged_df$survey_id <-  neptune_merged_df$survey.uid
neptune_merged_df <- neptune_merged_df %>% dplyr::select(-c(survey.uid, survey_ids))

write.csv(neptune_merged_df, "/Volumes/NO NAME/raw_data/neptune_df.csv")

