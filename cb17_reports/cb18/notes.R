table(d_f$school_name, d_f$survey_id, exclude = NULL)

z <- d_f %>% group_by(school_name) %>%
  summarise(unique_surveys = length(unique(survey_id)))

z <- neptune_df %>% group_by(school_name_nept) %>%
  summarise(unique_surveys = length(unique(survey_ids)))
unique(d_f$survey_id) %in% unique(neptune_df$survey_ids)

# @todo
# make sure there are no differences in coding between new and old surveys in Qualtrics
# check if race categories are the same

# check if the logs are by project_cohort or by school name
# some school names might be weird e.g. StMU full name has non-ascii characters

# there is something wrong with gender !!!!!!!!
# check if disadvantages status is ok