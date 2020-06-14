numbers_summary2 <- d_f %>%
  mutate(
    gms_bin_change = gms__s2_bin - gms__s1_bin,
    both_sessions_gms = !is.na(gms__s1_bin) & !is.na(gms__s2_bin)
  ) %>%
  group_by(project_cohort_id, organization_name) %>%
  summarise(
    n_s1 = sum(!is.na(ResponseID__s1)),
    n_s2 = sum(!is.na(ResponseID__s2)),
    n_s1_started = sum(!is.na(ResponseID__s1)),
    n_both = sum(!is.na(ResponseID__s1) & !is.na(ResponseID__s2)),
    n_either = sum(!is.na(gms__s1_bin) | !is.na(gms__s2_bin)),
    prop_s1_gms_both = mean(gms__s1_bin[both_sessions_gms]),
    prop_s2_gms_both = mean(gms__s2_bin[both_sessions_gms]),
    prop_s1_gms_all = mean(gms__s1_bin, na.rm = T),
    prop_s2_gms_all = mean(gms__s2_bin, na.rm = T),
    gms_bin_change = prop_s2_gms_both - prop_s1_gms_both,
    s2_prop_s1 = n_both/n_s1,
    hide_results_s2_n = n_both < S2_N_THRESHOLD,
    hide_results_s2_prop = s2_prop_s1 < S2_PROP_THRESHOLD,
    hide_LCs = n_s1 < S1_N_THRESHOLD
  ) %>%
  as.data.frame()


numbers_summary2 %>% dplyr::filter(project_cohort_id %in% "ProjectCohort_0Cvgi9sd") # 644

d_f %>%
  dplyr::filter(survey_id__s1 %in% "Survey_gGqIfFoO") %>%
  nrow() #563

d_f %>%
  dplyr::filter(project_cohort_id %in% "ProjectCohort_0Cvgi9sd") %>%
  nrow() #689

x <- d_f %>%
  dplyr::filter(
    project_cohort_id %in% "ProjectCohort_0Cvgi9sd",
    !is.na(ResponseID__s1)
  )
nrow(x) #644. Ok, so that's where the number comes from.
x$survey_id__s1 %>% table()

participation_neptune_check %>%
  dplyr::filter(
    !is.na(survey_id),
    abs(difference_with_min) > 30 & abs(difference_with_max) > 30
  ) %>%
  arrange(abs(difference_with_max))

s1_rcf_p %>%
  dplyr::filter(project_cohort_id %in% "ProjectCohort_0Cvgi9sd") %>%
  group_by(survey_id) %>%
  summarise(n = n())

affected_pids <- s1_raw %>%
  dplyr::filter(survey_id %in% q1_s2_ids) %>%
  pull(participant_id)

neptune_tables$pd %>%
  dplyr::filter(participant_id %in% affected_pids) %>%
  group_by(survey_ordinal) %>%
  summarise(n = n())


affected_pids_w_s1 <- neptune_tables$pd %>%
  dplyr::filter(
    participant_id %in% affected_pids,
    survey_ordinal %in% 1
  ) %>%
  pull(participant_id)

neptune_tables$pd %>%
  dplyr::filter(participant_id %in% affected_pids_w_s1) %>%
  select(participant_id, survey_id, survey_ordinal, project_cohort_id, cohort_label) %>%
  arrange(participant_id, survey_ordinal)


# Also: check five cases. Survey ids should only have data in survey 1 but not survey 2.
# pd ordinal should be 2 in every case. You can't request 1 and wind up at 2. But you can
# request 2 and wind up at 1.

neptune_tables$s %>% filter(project_cohort_id %in% "ProjectCohort_0Cvgi9sd")

s1_survey_ids <- neptune_tables$s$uid[neptune_tables$s$ordinal %in% 1]
s2_survey_ids <- neptune_tables$s$uid[neptune_tables$s$ordinal %in% 2]

q1_s2_ids <- s2_survey_ids[s2_survey_ids %in% s1_raw$survey_id]
q2_s1_ids <- s1_survey_ids[s1_survey_ids %in% s2_raw$survey_id] # there are NO offending s2 ids. Good news!

# To fully fix the database, Chris needs:
# participant_id, project_cohort_id, progress_in_survey1, progress_in_survey2
# pulled from Qualtrics so that I can fix the pd table

affected_project_cohorts <- neptune_tables$pd %>%
  dplyr::filter(survey_id %in% q1_s2_ids) %>%
  pull(project_cohort_id) %>%
  unique()


# start with the raw session 1 survey data (this has incomplete responses rbound in)
chris_table <- s1_raw %>%
  # filter to cases where survey_id is a s2 ordinal survey_id (look up on the Neptune)
  # survey table
  dplyr::filter(
    survey_id %in% neptune_tables$s$uid[neptune_tables$s$ordinal %in% 2]
  ) %>%
  # select just the relevant columns
  select(participant_id, survey_id, value, StartDate, EndDate) %>%
  rename(
    progress_in_survey_1 = value,
    StartDate_s1 = StartDate,
    EndDate_s1 = EndDate
  ) %>%
  # now get project_cohort_id from the Neptune survey table
  left_join(
    .,
    neptune_tables$s[c("uid", "project_cohort_id")],
    by = c("survey_id" = "uid")
  ) %>%
  # and get session 2 progress data from the s2 table. Note that
  # I get a few non-blank s2 survey_ids
  left_join(
    .,
    s2_raw[c("participant_id", "survey_id", "progress", "StartDate", "EndDate")],
    by = c("participant_id"),
    suffix = c("_s1", "_s2")
  ) %>%
  # rename some variables
  rename(
    progress_in_survey_2 = progress,
    survey_id_s1_wrong = survey_id_s1,
    StartDate_s2 = StartDate,
    EndDate_s2 = EndDate
  ) %>%
  # finally, merge in the CORRECT s1 survey_ids: these are the ones corresponding
  # to the same project_cohort_id but where survey_ordinal is marked "1"
  left_join(
    .,
    neptune_tables$s[neptune_tables$s$ordinal %in% 1, c("uid", "project_cohort_id")],
    by = "project_cohort_id"
  ) %>%
  rename(survey_id_s1_correct = uid) %>%
  arrange(participant_id)

write.csv(chris_table, "/Volumes/NO NAME 1/chris_table.csv", row.names = FALSE)

s1_raw %>%
  dplyr::filter(survey_id %in% q1_s2_ids) %>%
  pull(participant_id)

p_filtered %>%
  dplyr::filter(survey_id %in% "Survey_WxHKppY4") # it's an ordinal 2 survey id. How did it end up in the s1 data???

"Survey_WxHKppY4" %in% s1_raw$survey_id # TRUE. What????
