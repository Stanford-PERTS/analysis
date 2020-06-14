

# we now have a test dataset in which all RUs completed the diagnostic assessment,
# and 7 (randomly selected) RUs started their first improvement cycle.
# It's not a given that all 7 of these RUs had a complete observation 
# during the diagnostic phase (a complete observation being 
# defined as surveying > 50% of students in a single M-F 
# period), and it's not a given that all 7 of them have > 50% 
# participation for their improvement cycle.

# But the moment a single reporting unit achieves a SECOND complete 
# observation, from that day (week?) forward, all data for all 
# RUs on their team are counted as part of a second wave.
# NOW test this wave rule

# first, just see if I can define observations correctly here.
# A set of student observations is a complete observation IFF
# - At least 50% of students in the reporting unit have been surveyed
# since the start of the wave
# - At least an entire M-F period has passed that includes the first
# survey response in the wave (e.g., if the first student responded
# on Wednesday, then the observation isn’t complete until Friday,
# even if 80% of students have been surveyed)



# define ALL complete observations for each reporting unit
# observations include all M-F responses for a given reporting unit
# observations are complete IFF they include > 50% of expected responses

get_observation_info <- function(survey_df, expected_participation_table){
    
    # merge in expected participation information
    df <- merge(survey_df, expected_participation_table, by = "reporting_unit_id")
    
    # group survey responses to the week (defined as a M-F period, 
    # or more accurately, Sunday - Saturday)
    df$week_start <- df$day_start_survey %>%
        floor_date(unit = "week") %>%
        as.Date()
    
    # group survey responses by team, reporting unit, and the week in which they occurred
    df %>%
        group_by(team_id, reporting_unit_id, week_start) %>%
        # determine whether each reporting unit's responses for the week were complete observations
        summarise(
            expected_n = first(expected_n),
            n_surveyed_this_week = n(),
            first_survey_day_this_week = min(day_start_survey),
            last_survey_day_this_week = max(day_start_survey),
            pct_surveyed_this_week = n_surveyed_this_week/expected_n,
            is_complete_obs = ifelse(pct_surveyed_this_week > .5, 1, 0)
        ) %>%
        # sort by week within reporting unit
        arrange(reporting_unit_id, week_start) 
}


observation_info_df <- get_observation_info(qualtrics_2_waves, switchboard_data_mockup)

all_rus <- observation_info_df$reporting_unit_id %>% unique
all_weeks <- observation_info_df$week_start %>% unique

RU_weeks_expanded <- expand.grid(
    reporting_unit_id = all_rus, week_start = all_weeks
) %>%
    merge(., unique(observation_info_df[c("team_id", "reporting_unit_id")], by = "reporting_unit_id"))

expanded_observations <- merge(RU_weeks_expanded, observation_info_df, by = c("reporting_unit_id", "week_start", "team_id"), all = TRUE) %>%
    ungroup %>%
    # fill in missing complete observations
    mutate(is_complete_obs = ifelse(is.na(is_complete_obs), 0, is_complete_obs)) %>%
    group_by(team_id, reporting_unit_id) %>%
    # sort by week within reporting unit
    arrange(reporting_unit_id, week_start) %>%
    # count complete observations to-date within reporting units
    mutate(n_complete_obs_to_now = accumulate(.x = is_complete_obs, .f = sum))

waves_by_team <- expanded_observations %>%
    group_by(team_id, week_start) %>%
    summarise(wave = max(n_complete_obs_to_now))

waves_by_RU_week <- merge(waves_by_team, expanded_observations, by = c("team_id", "week_start"))
# here, Le_Conte started Wave 2 during the week of March 26. Nobody else is on wave 2.
# is that right?

actual_waves <- wave2_survey_start_dates %>%
    select(team_id, reporting_unit_id, day_start_survey) %>%
    unique
actual_waves$week_start <- actual_waves$day_start_survey %>%
    floor_date(unit = "week") %>%
    as.Date()

actual_waves_by_team <- merge(waves_by_team, actual_waves, by = "team_id", all = TRUE, suffixes = c("", "_wave2"))


check <- actual_waves_by_team %>%
    mutate(is_actually_during_wave_2 = week_start >= week_start_wave2) %>%
    merge(.,
          observation_info_df[c("reporting_unit_id", "week_start", "team_id", "pct_surveyed_this_week", "is_complete_obs")],
          by = c("reporting_unit_id", "week_start", "team_id")
    ) %>%
    filter(is_actually_during_wave_2 & is_complete_obs == 1)

# looks like Yoka should have started wave 2 during the week of April 9. Why didn't they?

expanded_observations %>% filter(reporting_unit_id %in% "Hernandez__Special_Ed_English_Period_3") %>%
    select(pct_surveyed_this_week, is_complete_obs)

# ahh, it's because this RU's FIRST observation was not complete! It makes sense. How to test it systematically?


# anyway there's more work to do. I now need to program the reports themselves 
# to panel the graphs on waves and all that jazz



# here, Le_Conte started Wave 2 during the week of March 26. Nobody else is on wave 2.
# is that right?

actual_waves <- wave2_survey_start_dates %>%
    select(team_id, reporting_unit_id, day_start_survey) %>%
    unique
actual_waves$week_start <- actual_waves$day_start_survey %>%
    floor_date(unit = "week") %>%
    as.Date()

actual_waves_by_team <- merge(waves_by_team, actual_waves, by = "team_id", all = TRUE, suffixes = c("", "_wave2"))


check <- actual_waves_by_team %>%
    mutate(is_actually_during_wave_2 = week_start >= week_start_wave2) %>%
    merge(.,
          observation_info_df[c("reporting_unit_id", "week_start", "team_id", "pct_surveyed_this_week", "is_complete_obs")],
          by = c("reporting_unit_id", "week_start", "team_id")
    ) %>%
    filter(is_actually_during_wave_2 & is_complete_obs == 1)

# looks like Yoka should have started wave 2 during the week of April 9. Why didn't they?

expanded_observations %>% filter(reporting_unit_id %in% "Hernandez__Special_Ed_English_Period_3") %>%
    select(pct_surveyed_this_week, is_complete_obs)

# ahh, it's because this RU's FIRST observation was not complete! It makes sense. How to test it systematically?


# anyway there's more work to do. I now need to program the reports themselves 
# to panel the graphs on waves and all that jazz
