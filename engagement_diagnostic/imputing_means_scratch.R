##############################################################################
##  Fill in missing student responses by propagating means forward
##  (this is so that run-charts aren't swayed by 
##  differences in participation across sessions)

# get means for each question within reporting units for each wave > 1
wave_RU_means <- data_dups_handled[c("reporting_unit_id", "wave", present_metrics)] %>%
    melt(id.vars = c("reporting_unit_id", "wave")) %>%
    rename(metric = variable) %>%
    group_by(reporting_unit_id, wave, metric) %>%
    summarise(mean_score = mean(value, na.rm = TRUE)) %>%
    mutate(next_wave = wave + 1) %>%
    dcast(reporting_unit_id + next_wave + wave ~ metric, value.var = "mean_score")

# I need to know which questions belong in which waves
# in order to figure out which values to impute. After all, I don't
# want to impute values for questions that were discontinued!

question_used_by_wave_wide <- data_dups_handled %>%
    group_by(wave) %>%
    select(one_of(present_metrics)) %>%
    summarise_all(.funs = function(x) sum(!is.na(x))) %>%
    group_by(wave) %>%
    summarise_all(.funs = function(x) x > 0) 

question_used_by_wave <-
    melt(question_used_by_wave_wide, id.vars = "wave") %>%
    rename(metric = variable, question_used_this_wave = value)


#wave_RU_means$next_wave[wave_RU_means$next_wave %in% 0] <- NA
users_RUs <- data_dups_handled[c("userID", "reporting_unit_id")] %>%
    unique

# get an identifier for all user/RU combos
users_RUs$user_RU <- paste0(users_RUs$userID, "___________", users_RUs$reporting_unit_id)

data_melted <- melt(data_dups_handled[c("userID", "reporting_unit_id", "wave", present_metrics)],
                    id.vars = c("userID", "reporting_unit_id", "wave")) %>%
    rename(metric = variable)

# create exapanded grid and merge in the real values
users_expanded <- expand.grid(
    user_RU = unique(users_RUs$user_RU),
    wave = unique(data_dups_handled$wave),
    metric = unique(present_metrics)
) %>%
    separate(user_RU, into = c("userID", "reporting_unit_id"), sep = "___________") %>%
    merge(., data_melted, by = c("userID", "reporting_unit_id", "metric", "wave"), all = TRUE) %>%
    merge(., question_used_by_wave, by = c("metric", "wave"), all = TRUE) %>%
    arrange(reporting_unit_id, userID, metric, wave) 
any(is.na(users_expanded$userID))

# append info about whether each subject was present by waves
user_present_by_wave <- users_expanded %>%
    group_by(userID, reporting_unit_id, wave) %>%
    summarise(
        n_missing_questions = sum(question_used_this_wave & is.na(value)),
        n_total_questions = sum(question_used_this_wave),
        user_present_this_wave = n_missing_questions < n_total_questions
    ) %>%
    select(userID, reporting_unit_id, wave, user_present_this_wave)

# merge in information about whether users were present for each wave
users_expanded_upres <- merge(
    users_expanded,
    user_present_by_wave,
    by = c("userID", "reporting_unit_id", "wave"),
    all = TRUE
)
any(is.na(users_expanded_upres$user_present_this_wave)) # FALSE

# finally, merge in the means to be imputed

wave_RU_means_melted <- melt(wave_RU_means, id.vars = c("reporting_unit_id", "next_wave", "wave")) %>%
    rename(metric = variable, imputed_mean = value)

# match wave to previous wave to get imputed values from previous wave
users_expanded_wmeans <- merge(
    users_expanded_upres,
    wave_RU_means_melted[!names(wave_RU_means_melted) %in% "wave"],
    by.x = c("wave", "metric", "reporting_unit_id"),
    by.y = c("next_wave", "metric", "reporting_unit_id"),
    all.x = TRUE,
    all.y = FALSE
)
any(is.na(users_expanded_wmeans$user_present_this_wave)) # FALSE
# should all have same number of rows
nrow(users_expanded_wmeans)
nrow(users_expanded_upres)
nrow(users_expanded)

complete_obs_by_RU <- waves_by_RU_week %>%
    filter(is_complete_obs == 1) %>%
    group_by(reporting_unit_id, wave) %>%
    summarise(RU_had_complete_obs = sum(is_complete_obs) > 0)

users_expanded_wobs <- merge(
    users_expanded_wmeans,
    complete_obs_by_RU,
    by = c("reporting_unit_id", "wave"),
    all = TRUE
)
# non-complete obs will be NA instead of FALSE because they were missing in users_expanded_wobs
users_expanded_wobs$RU_had_complete_obs[is.na(users_expanded_wobs$RU_had_complete_obs)] <- FALSE


# identify values to be imputed (we don't want to impute just randomly!)
# as ones where !user_present_this_wave & wave != 1 & question_used_this_wave & !reporting_unit_present_this_wave

users_expanded_wobs$value_is_imputed <- !users_expanded_wobs$user_present_this_wave &
    users_expanded_wobs$question_used_this_wave &
    users_expanded_wobs$wave > 1 &
    users_expanded_wobs$RU_had_complete_obs &
    !is.na(users_expanded_wobs$imputed_mean)

users_expanded_wobs$value_with_imputed <- ifelse(
    users_expanded_wobs$value_is_imputed,
    users_expanded_wobs$imputed_mean,
    users_expanded_wobs$value
)

# add a boolean indicating whether the user had imputed data for that wave
users_expanded_imputed <- users_expanded_wobs %>%
    group_by(userID, reporting_unit_id, wave) %>%
    mutate(data_was_imputed = sum(value_is_imputed) > 0)

users_expanded_imputed %>% group_by(reporting_unit_id, wave) %>% summarise(n_imputed = sum(data_was_imputed)) %>% as.data.frame

# now I want to merge these values back into the original data,
# retaining every subject/RU/wave combination.

# First cast back up to subject/RU/wave.
users_imputed_wide <- users_expanded_wobs %>%
    dcast(userID + reporting_unit_id + wave ~ metric, value.var = "value_with_imputed")



data_imputed <- data_dups_handled