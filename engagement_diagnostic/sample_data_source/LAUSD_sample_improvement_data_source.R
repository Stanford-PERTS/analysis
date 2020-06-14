# this script assumes `data` has been read in by 
# engagement_metascript.R from the crypt.
qualtrics_date_format <- "%Y-%m-%d %H:%M:%S"
stripped_time <- strptime( data$StartDate , qualtrics_date_format )

base_qualtrics <- list(i = "LAUSD_Diagnostic_Survey_v3_Apr4.csv") %>%
    util.find_crypt_paths() %>%
    unlist %>%
    read.csv %>%
    qc.clean_qualtrics() %>%
    select(-X) %>%
    # change this team_id (Dan messed it up in his spreadsheet)
    mutate(team_id = gsub("Berendo__Diagnostic_Team", "Berendo", team_id))

# say what the day and week was that each student started each survey response
base_qualtrics$day_start <- strptime( base_qualtrics$StartDate , qualtrics_date_format ) %>%
    floor_date(unit = "day") %>%
    as.Date()

# base_qualtrics$week_start <- strptime( base_qualtrics$StartDate , qualtrics_date_format ) %>%
#     floor_date(unit = "week") %>%
#     as.Date()


# select user/reporting unit combinations to be repeated as complete cases
demographics <- items$question_code[items$demographics]
all_students <- base_qualtrics[
    c("userID", "team_id", "reporting_unit_id", "day_start",
      demographics)
] %>%
    filter(!util.is_blank(userID)) %>%
    unique

switchboard_data_mockup <- all_students %>%
    group_by(reporting_unit_id) %>%
    # expected n is the total n, approximately doubled
    summarise(
        total_n = n(),
        expected_n = (total_n + floor(rnorm(1, total_n, 4)))
    ) %>%
    select(-total_n)

#### Now simulate a second wave of data collection.

# let's say next week, a random sample of 7 reporting units
# IN REALITY begin a second wave of data collection.

all_students_2waves <- all_students





generate_continuing_wave <- function(
    prop_continuing_RUs, prop_stud_attrition, improvement_items,
    prop_new_studs, approx_days_btwn_waves,
    base_survey
){
    n_rus <- unique(base_survey$reporting_unit_id) %>% length
    n_continuing_RUs <- round(prop_continuing_RUs * n_rus)
    # approx_n_continuing_RUs is the proportion of RUs who continue to the 
    # next wave, 
    
    rus_who_continued <- sample(
        unique(base_survey$reporting_unit_id),
        n_continuing_RUs
    )
    
    wave2_respondents <- base_survey %>%
        filter(reporting_unit_id %in% rus_who_continued) %>%
        select(team_id, reporting_unit_id, userID)
    
    # shave off respondents due to attrition
    wave2_respondents <- wave2_respondents[
        sample(1:nrow(wave2_respondents), nrow(wave2_respondents)*(1-prop_student_attrition)), 
    ] 
    
    # sort by team and RU
    wave2_respondents <- arrange(wave2_respondents, team_id, reporting_unit_id)
    
    # Add a couple of brand-new, never-before-seen students to each RU (increase # of rows by pct_new_studs)
    wave2_respondents_extra <- wave2_respondents[
        sample(1:nrow(wave2_respondents), nrow(wave2_respondents)*prop_new_studs), 
    ] %>%
        mutate(userID = util.hash_vector(userID))
    wave2_respondents <- util.rbind_union(list(wave2_respondents, wave2_respondents_extra))
    
    # sample survey data for each continuing item
    
    wave2_survey <- wave2_respondents %>%
        data.frame(., matrix(nrow = nrow(wave2_respondents), ncol = length(improvement_items))) %>%
        setNames(c(names(wave2_respondents), improvement_items))
    
    for(item in improvement_items){
        wave2_survey[[item]] <- sample(base_survey[, item], nrow(wave2_survey), replace = T)
    }
    
    # merge in StartDate info
    # start with each continuing RU's max start date, and figure out 
    # new start dates that are approx. 2 weeks later
    
    new_wave_start_dates <- base_survey %>%
        filter(reporting_unit_id %in% rus_who_continued) %>%
        group_by(reporting_unit_id) %>%
        summarise(
            last_time_day = last(day_start),
            day_start = as.Date(last_time_day + rnorm(1, approx_days_btwn_waves, 4), "1970-01-01")
        )
    
    wave2_survey_start_dates <- merge(wave2_survey, new_wave_start_dates, by = "reporting_unit_id") %>%
        select(-last_time_day)
}

wave2_data <- generate_continuing_wave(
    prop_continuing_RUs = .8, prop_stud_attrition = .4, improvement_items = c("ed16v1", "ed19v1"),
    prop_new_studs = .2, approx_days_btwn_waves = 30, base_survey = base_qualtrics
)

# rbind the new data to the old data
qualtrics_2_waves <- util.rbind_union(list(base_qualtrics, wave2_survey_start_dates))

wave3_data <- generate_continuing_wave(
    prop_continuing_RUs = .8, prop_stud_attrition = .4, improvement_items = c("ed16v1", "ed19v1"),
    prop_new_studs = .2, approx_days_btwn_waves = 30, base_survey = qualtrics_2_waves
)

qualtrics_3_waves <- util.rbind_union(list(qualtrics_2_waves, wave3_data))

data <- qualtrics_3_waves