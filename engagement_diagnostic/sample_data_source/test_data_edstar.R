DIAGNOSTIC <- FALSE


# items is a .csv listing all the items used in a particular survey?
items <- read.csv("question_text-revised.csv")
demographics <- c("gender","race")
names(items)  <- snake_case(names(items))
items$question_code <- snake_case(items$question_code)
items$question_text <- util.strip_non_ascii(items$question_text)
items$variable <- snake_case(items$variable)
item_metrics <- items$question_code[!items$question_code %in% demographics]
items <- rename(items, driver=variable )
items$demographics[items$demographics %in% "No"] <- FALSE
items$demographics[items$demographics %in% "Yes"] <- TRUE
items$demographics <- as.logical(items$demographics)
items$categorical[items$categorical %in% "No"] <- FALSE
items$categorical[items$categorical %in% "Yes"] <- TRUE
items$categorical <- as.logical(items$categorical)


data <- read.csv("Surveybuilder_test__EdStar.csv") %>%
    qc.clean_qualtrics()

names(data)[names(data) %in% "SSO_ID"] <- "team_id"
names(data)[names(data) %in% "group"] <- "reporting_unit_id"

data[,item_metrics] <- rnorm( nrow(data) * length(item_metrics) , 3, 1)

# add some extra rows to the data

# Delete this when real data are available; this section 
# manipulates sample data to arrive at pretty sample reports.
extra_data <- data.frame(matrix(ncol = ncol(data), nrow = 1500)) %>%
    setNames(names(data))
extra_data$reporting_unit_id <- sample(data$reporting_unit_id, nrow(extra_data), replace = T)
extra_data$team_id <- sample(data$team_id, nrow(extra_data), replace = T)
extra_data$StartDate <- sample(data$StartDate, nrow(extra_data), replace = T)
extra_data$gender <- sample(c(1,1,2), nrow(extra_data), replace = T)
extra_data$race <- sample(data$race, nrow(extra_data), replace = T)
extra_data$Study_ID <- sample(data$Study_ID, nrow(extra_data), replace = T)
data <- rbind(data, extra_data)
data$ID <- NULL
data_rbound <- data

# For Jacquie, change J-Fernandez to Hero School (bc her comparison 
# population is the whole school)
#data[data$team_id %in% "J-Fernandez", "team_id"] <- "Hero School"

# add some fake data to the survey metrics
data[,item_metrics] <- rnorm( nrow(data) * length(item_metrics) , 4, 1)
data$post_treatment <- data$StartDate > "2016-06-01"

# rename a few vars (to match the survey data as of Apr 2017)
names(data)[names(data) %in% "gender"] <- "genderv1"
names(data)[names(data) %in% "race"] <- "racev1"
items$question_code[items$question_code %in% "race"] <- "racev1"
items$question_code[items$question_code %in% "gender"] <- "genderv1"


# add a treatment effect
# data[data$post_treatment ,item_metrics] <-
#     data[data$post_treatment ,item_metrics ] +
#     rnorm(sum(data$post_treatment),1,.1)

# add some interesting stuff with gender in the relevance measures
data[
    # before the treatment the girls are doing bad on relevance
    data$gender > 1 & !data$post_treatment,
    c("relev1", "relev2", "relev3")
    ] <- rnorm(sum(data$gender > 1 & !data$post_treatment) * 3, 2, 1)


data[
    # after the treatment the girls are doing better but not perfect
    data$gender > 1 & data$post_treatment,
    c("relev1", "relev2", "relev3")
    ] <- rnorm(sum(data$gender > 1 & data$post_treatment) * 3, 5, 1)


# add hypothetical ID values to test data. Within team_id/study/reporting_unit_id combinations,
# there should be many repeated students with data for all dates,
# and some students with data for not-all dates. The operation
# below reporting_unit_ids by team_id, Study_ID, reporting_unit_id, and day_start,
# and assigns ID values 1 through however many rows there
# are for each team_id/Study_ID/reporting_unit_id combo. Thus, the values
# 1:min(nrow(data[,combo])) are repeated across days and constitute complete
# cases, and numbers > min(nrow(data[,combo])) are incomplete cases.
# @to-do: remove this when finished running script with test data
# data <- data %>%
#     group_by(
#         team_id,
#         Study_ID,
#         group,
#         day_start
#     ) %>%
#     mutate(ID = 1:n()) %>%
#     ungroup %>% as.data.frame

# change J-Fernandez to "math department"

data$team_id[data$team_id %in% "J-Fernandez"] <- "EdStar"
data$reporting_unit_id[data$reporting_unit_id %in% "Per 2 Alg"] <- "Hero School"

# for diagnostic, reduce to first day. Otherwise, for improvement, 
# reduce to relevance driver.
if(DIAGNOSTIC){
    qualtrics_date_format <- "%Y-%m-%d %H:%M:%S"
    stripped_time <- strptime( data$StartDate , qualtrics_date_format )
    data$day <- stripped_time %>%
        floor_date(., unit="day") %>%
        as.Date()
    first_day <- min(data$day)
    data <- data[data$day %in% first_day, ]
} else{
    items <- items[items$driver %in% "relevance" | items$demographics, ]
}