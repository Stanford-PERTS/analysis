# this script assumes `data` has been read in by 
# engagement_metascript.R from the crypt.

# the items spreadsheet has some of the wrong column names.
names(items)[names(items) %in% "questioncode"] <- "question_code"
names(items)[names(items) %in% "questiontext"] <- "question_text"
names(items)[names(items) %in% "generalcategory"] <- "driver"
names(items)[names(items) %in% "responseoptions"] <- "response_options"

# fix the driver labels to match d_desc
items$driver[items$driver %in% "Belonging"] <- "belonging"
items$driver[items$driver %in% "Relevance"] <- "relevance"
items$driver[items$driver %in% "GMS"] <- "growth_mindset_(general)"

# add new data 
all_drivers  <- items$driver %>% unique
all_metrics  <- items$question_code[items$demographics %in% "No"] %>% unique

extra_data <- data.frame(matrix(nrow = 500, ncol = ncol(data))) %>%
    setNames(names(data))

all_variables <- c(
    all_metrics,
    TEAM_COLUMN,
    REPORTING_UNIT_COLUMN,
    "Teacher_ID",
    "StartDate"
)
# add extra data by sampling from sample data
for(var in all_variables){
    extra_data[, var] <- sample(
        data[, var], nrow(extra_data), replace = T
    )
}

data <- rbind(data, extra_data)


# add a Study_ID variable (note that I don't really see 
# how this variable fits into the process diagram yet!!)
data$Study_ID <- "Study 1"

# add a gender column (note that genders other than 1 and 2
# are non-male-female gender identification, so they will 
# be rare.)
data$gender <- sample(c(rep(1:2, 300), 3), nrow(data), replace = T)

