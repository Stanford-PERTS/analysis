# this script can only be called from the engagement_metascript.R, it cannot
# run independently.
# It creates a one week summary and three weeks summary

# if you want to prevent command prompts in the txt ouput, run with ctrl+shift+S


# the total numbers of teams for which report was created


one_week_ago_start <- (as.Date(REPORT_DATE) - 8) %>% as.character()
three_weeks_ago_start <- (as.Date(REPORT_DATE) - 8 - 2*7) %>% as.character()


filtered_data <- data_cat_not_imputed %>%
  filter(!is.na(userID) & !is.na(class_id) & !is.na(week_start))


one_week_span_df <- data_cat_not_imputed[filtered_data$week_start >= one_week_ago_start,]
three_weeks_span_df <- data_cat_not_imputed[filtered_data$week_start >= three_weeks_ago_start,]



output_df <- data.frame()

#total number of participating teams
output_df[1,"report_date"] <- REPORT_DATE
output_df[2,"report_date"] <- REPORT_DATE
output_df[1,"time_span"] <- "1 week"
output_df[2,"time_span"] <- "3 weeks"
output_df[1,"active_teams"] <- length(unique(one_week_span_df$team_id))
output_df[2,"active_teams"] <- length(unique(three_weeks_span_df$team_id))



output_df[1,"active_class"] <- length(unique(one_week_span_df$class_id))
output_df[2,"active_class"] <- length(unique(three_weeks_span_df$class_id))

output_df[1,"active_particip"] <- paste0(one_week_span_df$userID, one_week_span_df$class_id) %>% unique %>% length
output_df[2,"active_particip"] <- paste0(three_weeks_span_df$userID, three_weeks_span_df$class_id) %>% unique %>% length


output_df[1,"exp_class"] <- triton_tbl$class_id[triton_tbl$team_id %in% unique(one_week_span_df$team_id)] %>% unique %>% length
output_df[2,"exp_class"] <- triton_tbl$class_id[triton_tbl$team_id %in% unique(three_weeks_span_df$team_id)] %>% unique %>% length

output_df[1,"expected_n"] <- triton_tbl$expected_n[triton_tbl$team_id %in% unique(one_week_span_df$team_id)] %>% sum
output_df[2,"expected_n"] <- triton_tbl$expected_n[triton_tbl$team_id %in% unique(three_weeks_span_df$team_id)] %>% sum


file_name <- "report_summary_" %+% REPORT_DATE %+% ".csv"
write.csv(output_df, RMD_BASE_PATH %+% file_name, row.names = FALSE)


# create table for Lauren which has team names, expected students and participating students
# for the last week only
active_team_id_1week <- unique(one_week_span_df$team_id)


one_week_span_df$userIDclassroomID <- paste0(one_week_span_df$userID, one_week_span_df$class_id)

team_summary_tbl_1week <- one_week_span_df %>% group_by(team_id) %>%
  summarise(
    team_name = first(team_name),
    active_class = length(unique(class_id)),
    active_n = n()
  ) %>% as.data.frame()

team_summary_tbl_1week$time_span <- "1 week"

team_summary_tbl_3week <- three_weeks_span_df %>% group_by(team_id) %>%
  summarise(
    team_name = first(team_name),
    active_class = length(unique(class_id)),
    active_n = n()
  ) %>% as.data.frame()

team_summary_tbl_3week$time_span <- "3 weeks"
team_summary_tbl <- rbind(team_summary_tbl_1week, team_summary_tbl_3week)

triton_tbl_smr <- triton_tbl %>% group_by(team_id) %>%
  summarise(
    expected_class = length(unique(class_id)),
    expected_n = as.integer(sum(expected_n))
  )

team_summary_tbl <- merge(
  team_summary_tbl,
  triton_tbl_smr,
  by = "team_id"
) %>% arrange(time_span)

team_summary_tbl$report_date <- REPORT_DATE

file_name <- "report_summary_teams_" %+% REPORT_DATE %+% ".csv"
write.csv(team_summary_tbl, RMD_BASE_PATH %+% file_name, row.names = FALSE)



##### Participation Summaries - dashboard
# https://docs.google.com/document/d/1EqS_5dTac4FBYr7eKCBvsjJIu6QhlFkN0rGeT_rlKQU/edit#

# all analyses should be run only for classes active in 2018, or registered in 2018

# Classes registered in 2018
filtering_vector <- (as.Date(class_tbl$created) %>% year) >= 2018
classes_registered_in_2018 <- class_tbl$uid[filtering_vector]

# classes active in 2018
filtering_vector <- (as.Date(data_cat_not_imputed$StartDate) %>% year) >= 2018
classes_active_in_2018 <- data_cat_not_imputed$class_id[filtering_vector] %>% unique


classes_in_2018 <- c(classes_registered_in_2018, classes_active_in_2018) %>% unique()

#create filtered copies of the data and the sql table
data_cat_not_imputed_filtered <- data_cat_not_imputed[data_cat_not_imputed$class_id %in% classes_in_2018,]
triton_tbl_filtered <- triton_tbl[triton_tbl$class_id %in% classes_in_2018,]



# Of users who have a class, how many surveyed students? (Have we made a product that’s compelling enough for people to want to try it?)

 percent_served_class_tbl <-
   data_cat_not_imputed_filtered %>% group_by(class_id) %>%
   summarise(serveyed = length(unique(userID))) %>%
   merge(.,
         triton_tbl_filtered,
         by = "class_id",
         all = T) %>%
   mutate(
     serveyed = ifelse(is.na(serveyed),0,serveyed),
     perc_serv = round((serveyed/as.numeric(expected_n)),2),
     eighty_or_more = ifelse(perc_serv >= .8, 1, 0)
   )
q1 <- mean( percent_served_class_tbl$eighty_or_more) # 66% of all classrooms

# Of users who surveyed students once (above), how many surveyed a second time? (Have we made a product that’s compelling enough that people want to keep using it?) (1)


percent_served_twice_class_tbl <-
  data_cat_not_imputed_filtered %>% group_by(class_id, week_start) %>%
  summarise(serveyed = length(unique(userID))) %>%
  merge(.,
        triton_tbl_filtered,
        by = "class_id",
        all = T) %>%
  mutate(
    serveyed = ifelse(is.na(serveyed),0,serveyed),
    perc_serv = round((serveyed/as.numeric(expected_n)),2),
    eighty_or_more = ifelse(perc_serv >= .8, 1, 0)
  ) %>%
  group_by(class_id) %>%
  summarise(
    active_weeks = n(),
    perc_serv_aver = mean(perc_serv, na.rm = T),
    eighty_or_more_sum = sum(eighty_or_more, na.rm = T)
  )

q2 <- sum (percent_served_twice_class_tbl$eighty_or_more_sum >= 2) /sum(percent_served_twice_class_tbl$eighty_or_more_sum >= 1)
# 48% of all classrooms who surveyed students once also surveyed them a second time.

# Of users who surveyed students once, how many have surveyed three times? (deep engagement) (2)

q3 <- sum (percent_served_twice_class_tbl$eighty_or_more_sum >= 3) /sum(percent_served_twice_class_tbl$eighty_or_more_sum >= 1)
# 24% of all classrooms who surveyed students once also surveyed them a second time


# What % of teams focused on each LC? Important for revealing if certain LCs are weak sauce.
data_cat_not_imputed_filtered$learning_conditions %>% unique
lc_df <- data_cat_not_imputed_filtered
lc_df$feedback_for_growth <- grepl("feedback-for-growth", lc_df$learning_conditions)
lc_df$meaningful_work <- grepl("meaningful-work", lc_df$learning_conditions)
lc_df$teacher_caring <- grepl("teacher-caring", lc_df$learning_conditions)

lc_summary_tbl <-
  lc_df %>% group_by(team_id) %>%
  summarise(
    feedback_for_growth_average = mean(feedback_for_growth, na.rm = T),
    meaningful_work_average = mean(meaningful_work, na.rm = T),
    teacher_caring_average = mean(teacher_caring, na.rm = T)
            )

lc_summary_tbl %>% summary

# feedback_for_growth_average: 0.7695
# meaningful_work_average: 0.7659
# teacher_caring_average: 0.9569


#How do the student questions about engagement with survey look? (esp. Dave’s proposed question of “did you rush through this”) (3)
# this is Q71


#Do you feel comfortable answering these questions honestly?
# 1 yes; 2 no
lc_df$honest_comfort %>% table()
mean(lc_df$honest_comfort, na.rm= T)
#My teacher will try to use my answers to this survey to make class better.
# 1 Strongly disagree, 5 Strongly agree

# it seems there is issue with recoding; There are only 5 options, but the sacle is 1 to 6, with 3 missing
lc_df$teacher_use_qs <- util.recode(lc_df$teacher_use_qs, c(4,5,6), c(3,4,5))
lc_df$teacher_use_qs %>% table()
mean(lc_df$teacher_use_qs, na.rm= T)

#I rushed through this survey and didn't think about my answers.
lc_df$rushing %>% table() # No data yet




# Start writing the dashboard analyses to an output file

file_name <- REPOSITORY_FOLDER %+% "/dashboard_output" %+% REPORT_DATE %+% ".txt"
sink(file_name, type = "output")
cat("\n")
cat("Report Date:" , REPORT_DATE, "\n")
cat("\n")
cat("#########################################################################\n")
cat("######################## Dashboard Summary Last Week  ###################\n")
cat("\n")
cat("q1. Of users who have a class, how many surveyed students? (Have we made a product that’s compelling enough for people to want to try it?\n")
cat("\tPercentage: ", round((q1*100),2), "%\n")
cat("\n")
cat("q2. Of users who surveyed students once (above), how many surveyed a second time? (Have we made a product that’s compelling enough that people want to keep using it?)\n")
cat("\tPercentage: ", round((q2*100),2), "%\n")
cat("\n")
cat("q3. Of users who surveyed students once, how many have surveyed three times? (deep engagement)\n")
cat("\tPercentage: ", round((q3*100),2), "%\n")
cat("\n")
cat("q4. What % of teams focused on each LC? Important for revealing if certain LCs are weak sauce.\n")
cat ("\tfeedback_for_growth_average: ",
     lc_summary_tbl$feedback_for_growth_average %>% mean(na.rm = T) %>%
       '*'(.,100) %>%
       round(.,2),
     "%\n")
cat ("\tmeaningful_work_average: ",
     lc_summary_tbl$meaningful_work_average %>% mean(na.rm = T) %>%
       '*'(.,100) %>%
       round(.,2),
     "%\n")
cat ("\tteacher_caring_average: ",
     lc_summary_tbl$teacher_caring_average %>% mean(na.rm = T) %>%
       '*'(.,100) %>%
       round(.,2),
     "%\n")
cat("\n")

# prepare class-level aggregated lc_df
lc_df_class <- lc_df %>% group_by(class_id) %>%
  summarise(honest_comfort = mean(honest_comfort, na.rm = T),
            teacher_use_qs = mean(teacher_use_qs, na.rm = T),
            rushing = mean(rushing, na.rm = T)
            )

cat("q5. How do the student questions about engagement with survey look?\n")
cat("\n")
cat("q5.1. Do you feel comfortable answering these questions honestly?\n")
cat("\tScale: 1 yes, 2 no; Mean = ", round(mean(lc_df$honest_comfort, na.rm= T),2),"\n")
cat("Classroom-level summary:\n")
print(summary(lc_df_class$honest_comfort))
cat("\n")
cat("\n")
cat("q5.2. My teacher will try to use my answers to this survey to make class better.\n")
cat("\tScale: 1 Strongly disagree, 5 Strongly agree; Mean = ", round(mean(lc_df$teacher_use_qs, na.rm= T),2),"\n")
cat("Classroom-level summary:\n")
print(summary(lc_df_class$teacher_use_qs))
cat("\n")
cat("\n")
cat("q5.2. I rushed through this survey and didn't think about my answers.\n")
cat("\tScale: 1 Strongly disagree, 5 Strongly agree; Mean = ", round(mean(lc_df$rushing, na.rm= T),2),"\n")
cat("Classroom-level summary:\n")
print(summary(lc_df_class$rushing))
cat("#########################################################################\n")
cat("\n")



# Stop writing to the file
sink()


