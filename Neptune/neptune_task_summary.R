setwd('~/Documents/GitHub/analysis')

source('common/ok_comma.R')
source('Neptune/neptune.R')

### INSTRUCTIONS ###

# This file should make it easier to answer questions that
# require task-level information, like:
#
# * When schools plan on participating
# * How many students schools are planning to invite
#
# To use it, customize the variables below to specify a program,
# cohort, and the tasks you want to see. Tasks are specified with
# their "label", which you can either find by looking at the
# program config file, or looking at the value of
# `all_task_labels` below (after the script is run).
#
# You can also limit the number of columns you see by commenting
# lines in the `desired_columns` section.
#
# When ready:
#
# 1. Mount "perts_crypt.vc" with VeraCrypt
# 2. Run this whole file with the "Source" button in RStudio.
#
# A table of data should appear that you can filter and sort.
# If you configured the settings as such, you can also use
# the csv file that was saved.

### CUSTOMIZE SETTINGS HERE ###

program_label <- 'cg17'  # options are: 'cg17', 'cg17', 'hg17', 'sse'
cohort_label <- '2019'  # note that 2017 cohort names vary, e.g. '2017_fall'
task_labels <-  ok_comma(c)(
  'cg17_survey__expected_participants',
  'cg17_survey__expected_launch_date',
)
ignore_orgs = ok_comma(c)(
  'Organization_FgDmBhby',  # Arnrow Test U
)
should_save_csv = TRUE  # TRUE or FALSE
file_to_save <- '~/Desktop/summary.csv'

desired_columns = ok_comma(c)(
  # # Participation
  "participantprogress.num_complete",
  
  # # Organization columns
  # "organization.country",
  # "organization.created",
  # "organization.mailing_address",
  "organization.name",
  # "organization.phone_number",
  # "organization.poid",
  # "organization.postal_code",
  # "organization.state",
  # "organization.status",  # 'unapproved', 'approved', or 'rejected'
  # "organization.uid",
  # "organization.website_url",
  
  # # Project columns
  # "project.last_active",
  # "project.loa_notes",
  # "project.priority",
  # "project.uid",
  
  # # ProjectCohort columns
  # "projectcohort.code",
  # "projectcohort.created",
  # "projectcohort.custom_portal_url",
  # "projectcohort.expected_participants",
  # "projectcohort.modified",
  # "projectcohort.portal_type",
  # "projectcohort.status",  # 'open' or 'closed' (closed being "frozen")
  # "projectcohort.uid",
  
  # # Survey columns.
  # # Note there may also be "survey.2" fields, etc.
  # "survey.1.modified",
  "survey.1.status",  # 'not ready', 'ready', or 'complete'
  
  # # Checkpoint columns.
  # # Note the part between the dots will differ based on checkpoint label.
  "checkpoint.organization__main.status",
  "checkpoint.cg17_project__commitment.status",
  # "checkpoint.cg17_survey__quiz.status",
  
  # # Task columns.
  # # Note the part between the dots will differ based on task label.
  "task.cg17_survey__expected_participants.attachment",  # data entered into the task
  # "task.cg17_survey__expected_participants.modified",
  "task.cg17_survey__expected_participants.status",  # 'complete' or 'incomplete'
  
  "task.cg17_survey__expected_launch_date.attachment",  # data entered into the task
  # "task.cg17_survey__expected_launch_date.modified",
  "task.cg17_survey__expected_launch_date.status",  # 'complete' or 'incomplete'
  
  # # Liaison columns.
  # "user.created",
  "user.email",
  # "user.last_login",
  "user.name",
  # "user.phone_number",
  # "user.uid",
)

### END CUSTOMIZED SETTINGS ###

### RUN LINE BY LINE IF DESIRED ###

tables <- neptune.pull_all_data()  # downloads data; doesn't need to be repeated
checkpoint_report <- neptune.checkpoint_report(tables, program_label, cohort_label, ignore_orgs = ignore_orgs)
task_report <- neptune.merge_tasks(tables, checkpoint_report, task_labels)
summary <- task_report[desired_columns]

if (should_save_csv) {
  write.csv(apply(summary, 2, as.character), file_to_save)
}

# Run this for a nice view:
View(summary)



# Other helpful stuff..

# list of all checkpoint labels:
all_checkpoint_labels <- tables$Checkpoint$checkpoint.label %>% unique() %>% sort()

# list of all task labels:
all_task_labels <- tables$Task$task.label %>% unique() %>% sort()

# list of all columns available for "desired_columns"
available_columns <- names(task_report)
