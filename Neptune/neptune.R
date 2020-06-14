# neptune.R
#
# Utilities for pulling and merging neptune data.
#
# Example:
#
# tables <- neptune.pull_all_data()
# checkpoint_report <- neptune.checkpoint_report(tables, program_label, cohort_label)
# task_report <- neptune.merge_task(tables, checkpoint_report, task_label)
# summary <- task_report[desired_columns]

library(dplyr)

gsub_columns <- function (pattern, replacement, subject_df) {
  names(subject_df) <- gsub(pattern, replacement, names(subject_df))
  return(subject_df)
}

neptune.pull_all_data <- function () {
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R")
  gymnast_install()
  source('common/big_pipe.R')
  sql <- modules::use('common/sql.R')

  # Sourcing this file will define the variable `project_credentials`, required
  # for BigPipe. The file must be in a mounted crypt.
  crypt_paths <- util.find_crypt_paths(list(creds = 'neptune_credentials.R'))
  source(crypt_paths$creds)

  # These files must also be in a mounted crypt, required for sql$connect.
  ssl_file_names <- list(
    key = "neptuneplatform_analysis-replica_2019.key",
    cert = "neptuneplatform_analysis-replica_2019.cert",
    ca = "neptuneplatform_analysis-replica_2019.ca"
  )

  # Datastore
  tables <- big_pipe('neptuneplatform', project_credentials)

  # Cloud SQL
  conn <- sql$connect('', dbname = 'neptune', ssl_file_names)
  tables$Checkpoint <- sql$get_table(conn, "checkpoint")
  # tables$Participant <- sql$get_table(conn, "participant_data")
  tables$ParticipantProgress <- sql$query(
    conn,
    "
      SELECT COUNT(`uid`) as `num_complete`, `project_cohort_id`
      FROM `participant_data`
      WHERE `key` = 'progress'
        AND `value` = '100'
      GROUP BY `project_cohort_id`
    "
  )
  tables$ParticipantProgressFirstSurvey <- sql$query(
    conn,
    "
      SELECT COUNT(`uid`) as `num_complete`, `project_cohort_id`
      FROM `participant_data`
      WHERE `key` = 'progress'
        AND `value` = '100'
        AND `survey_ordinal` = '1'
      GROUP BY `project_cohort_id`
    "
  )
  sql$disconnect(conn)

  # Prep for merging by prefixing col names with tables
  tables <- sql$prefix_tables(tables)

  return(tables)
}

neptune.checkpoint_report <- function(
  tables,
  program_label,
  cohort_label,
  remove_rejected = TRUE,
  ignore_orgs = c()
) {
  # Merge all the neptune data (except tasks and participation stuff)
  # into a wide table, limited to the given program and cohort.
  #
  # Args:
  #   tables - list of raw neptune data frames, combined results of
  #     sql$connect and big_pipe
  #   program_label - unitary character, e.g. 'sse'
  #   cohort_label - unitary character, e.g. '2018'
  #   remove_rejected - unitary logical, default true, whether to
  #     remove rejected orgs and project cohorts from result
  #   ignore_orgs - character, default empty, org ids to remove from
  #     result, e.g. Arnrow Test U
  #
  # Returns a wide data frame, indexed by project cohort, with all
  # relevant data from Organization, Project, Survey(s), Checkpoints,
  # and the liaison User.

  merge_checkpoints_as_label <- function (df, checkpoint_df, label, parent_kind) {
    # Add columns to the df that represent checkpoints with the given
    # label. Column names are like "checkpoint.[label].status".

    renamed_checkpoint_df <- checkpoint_df %>%
      filter(checkpoint.label == label) %>%
      # 'checkpoint.status' => 'checkpoint.[label].status'
      gsub_columns('\\.', paste0('.', label, '.'), .)

    if (parent_kind == 'Organization') {
      lhs_col = 'projectcohort.organization_id'
      rhs_col <- paste0('checkpoint.', label, '.organization_id')
    } else if (parent_kind == 'Project') {
      lhs_col = 'projectcohort.project_id'
      rhs_col <- paste0('checkpoint.', label, '.project_id')
    } else if (parent_kind == 'Survey') {
      lhs_col = 'projectcohort.uid'
      rhs_col <- paste0('checkpoint.', label, '.project_cohort_id')
    }

    # Duplicate the right-hand merge column. This makes the merge
    # code simpler, and keeps a copy of the original merge column
    # even after the merge so we can reference it later.
    renamed_checkpoint_df[[lhs_col]] <- renamed_checkpoint_df[[rhs_col]]

    return(merge(
      df,
      renamed_checkpoint_df,
      by = lhs_col,
      all.x = TRUE
    ))
  }

  merge_surveys_as_ordinal <- function (df, survey_df, ordinal) {
    # Add columns to the df that represent surveys with the given
    # ordinal. Column names are like "survey.[ordinal].status".

    renamed_survey_df <- survey_df %>%
      filter(survey.ordinal == ordinal) %>%
      # 'survey.status' => 'survey.[ordinal].status'
      gsub_columns('\\.', paste0('.', ordinal, '.'), .)

    # Duplicate the right-hand merge column. This makes the merge
    # code simpler, and keeps a copy of the original merge column
    # even after the merge so we can reference it later.
    lhs_col <- 'projectcohort.uid'
    rhs_col <- paste0('survey.', ordinal, '.project_cohort_id')
    renamed_survey_df[[lhs_col]] <- renamed_survey_df[[rhs_col]]

    return(merge(
      df,
      renamed_survey_df,
      by = lhs_col,
      all.x = TRUE
    ))
  }

  # Merge together all the kinds that are one per pc: Org,
  # Project, User.
  initial_merge <- tables$ProjectCohort %>%
    filter(
      projectcohort.program_label == program_label,
      projectcohort.cohort_label == cohort_label
    ) %>%
    merge(
      tables$ParticipantProgress,
      by.x = 'projectcohort.uid',
      by.y = 'participantprogress.project_cohort_id',
      all.x = TRUE
    ) %>%
    merge(
      tables$Organization,
      by.x = 'projectcohort.organization_id',
      by.y = 'organization.uid',
      all.x = TRUE
    ) %>%
    merge(
      tables$Project,
      by.x = 'projectcohort.project_id',
      by.y = 'project.uid',
      all.x = TRUE
    ) %>%
    merge(
      tables$User,
      by.x = 'organization.liaison_id',
      by.y = 'user.uid',
      all.x = TRUE
    ) %>%
    # Restore all the columns collapsed by merge(), even
    # though they're redundant, for completeness.
    mutate(
      organization.uid = projectcohort.organization_id,
      project.uid = projectcohort.project_id,
      user.uid = organization.liaison_id
    )

  # There are some things that are always noise: closed pcs
  # and rejected orgs. Remove them.
  post_removal <- initial_merge
  if (remove_rejected) {
    post_removal <- post_removal %>%
      filter(
        projectcohort.status != 'closed',
        organization.status != 'rejected'
      )
  }
  # Some of our orgs are for internal testing. Remove them.
  post_removal <- filter(post_removal, !organization.uid %in% ignore_orgs)

  # What are the available survey ordinals?
  survey_ordinals <- tables$Survey %>%
    filter(
      survey.program_label == program_label,
      survey.cohort_label == cohort_label
    ) %>%
    select(survey.ordinal) %>%
    distinct() %>%
    unlist() %>% unname()

  # Merge each survey in a separate set of columns.
  with_surveys <- post_removal
  for (o in survey_ordinals) {
    with_surveys <- merge_surveys_as_ordinal(with_surveys, tables$Survey, o)
  }

  # What are the available checkpoint labels?
  org_clabels <- c('organization__main')
  project_clabels <- tables$Checkpoint %>%
    filter(
      checkpoint.program_label == program_label,
      checkpoint.parent_kind == 'Project'
    ) %>%
    select(checkpoint.label) %>%
    distinct() %>%
    unlist() %>%
    unname()
  survey_clabels <- tables$Checkpoint %>%
    filter(
      checkpoint.program_label == program_label,
      checkpoint.cohort_label == cohort_label,
      checkpoint.parent_kind == 'Survey'
    ) %>%
    select(checkpoint.label) %>%
    distinct() %>%
    unlist() %>%
    unname()

  # Merge each survey in a separate set of columns.
  with_checkpoints <- with_surveys
  for (l in org_clabels) {
    with_checkpoints <- merge_checkpoints_as_label(
      with_checkpoints, tables$Checkpoint, l, 'Organization')
  }
  for (l in project_clabels) {
    with_checkpoints <- merge_checkpoints_as_label(
      with_checkpoints, tables$Checkpoint, l, 'Project')
  }
  for (l in survey_clabels) {
    with_checkpoints <- merge_checkpoints_as_label(
      with_checkpoints, tables$Checkpoint, l, 'Survey')
  }

  return(with_checkpoints)
}

neptune.merge_tasks <- function (tables, report_df, task_labels) {

  # What checkpoints are available? Look checkpoint id columns.
  pattern <- '^checkpoint\\.(\\S+)\\.uid$'
  checkpoint_id_cols <- grep(pattern, names(report_df), value = TRUE)

  merge_single_label <- function (df, label) {
    # Merge the requested task, which requires finding the correct checkpoint,
    # of the many labels we've merged.

    renamed_task_df <- tables$Task %>%
      filter(task.label == label) %>%
      # 'task.status' => 'task.[label].status'
      gsub_columns('\\.', paste0('.', label, '.'), .)

    matching_checkpoint_id_col <- NULL
    task_checkpoint_col <- paste0('task.', label, '.checkpoint_id')
    for (col in checkpoint_id_cols) {
      if (any(head(renamed_task_df[[task_checkpoint_col]]) %in% df[[col]])) {
        matching_checkpoint_id_col <- col
      }
    }
    if (is.null(matching_checkpoint_id_col)) {
      stop(paste0(
        "Couldn't find checkpoint matching task: ",
        label,
        " among checkpoint columns: ",
        paste(checkpoint_id_cols, collapse=", ")
      ))
    }

    # We expect the right hand merge column to be dropped, but
    # we want to keep it, so store it in an alias.
    renamed_task_df[[matching_checkpoint_id_col]] <- renamed_task_df[[task_checkpoint_col]]

    return(merge(
      df,
      renamed_task_df,
      by = matching_checkpoint_id_col,
      all.x = TRUE
    ))
  }

  for (label in task_labels) {
    report_df <- merge_single_label(report_df, label)
  }

  return(report_df)

}
