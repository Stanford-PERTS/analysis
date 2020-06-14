big_pipe <- import_module("big_pipe")$big_pipe
sql <- import_module("sql")
util <- import_module("util")

# Sourcing this file will define the variable `project_credentials`, required
# for BigPipe. The file must be in a mounted crypt.
crypt_paths <- util$find_crypt_paths(list(creds = 'neptune_credentials.R'))
source(crypt_paths$creds)

# These files must also be in a mounted crypt, required for sql$connect.
ssl_file_names <- list(
  key = "neptuneplatform_analysis-replica_2019.key",
  cert = "neptuneplatform_analysis-replica_2019.cert",
  ca = "neptuneplatform_analysis-replica_2019.ca"
)

# Datastore
tables <- big_pipe('neptuneplatform', project_credentials, download = TRUE)

# Cloud SQL
neptune_cloud_access <- sql$create_neptune_service()
tables$Checkpoint <- neptune_cloud_access$get_table("checkpoint")

# to get participant data from Neptune, you don't want to pull the whole
# pd table because it's like a billion rows. So you'll want to filter to a particular
# program or something like that. Example:
pd_query <- paste0("
  SELECT *
    FROM `participant_data`
    WHERE
      `program_label` IN('hg17')
    AND
      `key` = 'progress';"
  )
tables$pd <- neptune_cloud_access$query(pd_query)

# Now we have Neptune data in `tables` from both the Datastore and MySQL!!

# ----


# Prep for merging by prefixing col names with tables
tables <- sql$prefix_tables(tables)

# Example 1: Schools that have unfinished tasks after the terms of use, in hg17

Checkpoint_survey <- dplyr::filter(
  tables$Checkpoint,
  checkpoint.parent_kind == "Survey",
  checkpoint.program_label == "hg17",
  checkpoint.status == "incomplete"
)

co <- merge(
  Checkpoint_survey,
  tables$Organization,
  by.x = "checkpoint.organization_id",
  by.y = "organization.uid"
)

cou <- merge(
  co,
  tables$User,
  by.x = "organization.liaison_id",
  by.y = "user.uid"
)

report1 <- unique(dplyr::select(cou, user.name, user.email))
print("Example 1: Schools that have unfinished tasks after the terms of use, in hg17")
print(report1)

# Example 2: all HG17 schools who have their second report uploaded.

## Filter
Checkpoint_hg <- dplyr::filter(tables$Checkpoint, checkpoint.program_label == "hg17")
Task_report <- dplyr::filter(
  tables$Task,
  task.label == "hg17_survey__report_2",
  task.status == "complete"
)

## Merge
ct <- merge(
  Checkpoint_hg,
  Task_report,
  by.x = "checkpoint.uid",
  by.y = "task.checkpoint_id",
)

cto <- merge(
  ct,
  tables$Organization,
  by.x = 'checkpoint.organization_id',
  by.y = 'organization.uid'
)

ctou <- merge(
  cto,
  tables$User,
  by.x = 'organization.liaison_id',
  by.y = 'user.uid'
)

## Report
report2 <- dplyr::select(ctou, organization.name, user.email, user.name)
print("Example 2: all HG17 schools who have their second report uploaded.")
print(report2)

