# Retrieves Neptune BigPipe and SQL data and combines into `tables`.

gymnast_util_file = paste(gymnast_path, '/R/util.R', sep = '')

source(gymnast_util_file, chdir = TRUE)
source('common/big_pipe.R')
source('common/sql_connect.R')

generate_tables <- function () {
  # Sourcing this file will define the variable `project_credentials`, required
  # for BigPipe. The file must be in a mounted crypt.
  crypt_paths <- util.find_crypt_paths(list(creds = 'neptune_credentials.R'))
  source(crypt_paths$creds)

  # These files must also be in a mounted crypt, required for sql.connect.
  ssl_file_names <- list(
    key = "neptune_sql_production-01-analysis-replica.key",
    cert = "neptune_sql_production-01-analysis-replica.cert",
    ca = "neptune_sql_production-01-analysis-replica.ca"
  )

  # Datastore
  tables <- big_pipe('neptuneplatform', project_credentials)

  # Cloud SQL
  conn <- sql.connect('', dbname = 'neptune', ssl_file_names)
  tables$Checkpoint <- sql.get_table(conn, "checkpoint")
  tables$Participant <- sql.get_table(conn, "participant")
  tables$ParticipantData <- sql.get_table(conn, "participant_data")
  sql.disconnect(conn)

  # Now we have Neptune data in `tables` from both the Datastore and MySQL!!

  # Prep for merging by prefixing col names with tables
  return(sql.prefix_tables(tables))
}
