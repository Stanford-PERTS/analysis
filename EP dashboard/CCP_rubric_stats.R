# Script to create a csv for NTC of stats on rubric data.
#
# Requirements:
#
# * perts_crypt.vc mounted
# * sql dbs accessible (not on Impact Hub wifi)

if (!grepl('/analysis/?$', getwd())) {
  stop(paste0(
    "This file expects the working directory to be the root of the analysis ",
    "repository."
  ))
}


### Import

modules::import('dplyr')
dashboard_utils <- import_module('EP dashboard/dashboard_utils')
json_utils <- import_module('json_utils')
sql <- import_module('sql')

conn <- sql$connect(
  server_ip = "",
  dbname = "triton",
  ssl_file_names = list(
    ca = "triton_sql_production-01-analysis-replica.ca",
    key = "triton_sql_production-01-analysis-replica.key",
    cert = "triton_sql_production-01-analysis-replica.cert"
  ),
  mysql_user = "readonly"
)
program_tbl <- sql$get_table(conn, "program")
org_tbl <- sql$get_table(conn, "organization")
team_tbl <- sql$get_table(conn, "team")
cycle_tbl <- sql$get_table(conn, "cycle")
response_tbl <- sql$get_table(conn, "response")
sql$disconnect_all()


### Process

# Filter to CCP.
ccp_id <- program_tbl %>%
  filter(label %in% 'ccp19') %>%
  pull(uid)
ccp_orgs <- org_tbl %>%
  filter(program_id %in% ccp_id) %>%
  rename(
    organization_id = uid,
    organization_name = name
  )
ccp_teams <- team_tbl %>%
  filter(program_id %in% ccp_id) %>%
  rename(
    team_id = uid,
    team_name = name,
  )
ccp_cycles <- cycle_tbl %>%
  rename(cycle_id = uid) %>%
  left_join(ccp_teams, 'team_id') %>%
  filter(program_id %in% ccp_id) %>%
  mutate(
    cycle_name = paste0("Cycle ", ordinal)
  )

# Expand response data, just for the co-analysis, and add team data.
coanalysis <- response_tbl %>%
  filter(
    team_id %in% ccp_teams$team_id,
    module_label %in% 'CCPCoAnalysis'
  ) %>%
  dashboard_utils$widen_response_body() %>%
  mutate(
    teacher_practice = as.numeric(teacher_practice),
    classroom_interactions = as.numeric(classroom_interactions)
  ) %>%
  left_join(ccp_teams, by = "team_id") %>%
  # Get readable names for reponses that occurred in cycles.
  left_join(
    ccp_cycles[c("cycle_id", "cycle_name")],
    by = c("parent_id" = "cycle_id")
  ) %>%
  # Build a readable parent name from either the cycle name, or the parent_id
  mutate(
    parent_name = ifelse(grepl("^Cycle_", parent_id), cycle_name, parent_id)
  )

if (any(duplicated(paste(coanalysis$team_id, coanalysis$parent_id)))) {
  stop("wrong level")
}

# # Average over available stages (baseline, cycles)
# avg <- coanalysis %>%
#   group_by(team_id, team_name, organization_ids) %>%
#   summarise(
#     teacher_practice = mean(teacher_practice),
#     classroom_interactions = mean(classroom_interactions)
#   )

# Filter orgs to "Sites:"
sites <- ccp_orgs %>%
  filter(grepl("^Site:", organization_name))

# If CCP is configured correctly on Copilot, there should only be
# one "Site" organization per team. This may not be actually true
# since teams and orgs are many-to-many.
# Note that the `organization_ids` column is a JSON structure,
# specifically a unique array of strings.
coanalysis_sites <- coanalysis %>%
  # Now this is team X parent_id X organization_id
  json_utils$expand_string_array_column(organization_ids) %>%
  # Switch to singular since this has been unnested.
  rename(organization_id = organization_ids) %>%
  # If assumptions are correct, this is team X parent_id again.
  inner_join(sites, by = 'organization_id')

if (nrow(coanalysis_sites) %in% 0) {
  stop("Found no teams in any Sites.")
}

if (any(duplicated(paste(coanalysis_sites$team_id, coanalysis_sites$parent_id)))) {
  stop("wrong level")
}

avg <- coanalysis_sites %>%
  group_by(organization_id, organization_name, parent_name) %>%
  summarise(
    n = n(),
    teacher_practice_mean = mean(teacher_practice),
    teacher_practice_min = min(teacher_practice),
    teacher_practice_max = max(teacher_practice),
    teacher_practice_median = median(teacher_practice),
    classroom_interactions_mean = mean(classroom_interactions),
    classroom_interactions_min = min(classroom_interactions),
    classroom_interactions_max = max(classroom_interactions),
    classroom_interactions_median = median(classroom_interactions)
  )


### Wrangle data for consumption and write csv

avg %>%
  ungroup() %>%
  arrange(organization_name) %>%
  mutate(
    teacher_practice_mean = round(teacher_practice_mean, 2),
    classroom_interactions_mean = round(classroom_interactions_mean, 2)
  ) %>%
  rename(
    site_id = organization_id,
    site_name = organization_name
  ) %>%
  write.csv(file = 'CCP_rubric_stats.csv')
