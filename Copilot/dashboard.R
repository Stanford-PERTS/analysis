# Requires perts_crypt.vc mounted for db access.

modules::use("gymnast/R/bootstrap.R")$install_module_imports()
options(repos = "http://cran.rstudio.com/")
modules::depend("base", "3.6.2")
modules::depend("dplyr", "0.8.4")
modules::depend("modules", "0.8.0")

modules::import(
  "dplyr",
  `%>%`,
  "as_tibble",
  "filter",
  "group_by",
  "left_join",
  "mutate",
  "n",
  "summarise"
)

google_sheets <- import_module("google_sheets")
summarize_copilot <- import_module("summarize_copilot")
sql <- import_module("sql")
util <- import_module("util")

START_DATE <- "2019-06-01"
END_DATE <- "2020-05-31"
COPILOT_PROGRAM_LABELS <- c(
  "beleset19",
  "ccp19",
  "cset19",
  "ep19"
)
TEST_TEAMS <- c(
  "Arnrow",
  "arnrow test",
  "Arnrow testing",
  "Demo EP Team",
  "Demo Team Viper EP",
  "Engagement Video (ARNROW)",
  "Example CSET Project",
  "Lilia CC Example Team",
  "Lilia's CC Example Team (coach)",
  "Lilia's Example Elevate Project",
  "Lilia's Example Team",
  "PERTS CC Example Team",
  "PERTS Example Team",
  "Sarah demo team",
  "Sarah's demo team — 2 rosters",
  "Sarah Gripshover Demo Team — empty target groups",
  "Sarah's testing team — one roster",
  "Team Viper",
  "TEST Team Viper",
  "TEST Viper Team (CSET)",
  "TEST Viper Team"
)


######### Load from Triton and Neptune databases #########


triton <- sql$create_triton_service()
neptune <- sql$create_neptune_service()
# triton <- sql$create_triton_test_service()
# neptune <- sql$create_neptune_test_service()

triton_table_names <- c(
  "classroom",
  "cycle",
  "organization",
  "program",
  "participant",
  "team",
  "user"
)

triton_tables <- Map(triton$get_table, triton_table_names) %>%
  sql$prefix_tables() %>%
  Map(as_tibble, .)

# Take out test teams.
triton_tables$team <- filter(triton_tables$team, !team.name %in% TEST_TEAMS)

# These tables are big, b/c they include neptune program data as well. Limit the
# queries to Copilot programs, teams, and the appropriate school year.
p_query <- paste0("
  SELECT *
  FROM `participant`
  WHERE `organization_id` LIKE 'Team_%';
")
pd_query <- paste0("
  SELECT *
  FROM `participant_data`
  WHERE
    `program_label` IN(
      '", paste(COPILOT_PROGRAM_LABELS, collapse = "', '"), "'
    ) AND
    `modified` > '", START_DATE, "' AND
    `modified` < '", END_DATE, "' AND
    `key` = 'progress' AND
    `value` = '100';
")

neptune_tables <- list(
  participant = neptune$query(p_query),
  participant_data = neptune$query(pd_query)
) %>%
  sql$prefix_tables() %>%
  Map(as_tibble, .)

# ################
# # Fake some pd for testing
#
# FRACTION = 0.5
# to_bind <- list()
# ppt_ids <- neptune_tables$participant %>%
#   dplyr::sample_n(ceiling(nrow(neptune_tables$participant) * FRACTION)) %>%
#   dplyr::pull(participant.uid)
# for (program_label in triton_tables$program$program.label) {
#   pd <- dplyr::tibble(
#     uid = unlist(Map(
#       function(...) perts_ids$create_uid('ParticipantData'),
#       sequence(length(ppt_ids))
#     )),
#     created = rep(format(Sys.time(), '%Y-%m-%d %H:%M:%S'), length(ppt_ids)),
#     modified = rep(format(Sys.time(), '%Y-%m-%d %H:%M:%S'), length(ppt_ids)),
#     key = rep('progress', length(ppt_ids)),
#     value = rep('100', length(ppt_ids)),
#     participant_id = ppt_ids,
#     program_label = rep(program_label, length(ppt_ids)),
#     code = sample(triton_tables$classroom$classroom.code, length(ppt_ids), replace = TRUE),
#     survey_ordinal = rep(1, length(ppt_ids))
#   )
#   to_bind[[length(to_bind) + 1]] <- pd
# }
# neptune_tables$participant_data <- util$rbind_union(to_bind) %>%
#   util$prefix_columns('participant_data')
# ################


######### Merge data #########


# tc "team-class" merged data
# index: team-classroom
# cols:
# * team.*
# * classroom.*
# * captain.*
# * contact.*
tc <- summarize_copilot$team_class(
  triton_tables$program,
  # triton_tables$team %>% dplyr::filter(team.uid %in% 'Team_0ad3LyTE5xthc6mG'),
  triton_tables$team,
  # triton_tables$cycle,
  triton_tables$classroom,
  triton_tables$user
)

# tcc "team-class-cycle" merged data
# index: team-classroom-cycle
# adds cols:
# * cycle.*
#   - includes cycle.students_completed, aggregated by copilot
tcc <- summarize_copilot$team_cycle_class(tc, triton_tables$cycle)

# tcc_ppn "participation" merged data
# index: team-classroom-cycle
# adds cols:
# * num_completed_by_pd ("pd" means from the participant_data table)
tcc_ppn <- summarize_copilot$team_cycle_class_participation(
  tcc,
  triton_tables$cycle,
  triton_tables$classroom,
  neptune_tables$participant,
  neptune_tables$participant_data
)


######### Caculate dashbard metrics #########


# Classroom uids, since "started" is a classroom concept.
started_0_ids <- summarize_copilot$get_started_ids(tcc_ppn, threshold = 0)
started_80_ids <- summarize_copilot$get_started_ids(tcc_ppn, threshold = 80)

# Prep some more complex columns for later addition to the final output.
# Each have column `team.uid` for merging.
community_names <- summarize_copilot$team_community_names(
  triton_tables$team,
  triton_tables$organization,
  triton_tables$user
)
cycle_dates <- summarize_copilot$cycle_dates(
  triton_tables$team,
  triton_tables$cycle
)
cycle_ppn <- summarize_copilot$cycle_participation_wide(tcc_ppn)

# Get counts of classrooms over various thresholds per-program.
sum_ignore <- function(...) sum(..., na.rm = TRUE)
metrics_team <- tc %>%
  # Collapse classrooms while counting those which have started.
  mutate(
    team.created = team.created %>%
      strptime("%Y-%m-%d %H:%M:%S") %>%
      strftime("%Y-%m-%d")
  ) %>%
  group_by(
    team.created,
    program.label,
    team.uid,
    team.name,
    captain.name,
    captain.email
  ) %>%
  summarise()
metrics_teachers <- tc %>%
  group_by(team.uid) %>%
  summarise(
    teachers_unstarted = paste(
      na.omit(contact.email[!classroom.uid %in% started_0_ids]),
      collapse = ","
    ),
    num_teachers_total = length(unique(contact.email)),
    num_teachers_unstarted = length(unique(
      contact.email[!classroom.uid %in% started_0_ids]
    )),
    num_teachers_started_0 = length(unique(
      contact.email[classroom.uid %in% started_0_ids]
    )),
    num_teachers_started_80 = length(unique(
      contact.email[classroom.uid %in% started_80_ids]
    ))
  )
metrics_classrooms <- tc %>%
  group_by(team.uid) %>%
  summarise(
    num_classrooms_total = n(),
    num_classrooms_unstarted = sum(!classroom.uid %in% started_0_ids),
    num_classrooms_started_0 = sum(classroom.uid %in% started_0_ids),
    num_classrooms_started_80 = sum(classroom.uid %in% started_80_ids)
  )
metrics_rostered <- tc %>%
  group_by(team.uid) %>%
  summarise(
    num_rostered_total = sum_ignore(classroom.num_students),
    num_rostered_unstarted = sum_ignore(
      classroom.num_students[!classroom.uid %in% started_0_ids]
    ),
    num_rostered_started_0 = sum_ignore(
      classroom.num_students[classroom.uid %in% started_0_ids]
    ),
    num_rostered_started_80 = sum_ignore(
      classroom.num_students[classroom.uid %in% started_80_ids]
    )
  )

metrics <- metrics_team %>%
  left_join(metrics_teachers, by = "team.uid") %>%
  left_join(metrics_rostered, by = "team.uid") %>%
  # * community_names
  # * community_admin_names
  # * community_admin_emails
  left_join(community_names, by = "team.uid") %>%
  # * cycle_dates
  # * cycles_past
  # * cycle_present
  # * cycles_future
  left_join(cycle_dates, by = "team.uid") %>%
  # * cycle.abs.X
  # * cycle.pct.X
  # * avg.pct
  left_join(cycle_ppn, by = "team.uid") %>%
  left_join(metrics_classrooms, by = "team.uid")


######### Upload to Google Sheets #########

metrics_nice_columns <- metrics
names(metrics_nice_columns) <- gsub('[._]', ' ', names(metrics))

DASHBOARD_SHEET_ID <- "1pFK1vQzTmwRWfD4XsZw309nZwKnmcPyomzDg8qrX5zc"
# Link to Google Sheet:
# https://docs.google.com/spreadsheets/d/1pFK1vQzTmwRWfD4XsZw309nZwKnmcPyomzDg8qrX5zc/edit
creds_path <- util$find_crypt_paths("rserve_credential_body.json")
creds <- jsonlite::fromJSON(creds_path)
sheets_service <- google_sheets$open(
  creds$rserve_service_account_credentials,
  credential_type = "service_account_key"
)

tab_name <- strftime(Sys.time(), "%Y-%m-%d")
sheets_service$add_tab(DASHBOARD_SHEET_ID, tab_name) # no-op if tab exists
sheets_service$clear_tab(DASHBOARD_SHEET_ID, tab_name)
range <- paste0("'", tab_name, "'!A1")
sheets_service$overwrite(
  DASHBOARD_SHEET_ID,
  metrics_nice_columns,
  range_begin = range
)
sheets_service$format_simple_header_row(DASHBOARD_SHEET_ID, tab_name)

# Formatting:
