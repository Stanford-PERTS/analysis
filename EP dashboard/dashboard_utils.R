modules::import(tidyverse)
modules::import(lubridate)
modules::import(kableExtra)
modules::import(scales)
modules::import(dplyr)
modules::import(tidyr)
modules::import(reshape2)
modules::import(stringr)

modules::use("gymnast/R/bootstrap.R")$install_module_imports()
util <- import_module("util")
json_utils <- import_module("json_utils")

TODAY <- lubridate::ymd(Sys.Date())

smart_percent <- function(vec) {
  # format percents nicely
  ifelse(is.na(vec),
         # CM doesn't know what function DG was using here...
         #percent(0, accuracy = 1),
         #percent(vec, accuracy = 1)
         percent(0),
         percent(vec)
  )
}

widen_response_body <- function(responses) {
  # Apply to a data frame from the `response` table to expand the `body` field
  # (a JSON structure) into multiple columns.
  #
  # Example:
  #
  #   wide_responses <- widen_response_body(response_tbl)

  # Each response row be added to this and rbound together.
  dfs_to_bind <- list()

  widen <- function (res) {
    # `res` is a character vector with named elements.

    # Make a 1-row temporary data frame for every element EXCEPT body.
    # We'll expand the body and assign new columns to it.
    df_args <- as.list(res[!names(res) %in% 'body'])
    df_args$stringsAsFactors = FALSE
    res_df <- do.call(data.frame, df_args)

    # Parse the serialized field-level data into a list.
    tryCatch(
      {
        body <- jsonlite::fromJSON(res[['body']])
      },
      error = function (e) {
        print(e)
        print(is(res[['body']]))
        print(res[['body']])
        print(res)
        return(list())
      }
    )

    # For each field in the list...
    for (col in names(body)) {

      # Make sure the value is primitive. Otherwise keep it as a JSON string.
      value <- body[[col]]$value
      if (length(value) > 1) {
        value <- as.character(jsonlite::toJSON(value))
      } else if (!is.character(value) && !is.numeric(value)) {
        warning(paste0(
          "Replacing unrecognized value with NA. Is: ",
          paste(methods::is(value), collapse = ' '),
          ". Length: ",
          length(value),
          ". Value: ",
          value
        ))
        value <- NA
      }

      # Create a new column for this field.
      res_df[[col]] <- value
    }

    # Now that all the fields are unpacked, bind this row to the full data frame.
    dfs_to_bind[[res[['uid']]]] <<- res_df
  }

  apply(responses, 1, widen)
  wide <- util$rbind_union(dfs_to_bind)

  # Numeric types have been lost. I'd love to be able to dynamically make the
  # output column types match in the input ones, but I don't know how to do that,
  # short of writing out a bunch of if statements to matched each type with its
  # "as" function. So just check the numeric ones.
  for (col in names(responses)) {
    if (is.numeric(responses[[col]]) && col %in% names(wide)) {
      wide[[col]] <- as.numeric(wide[[col]])
    }
  }

  return(wide)
}

team_participation_tracking <- function (
  d,
  started_class_ids,
  program_tbl,
  team_tbl,
  test_teams,
  cycle_tbl,
  class_tbl,
  participant_tbl,
  org_tbl,
  user_tbl,
  response_tbl
) {
  ###### Participation metrics for Arnrow
  # tt = team table

  ### Assemble tt and add community info and team member counts

  team_program <- merge(
    team_tbl,
    dplyr::select(program_tbl, uid, program_label = label),
    by.x = 'program_id',
    by.y = 'uid'
  )

  # clean up tt
  tt <- team_program %>%
    select(team_name,
           team_id,
           created,
           organization_ids,
           captain_id,
           program_label) %>%
    filter(!team_name %in% test_teams)


  # break up teams' associated communities - ONLY STORING THE FIRST LISTED COMMUNITY!
  tt$community_id <- tt$organization_ids %>%
    gsub("\"", "", .) %>%
    gsub("\\[", "", .) %>%
    gsub("\\]", "", .) %>%
    gsub(",.*", "", .) %>%
    replace(. == "", NA)

  # get community name instead of ID
  tt <- merge(tt,
              org_tbl[, c("uid", "name")],
              by.x = "community_id",
              by.y = "uid",
              all.x = TRUE,
              all.y = FALSE) %>%
    rename(community_name = name)

  # add community contacts (max of three orgs per contact allowed)
  # separate out associated orgs for each user into multiple columns
  user_tbl$owned_organizations_cleaned <- user_tbl$owned_organizations %>%
    gsub("\"", "", .) %>%
    gsub("\\[", "", .) %>%
    gsub("\\]", "", .)
  user_tbl_orgs_separated <- separate(
    user_tbl,
    owned_organizations_cleaned,
    c("org_1", "org_2", "org_3"),
    sep = ", "
  )
  user_tbl_orgs_separated$org_1 <- ifelse(user_tbl_orgs_separated$org_1 %in% "",
                                          NA,
                                          user_tbl_orgs_separated$org_1)
  # melt, then regroup by org
  user_tbl_orgs_separated_melted <- melt(user_tbl_orgs_separated[c("user_id", "org_1", "org_2",
                                                                   "org_3", "name", "email")],
                                         id.vars = c("user_id", "name", "email"))
  org_to_contacts <- user_tbl_orgs_separated_melted %>%
    filter(!is.na(value)) %>%
    group_by(value) %>%
    summarise(community_contact_info = paste(name, email, sep = ", ", collapse = "; ")) %>%
    rename(community_id = value)
  # merge
  tt <- merge(tt,
              org_to_contacts,
              by = "community_id",
              all.x = TRUE,
              all.y = FALSE)

  # Add team captain info (we know it's one captain per team, and max of five teams per captain)
  user_tbl_captains <- user_tbl %>%
    select(user_id, name, email) %>%
    rename(captain_id = user_id, captain_name = name, captain_email = email)
  tt <- merge(tt,
              user_tbl_captains,
              by = "captain_id",
              all.x = TRUE,
              all.y = FALSE)

  # Add number of team members - number of user_tbl rows that pattern match in owned_teams
  count_team_matches <- function(team_id, team_assignments) {
    length(grep(team_id, team_assignments))
  }
  count_team_matches_vectorized <- Vectorize(
    count_team_matches,
    vectorize.args = "team_id"
  )
  tt$num_team_members <- count_team_matches_vectorized(
    tt$team_id,
    user_tbl$owned_teams
  )

  team_class <- team_tbl %>%
    dplyr::left_join(
      class_tbl %>% dplyr::select(class_id, team_id, contact_id),
      by = "team_id"
    )

  # * num_main_contacts, i.e. team members who are
  # in control of a classroom. Merge class to team and count unique contact ids.
  # * num_classrooms
  tt <- team_class %>%
    dplyr::group_by(team_id) %>%
    dplyr::summarise(
      num_main_contacts = length(unique(contact_id)),
      num_classrooms = n()
    ) %>%
    dplyr::right_join(tt, by = "team_id")

  # * main_contacts_unstarted i.e. emails of teachers who are main contacts of
  # rosters, but none of those rosters have reached the "started" threshold.
  unstarted_class_ids <- team_class %>%
    dplyr::filter(!class_id %in% started_class_ids) %>%
    dplyr::pull(class_id)
  unstarted_by_contact <- team_class %>%
    # Don't group by just contact, since a given user may be a member of
    # multiple teams. We're interested in people's performance within each team
    # independently.
    dplyr::group_by(team_id, contact_id) %>%
    dplyr::summarise(
      num_unstarted_classes = sum(class_id %in% unstarted_class_ids),
      num_total_classes = dplyr::n(),
    ) %>%
    # Limit to contacts where all their classes are unstarted.
    dplyr::filter(num_unstarted_classes == num_total_classes)
  unstarted_by_team <- unstarted_by_contact %>%
    dplyr::left_join(
      dplyr::select(user_tbl, user_id, email),
      by = c("contact_id" = "user_id")
    ) %>%
    # Summarize these email addresses by team.
    dplyr::group_by(team_id) %>%
    dplyr::summarise(
      main_contacts_unstarted = paste(unique(email), collapse = ", ")
    )
  tt <- dplyr::left_join(tt, unstarted_by_team, by = "team_id")

  count_participants <- function (team_tbl, ppt_tbl) {
    # Returns df with cols c("team_id", "num_participants")
    team_tbl %>%
      dplyr::left_join(
        dplyr::select(ppt_tbl, participant_id = uid, team_id),
        by = "team_id"
      ) %>%
      # drop rows representing teams who have no participants
      dplyr::filter(!is.na(participant_id)) %>%
      dplyr::group_by(team_id) %>%
      dplyr::summarise(num_participants = n())
  }

  # * num_participants
  tt <- tt %>%
    dplyr::left_join(
      count_participants(team_tbl, participant_tbl),  # adds `num_participants`
      by = "team_id"
    )
  # For teams where no participants were joined, record 0.
  tt$num_participants <- ifelse(is.na(tt$num_participants), 0, tt$num_participants)

  # * num_started_participants
  started_participant_tbl <- participant_tbl %>%
    json_utils$expand_string_array_column(classroom_ids) %>%
    dplyr::filter(classroom_ids %in% started_class_ids) %>%
    # Keep only the first row from any given participant, keeping all columns,
    # so we have a unique list of participants that appear in at least one
    # started class.
    dplyr::distinct(uid, .keep_all = TRUE)
  tt <- tt %>%
    dplyr::left_join(
      dplyr::rename(
        count_participants(team_tbl, started_participant_tbl),
        num_started_participants = num_participants
      ),
      by = "team_id"
    )
  # For teams where no participants were joined, record 0.
  tt$num_started_participants <- ifelse(
    is.na(tt$num_started_participants),
    0,
    tt$num_started_participants
  )


  ### add cycle information:
  # tag cycles as over or not based on date,
  # then summarise to team level
  cycle_tbl_cleaned <- cycle_tbl %>%
    filter(!is.na(start_date), !is.na(end_date)) %>%
    mutate(cycle_over = end_date < TODAY,
           cycle_happening_now = (start_date <= TODAY) & (end_date >= TODAY),
           cycle_not_started = start_date > TODAY) %>%
    arrange(team_id, cycle_name)
  # get all past cycles
  cycle_tbl_summarized_past <- cycle_tbl_cleaned %>%
    filter(cycle_over) %>%
    group_by(team_id) %>%
    summarise(cycles_past = paste(cycle_name, collapse = "; "),
              most_recent_past_cycle = last(cycle_name))
  # get present cycle (should just be one)
  cycle_tbl_summarized_present <- cycle_tbl_cleaned %>%
    filter(cycle_happening_now) %>%
    group_by(team_id) %>%
    summarise(cycle_happening_now = paste(cycle_name, collapse = "; "))
  # get all future cycles
  cycle_tbl_summarized_future <- cycle_tbl_cleaned %>%
    filter(cycle_not_started) %>%
    group_by(team_id) %>%
    summarise(cycles_in_future = paste(cycle_name, collapse = "; "))
  cycle_tbl_summarized_all <- cycle_tbl_cleaned %>%
    # no filter
    group_by(team_id) %>%
    summarise(cycle_dates_set = paste(cycle_name, collapse = "; "))
  # merge past, present, and future
  cycle_tbl_summarized <- cycle_tbl_summarized_past %>%
    merge(
      .,
      cycle_tbl_summarized_present,
      by = "team_id",
      all = TRUE
    ) %>%
    merge(
      .,
      cycle_tbl_summarized_future,
      by = "team_id",
      all = TRUE
    ) %>%
    merge(
      .,
      cycle_tbl_summarized_all,
      by = "team_id",
      all = TRUE
    )

  # merge in with tt
  tt <- merge(tt,
              cycle_tbl_summarized,
              by = "team_id",
              all.x = TRUE,
              all.y = FALSE)


  ### add participation info across cycles

  # Use class_tbl to get total expected roster counts for each teacher across classrooms
  teacher_expected_student_counts <- class_tbl %>%
    group_by(contact_id) %>%
    summarise(expected_num_students = sum(num_students))
  # get participation within team-cycle-teacher
  d_teachers <- d %>%
    group_by(team_id, cycle_name, teacher_id, class_id) %>%
    summarise(num_observed_students = length(unique(userID))) %>%
    summarise(num_observed_students = sum(num_observed_students))
  # merge in expected student counts for teachers,
  # convert NA to 0 for expected and observed participation,
  # calculate teacher-cycle-level participation
  d_teachers <- merge(d_teachers,
                      teacher_expected_student_counts,
                      by.x = "teacher_id",
                      by.y = "contact_id",
                      all.x = TRUE,
                      all.y = FALSE)
  d_teachers$num_observed_students <- ifelse(is.na(d_teachers$num_observed_students),
                                             0,
                                             d_teachers$num_observed_students)
  d_teachers$expected_num_students <- ifelse(is.na(d_teachers$expected_num_students),
                                             0,
                                             d_teachers$expected_num_students)
  d_teachers <- mutate(d_teachers,
                       pct_student_participation = num_observed_students / expected_num_students,
                       at_least_80_pct_participated = num_observed_students >= .8 * expected_num_students)
  # merge in practice journals for team-cycle-teachers
  pj_tbl_f <- response_tbl %>%
    filter(grepl("EPPracticeJournal", module_label)) %>%
    rename(cycle_id = parent_id,
           teacher_id = user_id) %>%
    mutate(did_practice_journal = TRUE)
  # add "Cycle_" to cycle_id if not there already
  pj_tbl_f$cycle_id <- ifelse(grepl("^Cycle_", pj_tbl_f$cycle_id),
                              pj_tbl_f$cycle_id,
                              "Cycle_" %+% pj_tbl_f$cycle_id)
  pj_tbl_f <- merge(pj_tbl_f,
                    cycle_tbl[, c("cycle_id", "cycle_name")],
                    by = "cycle_id",
                    all.x = TRUE,
                    all.y = FALSE)
  d_teachers <- merge(d_teachers,
                      pj_tbl_f[, c("teacher_id", "team_id", "cycle_name", "did_practice_journal")],
                      by = c("teacher_id", "team_id", "cycle_name"),
                      all.x = TRUE,
                      all.y = FALSE)


  # Group to team-cycle level
  d_team_cycle <- d_teachers %>%
    group_by(team_id, cycle_name) %>%
    summarise(pct_participating_students = sum(num_observed_students) / sum(expected_num_students),
              pct_participating_teachers = sum(at_least_80_pct_participated %in% TRUE) /
                length(at_least_80_pct_participated),
              pct_practice_journals_completed = sum(did_practice_journal %in% TRUE) /
                length(did_practice_journal))
  # merge in stuff from cycle table
  d_team_cycle_merged <- merge(d_team_cycle,
                               cycle_tbl_summarized[, c("team_id",
                                                        "most_recent_past_cycle",
                                                        "cycle_happening_now")],
                               by = "team_id",
                               all.x = TRUE,
                               all.y = FALSE)

  # Group to team level to report past and present stats
  d_team <- d_team_cycle_merged %>%
    group_by(team_id) %>%
    summarise(pct_participating_teachers_from_most_recent_past_cycle =
                first(pct_participating_teachers[cycle_name %in% most_recent_past_cycle]),
              pct_participating_teachers_from_cycle_happening_now =
                first(pct_participating_teachers[cycle_name %in% cycle_happening_now]),
              avg_student_participation_rate = mean(pct_participating_students, na.rm = TRUE),
              avg_teacher_participation_rate = mean(pct_participating_teachers, na.rm = TRUE),
              pct_practice_journals_from_most_recent_past_cycle =
                first(pct_practice_journals_completed[cycle_name %in% most_recent_past_cycle]),
              pct_practice_journals_from_cycle_happening_now =
                first(pct_practice_journals_completed[cycle_name %in% cycle_happening_now]),
              avg_practice_journal_completion_rate = mean(pct_practice_journals_completed,
                                                          na.rm = TRUE))

  d_team[, setdiff(names(d_team), "team_id")] <- util$apply_columns(
    d_team[, setdiff(names(d_team), "team_id")],
    smart_percent
  )

  # merge participation info into tt
  tt <- merge(tt,
              d_team,
              by = "team_id",
              all.x = TRUE,
              all.y = FALSE)



  ### add info about team-level completion of Copilot tasks

  # RECRUITMENT:

  # % of teachers on the team that filled out the implementation agreement
  # Get agreement completion records for all teachers, mapped to teams,
  # sum to get total agreements per team,
  # compare to number of teachers per team
  imp_agreements <- response_tbl %>%
    filter(module_label %in% "EPImplementationAgreement",
           progress %in% 100) %>%
    group_by(team_id) %>%
    summarise(num_teachers_signed_imp_agreement = length(unique(user_id)))
  tt <- merge(tt,
              imp_agreements,
              by = "team_id",
              all.x = TRUE,
              all.y = FALSE)
  tt$pct_teachers_signed_imp_agreement <- smart_percent(tt$num_teachers_signed_imp_agreement /
                                                          tt$num_team_members)

  # Does team give cert credit?
  cert_credit_info <- team_tbl %>%
    filter(str_detect(task_data, "credit_available")) %>%
    mutate(full_credit_string = str_extract(task_data, "\"credit_available\": \"(yes|no|unsure)\""),
           team_gives_credit_for_ep = str_extract(full_credit_string, "(yes|no|unsure)"),
           full_credit_string = NULL) %>%
    select(team_id, team_gives_credit_for_ep)
  tt <- merge(tt,
              cert_credit_info,
              by = "team_id",
              all.x = TRUE,
              all.y = FALSE)

  context_survey_teams <- response_tbl %>%
    dplyr::filter(module_label %in% 'EPSchoolContextSurvey') %>%
    dplyr::pull(team_id)

  tt$submitted_school_context_survey <- tt$team_id %in% context_survey_teams

  ### clean up tt and save

  tt <- tt %>%
    select(team_name,
           team_id,
           program_label,
           captain_name,
           captain_email,
           # num_team_members,
           num_main_contacts,
           community_name,
           community_contact_info,
           created,
           cycle_dates_set,
           num_classrooms,
           num_participants,
           num_started_participants,
           main_contacts_unstarted,
           cycles_past,
           most_recent_past_cycle,
           pct_participating_teachers_from_most_recent_past_cycle,
           pct_practice_journals_from_most_recent_past_cycle,
           cycle_happening_now,
           pct_participating_teachers_from_cycle_happening_now,
           pct_practice_journals_from_cycle_happening_now,
           cycles_in_future,
           avg_student_participation_rate,
           avg_teacher_participation_rate,
           avg_practice_journal_completion_rate,
           pct_teachers_signed_imp_agreement
           # team_gives_credit_for_ep,
           # submitted_school_context_survey
    ) %>%
    dplyr::arrange(program_label, team_name)

  return(tt)
}
