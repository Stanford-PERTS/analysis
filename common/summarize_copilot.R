modules::import(
  "dplyr",
  `%>%`,
  "left_join",
  "rename",
  "select"
)

merge_participants <- function(triton.participant, neptune.participant) {
  # Args are rows directly from db tables, with prefixed column names.
  # names(triton.participant) %in% c('participant.uid', 'participant.in_target_group', ...)
  # names(neptune.participant) %in% c('participant.uid', 'participant.name', ...)
  #
  # Returns df with names 'participant_id' and 'in_target_group' where the
  # latter is %in% c(0, 1, NA).

  if (!all(triton.participant$in_target_group %in% c(0, 1))) {
    stop("Triton participant target group values not all in (0, 1).")
  }

  # Rename some triton columns for clarity. Things get pretty confusing since
  # both tables are named "participant".
  triton_for_join <- triton.participant %>%
    select(
      triton.stripped_student_id = participant.stripped_student_id,
      triton.team_id = participant.team_id,
      in_target_group = participant.in_target_group
    )

  neptune.participant %>%
    left_join(
      triton_for_join,
      by = c(
        participant.organization_id = "triton.team_id",
        participant.name = "triton.stripped_student_id"
      )
    ) %>%
    select(participant.uid, in_target_group) %>%
    rename(participant_id = participant.uid)
}
