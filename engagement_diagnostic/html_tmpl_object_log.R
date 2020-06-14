# this file is runnable only if called from the engagement_diagnostic.Rmd
# for each report, it saves a text file with description of all objects that
# are passed to the html template. It could be very helpful for debugging. For
# example, you can compare the objects for reports which work to the objects for
# reports which fail to generate.


# list the objects passed to the html
list_passed_objects <- list()
list_passed_objects['page_title']  = page_title
list_passed_objects['report_date'] =  paste0(months(date(REPORT_DATE))," ",day(date(REPORT_DATE)), ", ",year(date(REPORT_DATE)))
list_passed_objects['team_name'] =  team_title
list_passed_objects['team_id'] =  team_id_triton
list_passed_objects['classroom_name'] =  reporting_unit_title
list_passed_objects['classroom_id'] =  reporting_unit_id_triton
list_passed_objects['classroom_id_url'] =  gsub("Classroom_", "", reporting_unit_id_triton)
list_passed_objects['zero_length_table'] =  zero_length_table
list_passed_objects['max_weeks_missing'] =  max_weeks_missing
list_passed_objects['participation_table_html'] =  gsub("\n", "", util.html_table_data_frame(participation_table_df, print.results = FALSE))
list_passed_objects['active_fg'] =  'feedback_for_growth' %in% present_drivers
list_passed_objects['timeline_active_fg'] =
  ifelse('feedback_for_growth' %in% timeline_charts,!is.na(timeline_charts[['feedback_for_growth']]), NA)
list_passed_objects['name_fg'] =  driver_desc[['feedback_for_growth']]$name
list_passed_objects['name_sent_case_fg'] =  driver_desc[['feedback_for_growth']]$name_sent_case
list_passed_objects['name_title_case_fg'] =  driver_desc[['feedback_for_growth']]$name_title_case
list_passed_objects['description_fg'] =  driver_desc[['feedback_for_growth']]$description
list_passed_objects['graphic_url_fg'] =  image_paths[['feedback_for_growth']]
list_passed_objects['timeline_chart_fg'] =  timeline_charts[['feedback_for_growth']]
list_passed_objects['bar_chart_fg'] =  cross_section_charts[['feedback_for_growth']]
list_passed_objects['active_mw'] =  'meaningful_work' %in% present_drivers
list_passed_objects['timeline_active_mw'] =
  ifelse('meaningful_work' %in% timeline_charts,!is.na(timeline_charts[['meaningful_work']]), NA)
list_passed_objects['name_mw'] =  driver_desc[['meaningful_work']]$name
list_passed_objects['name_sent_case_mw'] =  driver_desc[['meaningful_work']]$name_sent_case
list_passed_objects['name_title_case_mw'] =  driver_desc[['meaningful_work']]$name_title_case
list_passed_objects['description_mw'] =  driver_desc[['meaningful_work']]$description
list_passed_objects['graphic_url_mw'] =  image_paths[['meaningful_work']]
list_passed_objects['timeline_chart_mw'] =  timeline_charts[['meaningful_work']]
list_passed_objects['bar_chart_mw'] =  cross_section_charts[['meaningful_work']]
list_passed_objects['active_tc'] =  'teacher_caring' %in% present_drivers
list_passed_objects['timeline_active_tc']  =
  ifelse('teacher_caring' %in% timeline_charts,!is.na(timeline_charts[['teacher_caring']]), NA)
list_passed_objects['name_tc']  =  driver_desc[['teacher_caring']]$name
list_passed_objects['name_sent_case_tc']  =  driver_desc[['teacher_caring']]$name_sent_case
list_passed_objects['name_title_case_tc']  =  driver_desc[['teacher_caring']]$name_title_case
list_passed_objects['description_tc']  =  driver_desc[['teacher_caring']]$description
list_passed_objects['graphic_url_tc']  =  image_paths[['teacher_caring']]
list_passed_objects['timeline_chart_tc']  =  timeline_charts[['teacher_caring']]
list_passed_objects['bar_chart_tc']  =  cross_section_charts[['teacher_caring']]

# save into a file
file_path <- RMD_BASE_PATH %+% "/objects_passed_to_html_template_" %+% reporting_unit_id_triton %+% ".txt"
if(TEAM_ONLY) {
  file_path <- RMD_BASE_PATH %+% "/objects_passed_to_html_template_" %+% team_id_triton %+% ".txt"
}

if (file.exists(file_path)) {file.remove(file_path)}
for (field in names(list_passed_objects)){
  line_ = paste0(field, ": ",list_passed_objects[[field]], "\n")
  cat(line_,file=file_path,append=TRUE,sep="\n")
}