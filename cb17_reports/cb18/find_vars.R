# The goal of this script is to find variables which are used in the CB survey
# but not in the reports. Based on it, I will make recommendations about which
# variables can be dropped (Sarah suggest to exclude at least 5 variables)
# I will parse all code and all outlines into words. Then I will check wich
# survey variables are not part of this code. Then I will find them in the
# Qualtrics doc, and recommend which one can be removed

folder_path <- "/Users/rumen/Sites/analysis/cb17_reports/cb18/"
f_path <- list(
  f1 =  "cb18_metascript.R",
  f2 =  "cb18_helpers.R",
  f3 =  "cb18_preliminary_analysis.Rmd",
  f4 =  "cb18_report.Rmd",
  f5 = "create_neptune_table.R",
  f6 = "disadv_status_logic.txt",
  f7 = "notes.R",
  f8 = "text_entries.R"
) %>% lapply(function(x) paste0(folder_path,x))

# add crypt paths to the list
f_path$race_key <- util.find_crypt_paths(list(path = "CB17_race_key.csv"))$path
f_path$scale_descriptions <- util.find_crypt_paths(list(path = "CB17_scale_descriptions.csv"))$path
f_path$items <- util.find_crypt_paths(list(path = "CB17_items.csv"))$path
f_path$outline <- util.find_crypt_paths(list(path = "google_doc_copy_report_outline.txt"))$path

#### Read expected sample size by school


file_to_words <- function(file_path) {
  # takes a path to file, and retruns all words
  mystring <- readr::read_file(file_path)
  all_words <- stringr::str_extract_all(mystring, c("[[:alpha:][:digit:]\\._]*")) %>%
    unlist %>% unique
  return(all_words)
}

#file_to_words(f_path[[11]])

all_words_ls <- lapply(f_path, file_to_words) %>% unlist %>% tolower %>% unique

sort(all_words_ls)[2000:length(all_words_ls)]

survey_vars <- s1_raw %>% names %>% tolower() %>% unique

unused_questions <- setdiff(survey_vars, all_words_ls) %>% sort
# all_words_ls[all_words_ls %in% survey_vars]
# the s1_raw comes from cb18_metascript.R. The objects should be in the environment.

pmatch_v <- Vectorize(pmatch)

target <- "mind1"
all_words_ls[grepl(target, all_words_ls)]

survey_vars[grepl(target, survey_vars)]


########
# Here is the final outcome
#
#
# __pdd__t1__demog__distracted__pdd__ How distracted were you as you completed the materials? (e.g., by interruptions, other people, social media, etc.)
#
# __pdd__t1__demog__physical_health__pdd__ 	In general, would you say your physical health is:
#
#   __pdd__t1__demog__english_age__pdd__  How many years old were you when you first learned English?
#
#
# __pdd__t1__dv__belonging_proc2__basic__pdd__ Whether you belong at ${e://Field/school_name} or not is something very basic about you and you can't really change that very much.
#
# __pdd__t1__dv__belonging_proc3__can_increase__pdd__ No matter who you are you can significantly increase your belonging at ${e://Field/school_name}.
#
# __pdd__t1__dv__diff3__confident_handle__pdd__ How confident are you that you will be able to handle any difficulties you face in the transition to ${e://Field/school_name}?
#
# __pdd__t1__dv__do_well__pdd__  How important is it to you to do well at ${e://Field/school_name}?
#
#
# __pdd__t1__dv__g_mind1__certain_amount__pdd__  You have a certain amount of intelligence, and you really can't do much to change it.
#
# __pdd__t1__dv__g_mind2__grow_intell__pdd__ You can grow your basic intelligence a lot in your lifetime.
#
# __pdd__t1__dv__grit1__finish__pdd__ I finish whatever I begin.
#
# __pdd__t1__dv__grit2__interested__pdd__ I stay interested in my goals, even if they take a long time (months or years) to complete.
#
# __pdd__t1__dv__grit3__hard_work__pdd__ I am a hard worker.
