# 1 - 2019 CL EP data cleaning.R
# Daniel Greene
# 7/24/2019

# The purpose of this script is to review the CL data and do any cleaning before analysis.


############### USER-DEFINED INPUTS #####################################

grades_crypt_path <- "SY 2018-19 Q4/Learning_S19.csv"
qualtrics_crypt_path <- "SY 2018-19 Q4/CL 2019 Qualtrics responses.csv"
courses_crypt_path <- "SY 2018-19 Q4/courses.csv"
general_data_folder_crypt_path <- "SY 2018-19 Q4"
script_folder_path <- "~/Sites/analysis/Engagement Project 2018-19/2018-2019 CL EP analysis"

############### LOAD LIBRARIES AND PATHS #####################################

github_base_path <- "https://raw.githubusercontent.com/PERTS/gymnast/master/"

tryCatch({
  source("~/Sites/gymnast/R/util.R", chdir = TRUE)
  gymnast_install()
  library(tidyverse)
  source("~/Sites/gymnast/R/util_data_summaries.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_qualtrics_cleaning.R", chdir = TRUE)
}, error = function(e){
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R")
  gymnast_install()
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_data_summaries.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_qualtrics_cleaning.R")
})

library(tidyverse)
library(lubridate)

# helper function for converting 100-point format to 4-point GPA format
convert_gpa <- function(vec) {
  # check that vec is numeric 0-100
  if(!is.numeric(vec) | min(vec) < 0 | max(vec) > 100) {
    stop("ERROR: 100-point GPA is not numeric or in bounds.")
  }
  # round vec to whole numbers
  vec <- round(vec)
  # convert vec to 4-point GPA
  vec[vec < 65] <- 0.0
  vec[vec >= 65 & vec <= 66] <- 1.0
  vec[vec >= 67 & vec <= 69] <- 1.3
  vec[vec >= 70 & vec <= 72] <- 1.7
  vec[vec >= 73 & vec <= 76] <- 2.0
  vec[vec >= 77 & vec <= 79] <- 2.3
  vec[vec >= 80 & vec <= 82] <- 2.7
  vec[vec >= 83 & vec <= 86] <- 3.0
  vec[vec >= 87 & vec <= 89] <- 3.3
  vec[vec >= 90 & vec <= 92] <- 3.7
  vec[vec >= 93] <- 4.0
  return(vec)
}


############### LOAD DATA #####################################

# g for grades
g <- util.find_crypt_paths(list(a = grades_crypt_path))$a %>%
  read.csv()


# q for qualtrics
q <- util.find_crypt_paths(list(a = qualtrics_crypt_path))$a %>%
  read.csv() %>%
  qc.clean_qualtrics()

# c for courses
c <- util.find_crypt_paths(list(a = courses_crypt_path))$a %>%
  read.csv()

############### CLEAN QUALTRICS DATA #####################################

# rename vars
q <- rename(q,
            tc1_2 = Q3,
            tc2_2 = Q5,
            tc4_2 = Q7,
            fg2_2 = Q11,
            fg3_2 = Q13,
            mw1_2 = Q15,
            mw2_2 = Q17,
            mw3_2 = Q19,
            student_id = AnonymizedStudentID)

# create comb_lc_score
lc_qs <- c("tc1_2", "tc2_2", "tc4_2", "fg2_2", "fg3_2", "mw1_2", "mw2_2", "mw3_2")
q$comb_lc_score <- rowMeans(q[, lc_qs], na.rm = TRUE)

# create individual LC scores
q$tc <- rowMeans(q[, c("tc1_2", "tc2_2", "tc4_2")], na.rm = TRUE)
q$fg <- rowMeans(q[, c("fg2_2", "fg3_2")], na.rm = TRUE)
q$mw <- rowMeans(q[, c("mw1_2", "mw2_2", "mw3_2")], na.rm = TRUE)


# drop unneeded columns
cols_to_keep <- c("StartDate", "student_id", lc_qs, "comb_lc_score", "tc", "fg", "mw")
q <- q[, cols_to_keep]

# drop rows with no student_id
q <- filter(q, !student_id %in% "")

# sanity-check - any duplicate student IDs? Yes! 5. Cut the dups.
sum(duplicated(q$student_id))
q <- q[!duplicated(q$student_id), ]

############### CLEAN COURSE DATA #####################################

# We just care about whether a course is math or not... make a simple version
c_simple <- c %>%
  mutate(is_math = !is.na(math)) %>%
  select(cnum, is_math)

# sanity check - any NA course numbers?
any(is.na(c_simple$cnum)) # no

# sanity check - any duplicate courses?
sum(duplicated(c_simple$cnum)) # Yes! Many!

# Do these dups ever have differing math-coding values?
c_simple_agg <- c_simple %>%
  group_by(cnum) %>%
  summarise(n_entries = length(cnum),
            n_unique_math_vals = length(unique(is_math)))
table(c_simple_agg$n_entries) # 458 cnums w 2 entries, 21 w 3
table(c_simple_agg$n_unique_math_vals) # but they all have consistent math codings, so we are good
# In that case, cut duplicates for simplicity
c_simple <- c_simple[!duplicated(c_simple), ]

############### CLEAN GRADES DATA #####################################

# rename vars
g <- rename(g,
            student_id = anonymizedstudentid,
            gender = female,
            school = school_name_mp4)

# recode gender
g$gender <- util.recode(g$gender,
                        c(0, 1),
                        c("Male or nonbinary", "Female"))

##### get Q4 math GPA and non-math GPA:

# make long grade data (each row is a student x course ordinal, with course number and gpa)
# for simpler melting, do course numbers and gpa separate, then merge
gl_nums <- g %>%
  select(student_id,
         cnum_mp4_1, cnum_mp4_2, cnum_mp4_3,cnum_mp4_4,
         cnum_mp4_5, cnum_mp4_6, cnum_mp4_7,cnum_mp4_8) %>%
  melt(id.vars = c("student_id")) %>%
  mutate(course_ordinal = stringr::str_sub(variable, start = -1)) %>%
  rename(cnum = value) %>%
  select(-variable)
gl_gpas <- g %>%
  select(student_id,
         cgpa_mp4_1, cgpa_mp4_2, cgpa_mp4_3, cgpa_mp4_4,
         cgpa_mp4_5, cgpa_mp4_6, cgpa_mp4_7, cgpa_mp4_8) %>%
  melt(id.vars = c("student_id")) %>%
  mutate(course_ordinal = stringr::str_sub(variable, start = -1)) %>%
  rename(course_gpa = value) %>%
  select(-variable)
gl <- merge(gl_nums,
            gl_gpas,
            by = c("student_id", "course_ordinal"),
            all = TRUE)

# use course table to tag rows as math or non-math,
# and cut rows with no cnum, gpa, or is_math value
gl <- merge(gl,
            c_simple,
            by = "cnum",
            all.x = TRUE,
            all.y = FALSE) %>%
  filter(!is.na(cnum),
         !is.na(course_gpa),
         !is.na(is_math))

# cast back to student level to get Q4 math and Q4 non-math avg,
# and filter 24 students who don't have exactly one math course
gs <- gl %>%
  group_by(student_id, is_math) %>%
  summarise(avg_gpa = mean(course_gpa, na.rm = TRUE),
            num_courses_for_avg_gpa = length(course_gpa)) %>%
  filter(!is_math | (num_courses_for_avg_gpa %in% 1)) %>%
  dcast(student_id ~ is_math, value.var = "avg_gpa" ) %>%
  rename(q4_non_math_gpa = "FALSE",
         q4_math_gpa = "TRUE") %>%
  filter(!is.na(q4_math_gpa))

# also merge in course IDs for Q4 math courses
g_math_course_ids <- gl %>%
  filter(is_math) %>%
  group_by(student_id) %>%
  summarise(q4_math_course_id = first(cnum),
            num_math_courses = n()) %>%
  filter(num_math_courses %in% 1) %>%
  select(-num_math_courses)

gs <- merge(gs,
            g_math_course_ids,
            by = "student_id",
            all.x = TRUE,
            all.y = FALSE)

# convert from 100-point format to 4-point GPA format
gs$q4_non_math_gpa <- convert_gpa(gs$q4_non_math_gpa)
gs$q4_math_grade <- convert_gpa(gs$q4_math_gpa)

# merge back into g, only keeping complete cases from gs
g <- merge(g,
           gs,
           by = "student_id",
           all.x = FALSE,
           all.y = TRUE)

# Also get total unexcused absences and total suspension days
g$q4_unex_absences <- g$tot_unexcused_mp4
g$q4_suspensions <- g$tot_missed_susp_mp4

# Also calculate proportion of q4 days that are absent (assuming 47 days in a quarter based on data inspection)
# use Gates Foundation convention of 96% attendance as good
g$prop_q4_present <- round(1 - (g$tot_absent_mp4 / 47), 2)
g$q4_present_high <- g$prop_q4_present >= .96

# drop unneeded columns
cols_to_keep <- c("student_id", "gender", "white", "black", "asian", "hispanic",
                  "amindian", "pacisl",  "multirace", "frpl", "ell", "iep", "ese", "school", "q4_math_grade", "q4_non_math_gpa",
                  "q4_math_course_id", "q4_unex_absences", "q4_suspensions", "prop_q4_present", "q4_present_high")
g_simple <- g[, cols_to_keep]

# sanity-check: no dup student-ids
any(duplicated(g_simple$student_id)) # good

############### MERGE AND SANITY-CHECK #####################################

# Merge q with g_simple, only keeping complete cases
d <- merge(q,
           g_simple,
           by = "student_id",
           all.x = FALSE,
           all.y = FALSE)
# 1369 complete cases... about 40-100 didn't match up

# Sanity-check proportions missing & ranges for each variable - excellent.
ds.summarize_by_column(d)

# Save the result
generic_save_path <- util.find_crypt_paths(list(a = general_data_folder_crypt_path))$a
write.csv(d, generic_save_path %+% "/2018-2019 CL cleaned data for PERTS analysis.csv")


# model: q4_math_grade ~ comb_lc_score

# propose to not use:
## repeating a grade is redundant with GPA
## home language is redundant with ELL

