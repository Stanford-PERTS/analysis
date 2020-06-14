# In this script we will k-anonymize the cb17 script
# the general flow could be found here: https://www.lucidchart.com/documents/edit/621172f2-5385-4c36-88ed-c7ef69e33017
# notes could be found here: https://docs.google.com/document/d/1qWNd1wtchikQs8bDolr8UIdAWMasvoN0XnakGPdlbcY/edit#heading=h.38rfg5srt49k

# input files:
## input_data.csv
## recode_key.csv - controls which variables will be recoded, and the new names
##                    of each variable. Recoded variables will be automatically included
## var_key.csv - controls which variables will be included

# output files:
## match_key.csv
## k_anonymized_cb17.csv
## basic_log.csv
## detailed_log.csv

# define constants
MIN_CELL <- 2 # controls what is the minimum cell size for masking a category

# LOAD LIBRARIES

# source gymnast functions
tryCatch({
  source("~/Sites/gymnast/R/util.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_qualtrics_cleaning.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_graphing.R", chdir = TRUE)
  source("~/Sites/gymnast/R/util_scale_computation.R", chdir = TRUE)
}, error = function(e){
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_qualtrics_cleaning.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_graphing.R")
  source("https://raw.githubusercontent.com/PERTS/gymnast/master/R/scale_computation.R")
})

# DEFINE PATHS AND CONSTANTS

REPO_PARENT <- "~/Sites/"
RMD_BASE_PATH <- REPO_PARENT %+% "perts_analyses/cb17_reports/k_anonymization"
DROPBOX_DIR <- "~/Dropbox (PERTS)/"
MIN_CELL <- 5

source(REPO_PARENT %+% "perts_analyses/cb17_reports/cb17_helpers.R")
source(REPO_PARENT %+% "perts_analyses/cb17_reports/k_anonymization/helper_functions.R")

library(rmarkdown)
library(tidyr)
library(tools)
library(jsonlite)

# set stringsAsFactors to FALSE, and add any other global R settings
options(stringsAsFactors = FALSE)
options(scipen=999) #avoids scientific notations
###########################

# READ DATA
# the data input comes from the report generation script.
input_file_name <- "input_data_k_anonym.csv"
input_path <- util.find_crypt_paths(list(f1 = input_file_name)) %>%  unlist
input_df <- read.csv(input_path)

input_df$report_gender[input_df$report_gender %in% "Other"] <- NA
med_age <- input_df$t1__demog__english_age %>% median(., na.rm = T) #not sure what variable this is

input_df$age_median <- ifelse(input_df$t1__demog__english_age >= med_age, "High", "Low")

demogr_vars <- c("disadv_race", "report_gender", "disadv_educ", "age_median")

melted_df <- data.frame()

for (demogr_var in demogr_vars) {
  buff_df <- input_df
  buff_df$variable <- buff_df[,demogr_var]
  current_df <- buff_df %>%
    group_by(school_name, variable) %>%
    summarise(count_n = n())
  current_df <- current_df[!current_df$variable %in% c("<NA>", "NA", NA),]
  current_df$variable <- paste0(demogr_var, "_", current_df$variable)
  melted_df <- bind_rows(melted_df, current_df)
}
casted_df <- melted_df %>% dcast(school_name ~ variable)
write.csv(casted_df, RMD_BASE_PATH %+% '/basic_log.csv', row.names = F)

# cycle through colleges and mask variables in order of importance

anonymized_df <- input_df

# we should do this recursively


#in_df <- input_df[input_df$school_name == "UofA", ]
mask_demographics <- function(in_df, demogr_vars) {
  cross_tabbed_df <- in_df %>%
    group_by_(.dots = demogr_vars) %>%
    summarise(cell_count = n()) %>%
    as.data.frame()
  # remove rows containing NAs
  #print(demogr_vars)
  if (length(demogr_vars) > 1){
    na_rows <- cross_tabbed_df[,demogr_vars] %>%
      apply(., 1, function(x) any(is.na(x)))
  } else {
    na_rows <- is.na(cross_tabbed_df[,demogr_vars])
  }
  cross_tabbed_df <- cross_tabbed_df[!na_rows,]
  bad_range <- any(cross_tabbed_df$cell_count %in% c(1:4))
  if (!bad_range) {
    print(cross_tabbed_df)
    return(demogr_vars)
  } else {
    new_demogr_vars <- demogr_vars[-length(demogr_vars)]
    if (length(new_demogr_vars) > 0) {
      mask_demographics(in_df, new_demogr_vars)
    } else {
      return(NULL)
    }
  }
}

#x <- mask_demographics(in_df, demogr_vars)


demogr_to_keep_list <- list()
demogr_to_remove_list <- list()

anoymized_df <- input_df
anoymized_df$age_median <- sample(c("Low", "High"), nrow(input_df), replace = T)
for (school_name in unique(anoymized_df$school_name)) {
  in_df <- anoymized_df[anoymized_df$school_name == school_name, ]
  demogr_to_keep <- mask_demographics(in_df, demogr_vars)
  demogr_to_remove <- setdiff(demogr_vars, demogr_to_keep)
  print(school_name)
  print(nrow(in_df))
  print(demogr_to_keep)
  print("")
  demogr_to_keep_list[[school_name]] <- demogr_to_keep
  demogr_to_remove_list[[school_name]] <- demogr_to_remove
}



# set demogr_to_remove to NA for each school
for (school_name in unique(anoymized_df$school_name)) {
  anoymized_df[
    anoymized_df$school_name == school_name,
    demogr_to_remove_list[[school_name]]
  ] <- NA

}

table(anoymized_df$school_name, anoymized_df$age_median)

# @todo
# needs debugging. small colleges seems to avoid anonymization (
# check arnrow u)


# try the anonymization libraries
# a paper introducing sdcMicro: https://www.dropbox.com/sh/bir4ygdamcxmymm/AACyeYED3lgXPQEemnGAvcb6a?dl=0&preview=sdc_guidelines.pdf#pageContainer24
#install.packages("sdcMicro")
library(sdcMicro)
anoymized_df$age_discr <- sample(c(20:80), nrow(input_df), replace = T)

# CONTIN. SCALES
x <- globalRecode(anoymized_df$age_discr, c(1,9,19,29,39,49,59,69,100), labels=1:8)

## for objects of class sdcMicro:

sdc <- createSdcObj(anoymized_df,
                    keyVars=c('report_gender','report_race', "age_discr"),
                    numVars=c("t1__dv__fall1__fit_in",
                              "t1__dv__soph1__fit_in"
                    ))
sdc <- globalRecode(sdc, column="age_discr", breaks=3)
table(get.sdcMicroObj(sdc, type="manipKeyVars")$age_discr)

# CATEGORICAL DATA
## for objects of class sdcMicro:
sdc <- groupAndRename(sdc, var="report_gender", before=c("Male","Female"), after=c("Genderless"))
#
table(get.sdcMicroObj(sdc, type="manipKeyVars")$report_gender)


# hash ids add id look up table, where we can connect new and old ids

##### BUILDING A HIEARARCHICAL CATEGORY MERGE

# read specs from google worksheet
# specs are defined here: https://docs.google.com/spreadsheets/d/1m1V7aLKT-nHTf0nuPFh2zpcobw66rZty-HUZPRhmIcU/edit#gid=0
# a graphic example here:https://www.lucidchart.com/documents/edit/f433464a-8260-4988-8a3b-b85e01f3a189#
library("googlesheets")
library("RCurl")

tree_specs_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTq1gMoN1pMvCDYOhsn7ieXIevw-_n32r_z3DvjSmddiH6dquzyihQ-txIXcDpO8MSG5ikalvRVECbk/pub?gid=0&single=true&output=csv"
tree_specs_df <- read.csv(tree_specs_url, na.strings = c("", " "), stringsAsFactors = F)

# clear white spaces
tree_specs_df <- tree_specs_df %>% lapply(., function(x) util.trim(x)) %>%
  as.data.frame(., stringsAsFactors = F)
tree_specs_df$global_order <- tree_specs_df$global_order  %>% as.numeric()






# Create all levels of resolution for a category, moving from n to 1
# where n is the number of original category levels and contains maximum information

# simulate some data
categories_in_data <- c("race", "gender")
simulated_levesl_vect <- c("A","B", "C", "D", "E", "F", "G", "NA")
n_row_ <- 100
simulated_df <-
  data.frame (
    index_row  = c(1:n_row_),
    race = sample(simulated_levesl_vect,n_row_, replace = T),
    gender = sample(c("A", "B", "C", "NA"),n_row_, replace = T),
    stringsAsFactors=FALSE
  )

# create local order variable, within each category
tree_specs_df <-  tree_specs_df %>% create_local_order()

# add indexed category name for different levels of resolution
tree_specs_df <- tree_specs_df  %>%
  group_by(category) %>%
  mutate (rev_loc_order = (max(local_order, na.rm = T) + 1) - local_order,
          category_indexed = paste0(category, "_", rev_loc_order),
          category_indexed_to_mask =paste0(category, "_", (rev_loc_order + 1)) ) %>%
  as.data.frame() %>%
  select(-rev_loc_order)
tree_specs_df$category_indexed[is.na(tree_specs_df$local_order)] <- NA
tree_specs_df$category_indexed_to_mask[is.na(tree_specs_df$local_order)] <- NA
# category_indexed will contain the names of the different resolution levels for each category
# category_indexed_to_mask is the category which will be masked after the merge described in
# category_indexed. For example category_indexed = race_3 means that there are 3 levels in this category
# and that race_4 will be masked if we want to use race_3





# CHECKS
# 1. category levels in data should be present in specs
cat_levels_in_specs <- tree_specs_df$category_level_label[is.na(tree_specs_df$global_order)]
cat_levels_in_data <- c()
for (cat in categories_in_data) {
  cat_levels_in_data <- c(cat_levels_in_data, simulated_df[,cat]) %>% unique
}


if (!all(cat_levels_in_data %in% cat_levels_in_specs)) {
  stop("The category levels defined in settings are not the same as the ones found in the data!")
}

# 2. category merges should equal to the number of levels minus the number of categories
# For example, a category with initial 8 levels will need 7 binary merges to be reduced to 1 level
cat_merges_in_specs <- tree_specs_df$category_level_code[!is.na(tree_specs_df$global_order)]
n_categories <- unique(tree_specs_df$category) %>% length
if (length(cat_merges_in_specs) !=  (length(cat_levels_in_specs) - n_categories)) {
  stop("The category levels defined in settings are not the same as the ones found in the data!")
}

# 3. the order values equal to the number of levels minus 1
vec_x <- 1:length(cat_merges_in_specs)
vec_y <- min(tree_specs_df$global_order, na.rm = T):max(tree_specs_df$global_order, na.rm = T)
if (!all(vec_x == vec_y)) {
  stop("The the order values in the settings are wrong.")
}


# 4. all merged category levels should be defined as parents
if (length(setdiff(cat_merges_in_specs, unique(tree_specs_df$category_level_parent))) > 0) {
  stop("Discrepancy betweeen merged category codes and parent description codes")
}


# MERGING






# add the highest resolution categories, which are actually unchanged

simulated_df <-  created_indexed_categories(simulated_df, tree_specs_df, categories_in_data)




### MASKING

# keep a dataframe which shows which category needs to be masked

# compute current resoultion for each of the key variables (demographics)

indexed_categories_to_mask <- compute_excluded_categories(simulated_df, tree_specs_df, categories_in_data, MIN_CELL)

# sort alphabetically key variable columns
ordered_cols <- c(categories_in_data, indexed_cat_name)
simulated_df <-  reorder_cols(simulated_df, ordered_cols)
View(simulated_df)

# mask categories
masked_df <- simulated_df
cats_to_mask <- c(categories_in_data, indexed_categories_to_mask)
masked_df[,cats_to_mask] <- NA
View(masked_df)
# debug
# run checks

# @todo: allow for categories to have fewer level than expected (but not more)
# for example, race_8 could have 4 levels, but not 9.
# test for all colleges!!!
